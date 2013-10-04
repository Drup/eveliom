open Apitype

module Response = struct

  open Simplexmlparser

  exception Wrong of string
  exception ApiError of (int * string)

  type 'a result = {
    version : string ;
    cachedUntil : Apitime.t ;
    data : 'a
  }

  type 'a t =
    | Result of 'a result
    | Error of (int * string)

  type 'a extract = Simplexmlparser.xml list -> 'a t

  let cast = function
    | Result x -> x
    | Error a -> raise (ApiError a)

  let mapr f { version ; cachedUntil ; data } =
      { version ; cachedUntil ; data = f data }

  let map_t f = function
    | Result a -> Result (f a)
    | Error _ as x -> x

  let map f = map_t (mapr f)

  let with_data r d = mapr (fun _ -> d) r

  let parse_row = function
    | Element ("row", l , []) -> l
    | _ -> raise (Wrong "row")

  let parse_row' f = function
    | Element ("row", l , m) -> l, f m
    | _ -> raise (Wrong "row")

  let parse_rowset f = function
    | Element ("rowset", [("name", name); ("key", key); ("columns", col)], rows)
      -> List.map f rows
    | _ -> raise (Wrong "rowset")

  let parse_tag = function
    | Element (k, [], [PCData v]) -> (k, v)
    | _ -> raise (Wrong "tag")

    let (@>) f g  = function
      | [ l ] -> f g l
      | _ -> raise (Wrong "rowset")
    and (@>>) f g l = List.map (f g) l

  let parse_tags = List.map parse_tag

  let extract = function
    | [Element ("eveapi", [("version", version)],
         [ Element ("currentTime", [], [PCData t1 ]) ;
           Element ("result", [], data) ;
           Element ("cachedUntil", [], [PCData t2 ])
         ])]
      -> Result { version ; cachedUntil = Apitime.extract_next_date t1 t2 ; data }
    | [Element ("eveapi", _ , l)] ->
        let bla = function Element ("error",_,_) -> true | _ -> false in
        let f = function
          | Element ("error", ["code", code], [PCData descr]) ->
              Error (int_of_string code, descr)
          | _ -> raise (Wrong "error detection")
        in List.find bla l |> f
    | [PCData "Bad Request"] -> raise (Wrong "Bad request")
    | [] -> raise (Wrong "empty answer")
    | _ -> raise (Wrong "extract")

  let extract_tags r =
    map parse_tags (extract r)

  let extract_rowset r =
    map (parse_rowset @> parse_row) (extract r)

  let extract_rowsets r =
    map (parse_rowset @>> parse_row) (extract r)

  let extract_rowset2 r =
    map (parse_rowset @>> parse_rowset @@ parse_row) (extract r)

end

module OS = Ocsigen_stream

(** Fetching stuff *)

let http_fetch ?(https=false) prefix endpoint args =
  let open Ocsigen_http_client in
  lwt response = match args with
    | [] -> get ~https ~host:prefix ~uri:endpoint ()
    | _ ->  post_urlencoded ~https ~host:prefix ~uri:endpoint ~content:args ()
  in
  let stream = response.Ocsigen_http_frame.frame_content in
  lwt data = match stream with
    | None -> Lwt.return ""
    | Some s ->
        let body = OS.string_of_stream (int_of_float (2. ** 30.)) (OS.get s) in
        lwt () = Ocsigen_stream.finalize s `Success in
        body
  in
  let xmldata = Simplexmlparser.xmlparser_string data in
  Lwt.return xmldata

(** Common part of all APIs *)

type cache = Long | Short | MShort

let tq = "api.eveonline.com"

let test = "api.testeveonline.com"

(** Typing madness ! *)

type apikey = < keyId : int ; vCode : string >
let apikey ~keyId ~vCode : apikey =
  object
    method keyId = keyId
    method vCode = vCode
  end

type charkey = < keyId : int ; vCode : string ; characterID : int >
let charkey ~keyId ~vCode ~charId : charkey =
  object
    method keyId = keyId
    method vCode = vCode
    method characterID = charId
  end

let add_char ~(key:apikey) ~charId =
  charkey key#keyId key#vCode charId

type enc_param = (string * string) list

type ('extract, 'auth , 'param, 'out) internal_api = {
  cache : cache ;
  result : 'extract Response.extract ;
  auth : 'auth -> enc_param ;
  param : 'param -> enc_param ;
  decode : 'extract -> 'out ;
  uri : string ;
}

type (_,_,_) api =
  Api : (_ , 'auth , 'param, 'out) internal_api -> ('auth , 'param, 'out) api

let apply_api ?(https=false) prefix (Api endpoint) =
  let cont args =
    lwt response = http_fetch ~https prefix endpoint.uri args in
    let data = Response.map endpoint.decode (endpoint.result response) in
    Lwt.return data
  in
  let f auth param =
    cont (endpoint.auth auth @ endpoint.param param)
  in f

let rec periodic_update ?delay ~call ~error ~update () =
  let open Response in
  let get_delay r =
    Ocsigen_lib.Option.get
      (fun () -> Apitime.time_until r.cachedUntil)
      delay
  in
  lwt r = call () in
  lwt next_wait = match r with
    | Result t ->
        lwt next = update t in
        let d = match next with `KeepGoing -> `Delay (get_delay t) | x -> x in
        Lwt.return d
    | Error s ->
        lwt next = error s in
        let d = match next, delay with `KeepGoing, Some d -> `Delay d | x,_ -> x in
        Lwt.return d
  in
  match next_wait with
    | `Delay d ->
        let t = Apitime.to_seconds d in
        lwt () = Lwt_unix.sleep (float t) in
        periodic_update ?delay ~call ~error ~update ()
    | `Retry ->
        lwt () = Lwt_unix.yield () in
        periodic_update ?delay ~call ~error ~update ()
    | _ as x -> Lwt.return x

let handle_error ?log (x,s) =
  Ocsigen_lib.Option.iter ((|>) s) log ;
  match x with
    | i when i < 200 -> `Stop
    | i when i < 300 -> `Stop
    | 521 -> `Stop
    | i when i < 600 -> `Delay (Apitime.Period.lmake ~minute:30 ())
    | 901 | 902 | 999 | 1001 -> `Delay (Apitime.Period.lmake ~hour:1 ())
    | 903 -> `Delay (Apitime.Period.lmake ~hour:1 ())
    | 904 -> `Delay (Apitime.Period.lmake ~day:1 ())
    | _ -> `Stop
