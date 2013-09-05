open Apitype

module Response = struct

  open Simplexmlparser

  exception Wrong of string
  exception ApiError of (int * string)

  type 'a result = {
    version : string ;
    cachedUntil : Time.t ;
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

  let parse_rowset = function
    | Element ("rowset", [("name", name); ("key", key); ("columns", col)], rows)
      -> List.map parse_row rows
    | _ -> raise (Wrong "rowset")

  let parse_rowset2 = function
    | Element ("rowset", [("name", name); ("key", key); ("columns", col)], rows)
      -> List.map parse_rowset rows
    | _ -> raise (Wrong "rowset")

  let parse_tag = function
    | Element (k, [], [PCData v]) -> (k, v)
    | _ -> raise (Wrong "tag")

  let parse_tags = List.map parse_tag

  let extract = function
    | [Element ("eveapi", [("version", version)],
         [ Element ("currentTime", [], [PCData t1 ]) ;
           Element ("result", [], data) ;
           Element ("cachedUntil", [], [PCData t2 ])
         ])]
      -> Result { version ; cachedUntil = Time.extract_next_date t1 t2 ; data }
    | [Element ("eveapi", _ , [ _  ; Element ("error", ["code", code], [PCData descr]) ; _ ])]
      -> Error (int_of_string code, descr)
    | _ -> raise (Wrong "extract")

  let extract_tags r = map parse_tags (extract r)

  let extract_rowset r  =
    let f = function
      | [ l ] -> parse_rowset l
      | _ -> raise (Wrong "rowset")
    in map f (extract r)

  let extract_rowsets r = map (List.map parse_rowset) (extract r)

  let extract_rowset2 r  =
    let f = function
      | [ l ] -> parse_rowset2 l
      | _ -> raise (Wrong "rowset2")
    in map f (extract r)

end

module OS = Ocsigen_stream

(** Fetching stuff *)

let http_fetch ?(https=false) prefix endpoint args =
  let open Ocsigen_http_client in
  lwt response =
    post_urlencoded ~https ~host:prefix ~uri:endpoint ~content:args () in
  let stream = response.Ocsigen_http_frame.frame_content in
  lwt data = match stream with
    | None -> Lwt.return ""
    | Some s ->
        let body = OS.string_of_stream 100000000 (OS.get s) in
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

let rec periodic_update call update =
  lwt r = call () in
  let x = update r in
  match_lwt x with
    | `Ok -> begin
        let t = Time.get_second_until r.Response.cachedUntil in
        lwt () = Lwt_unix.sleep (float t) in
        periodic_update call update
      end
    | _ -> x
