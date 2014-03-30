open Apitype
open Ocsigen_lib

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
    Api : (_ , 'auth , 'param, 'out) internal_api ->
    ('auth , 'param, 'out) api

let apply_api ?(https=false) prefix (Api endpoint) =
  let cont args =
    lwt response = http_fetch ~https prefix endpoint.uri args in
    let data = Response.map endpoint.decode (endpoint.result response) in
    Lwt.return data
  in
  let f auth param =
    cont (endpoint.auth auth @ endpoint.param param)
  in f

(** Repeated calling *)

let rec periodic_update ?(log=ignore) ?delay ~call ~error ~update () =
  let open Response in
  let get_delay r =
    Option.get
      (fun () -> Apitime.time_until r.cachedUntil)
      delay
  in
  lwt result = call () in
  lwt next_wait = match result with
    | Result t -> begin
        lwt next = update t in
        match next with
            `KeepGoing -> Lwt.return (`Delay (get_delay t))
          | x -> Lwt.return x
      end
    | Error s ->
        lwt next = error s in
        begin
          match next, delay with
              `KeepGoing, Some d -> Lwt.return (`Delay d)
            | x,_ -> Lwt.return x
        end
  in
  match next_wait with
    | `Delay d ->
        log d ;
        let t = Apitime.to_seconds d in
        assert (t >= 0) ;
        lwt () = Lwt_unix.sleep (float t) in
        periodic_update ~log ?delay ~call ~error ~update ()
    | `Retry ->
        log (Apitime.Period.empty) ;
        lwt () = Lwt_unix.yield () in
        periodic_update ~log ?delay ~call ~error ~update ()
    | x -> log (Apitime.Period.second 5) ; Lwt.return x

let handle_error ?log (x,s) =
  Option.iter ((|>) s) log ;
  match x with
    | i when i < 200 -> `Stop
    | i when i < 300 -> `Stop
    | 521 -> `Stop
    | i when i < 600 ->
        `Delay (Apitime.Period.lmake ~minute:30 ())
    | 901 | 902 | 999 | 1001 ->
        `Delay (Apitime.Period.lmake ~hour:1 ())
    | 903 -> `Delay (Apitime.Period.lmake ~hour:1 ())
    | 904 -> `Delay (Apitime.Period.lmake ~day:1 ())
    | _ -> `Stop
