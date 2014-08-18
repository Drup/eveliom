open Apitype
open Ocsigen_lib

module OS = Ocsigen_stream

(** Fetching stuff *)

let http_fetch ?(log=ignore) ?(https=false) prefix endpoint args =
  let scheme = if https then "https" else "http" in
  let target = Uri.make ~scheme ~host:prefix ~path:endpoint ~query:args () in
  log ("Request to " ^ Uri.to_string target) ;
  lwt response = Cohttp_lwt_unix.Client.get target
  in
  lwt data = Cohttp_lwt_body.to_string (snd response) in
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

type enc_param = (string * string list) list

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

let apply_api ?log ?(https=false) prefix (Api endpoint) =
  let cont args =
    lwt response = http_fetch ?log ~https prefix endpoint.uri args in
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
