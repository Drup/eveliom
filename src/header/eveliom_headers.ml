
module M = Eve_headers.Make(Http_headers)
include M

(* How to fetch headers with Ocsigen/Eliom. *)
let fetch_headers () =
  let open Ocsigen_http_frame in
  let open Http_header in
  let ri = Eliom_request_info.get_ri () in
  get_headers (Ocsigen_extensions.Ocsigen_request_info.http_frame ri).frame_header


(* We are going to use an eref to make the igb easier to access
   This eref will have the Request scope to be updated correctly for each request and will contain the current state of the igb
*)

let init_igb_headers () =
  let headers = fetch_headers () in
  get_state headers

let headers_ref =
  Eliom_reference.Volatile.eref_from_fun
    ~scope:`Request
    init_igb_headers

let get () =
  Eliom_reference.Volatile.get headers_ref

let wrap ~trusted ~nottrusted ~notigb () = match get () with
  | NotIGB -> notigb
  | IGB -> nottrusted
  | Trusted x -> trusted x
