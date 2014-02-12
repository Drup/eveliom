open Lwt
open Apicall

module type API = sig
  type response
  type storage
  val call : unit -> response Response.t Lwt.t
  val init : unit -> storage Lwt.t
  val update : storage -> response -> storage
  val name : string
  val period : 'd Apitime.apiperiod option
  val store : Ocsipersist.store
end

module Caching (R : API) = struct

  let with_name s = Printf.sprintf s R.name
  let warning s = Ocsigen_messages.warning (with_name s)
  let error s = Ocsigen_messages.errlog (with_name s)

  let call () =
    warning "Updating %s ..." ;
    lwt x = R.call () in
    warning "%s updated!" ;
    Lwt.return x

  let table =
    Ocsipersist.make_persistent_lazy_lwt
      ~store:R.store
      ~name:R.name
      ~default:(fun () ->
        lwt init = R.init () in
        call () >|= Response.cast >|= Response.mapr (R.update init))

  let get () =
    table >>= Ocsipersist.get

  let set x =
    lwt t = table in Ocsipersist.set t x

  let update r =
    let open Response in
    lwt current = get () in
    lwt () = set (Response.mapr (R.update current.Response.data) r) in
    Lwt.return `Ok

  let error (x,s) =
    let err_result = handle_error ~log:Ocsigen_messages.errlog (x,s) in
    match err_result with
      | `Delay _ | `KeepGoing | `Retry as r -> r
      | r -> error "Stop updating %s." ; r

  let thread : _ Lwt.t =
    let delay = R.period in
    let error x = Lwt.wrap (fun () -> error x) in
    let log d = Ocsigen_messages.warning
        (with_name "%s : Next update in %is" (Apitime.to_seconds d)) in
    periodic_update ~log ?delay ~call ~error ~update ()

end
