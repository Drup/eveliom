open Apicall
open Apitype

module M = Map.Make(CalendarLib.Calendar.Precise)

type t = Apitype.walletJournal M.t

let get_first w =
  snd (M.max_binding w)

let get_last w =
  snd (M.min_binding w)

let list2wallet ?(w=M.empty) l =
  let aux w wj = M.add wj.date wj w in
  List.fold_left aux w l

let is_subset l w =
  let rec aux = function
    | [] -> true
    | wjl::t -> M.mem wjl.date w && aux t
  in
  aux l

let walk_forward ~uri ~key ?(rowCount=2500) ?(w=M.empty) () =
  let open Response in
  let f () = apply_api uri Character.walletJournal key (None, Some rowCount) in
  let rec aux w =
    lwt l = f () in
    let l = cast l in
    if is_subset l.data w then
      Lwt.return (with_data l w)
    else
      aux (list2wallet ~w l.data)
  in
  aux w

let walk_backward ~uri ~key ?(rowCount=2500) ?fromID ?(w=M.empty) () =
  let open Response in
  let f from = apply_api uri Character.walletJournal key (None,from) in
  let rec aux w from =
    lwt l = f from in
    let l = cast l in
    if l.data = [] then
      Lwt.return (with_data l w)
    else
      let w = list2wallet ~w l.data in aux w (Some (get_last w).refID)
  in
  let from = match fromID with
    | Some i -> Some i
    | None -> if M.is_empty w then None else Some (get_last w).refID
  in
  aux w from

let get_everything ~uri ~key ?rowCount () =
  lwt w = walk_forward ~uri ?rowCount ~key () in
  lwt w = walk_backward ~uri ?rowCount ~w:w.Response.data ~key () in
  Lwt.return w
