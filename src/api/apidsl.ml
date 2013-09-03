include Apicall
include Apitype

(** Convenience stuff *)

let aoc = List.assoc
let ios = int_of_string
let soi = string_of_int
let fos = float_of_string
let sof = string_of_float
let bos = bool_of_string
let sob = string_of_bool

let s2date x : date = x

let entity ~name ~id = { name ; id = ios id }

let sopt f = function "" -> None | s -> Some (f s)
let sopti = function "" -> 0 | s -> ios s
let soptf = function "" -> 0. | s -> fos s

(** DSL stuff *)

let apikey (k : apikey) =
  [ ("keyID", string_of_int (k#keyId)) ; ("vCode", k#vCode) ]

let charkey (k : charkey) =
  ("characterID", string_of_int (k#characterID)) :: apikey (k :> apikey)

let no_param () = []
let no_decode id = id
let ( ** ) f g (x,y) = f x @ g y
let (!?) f = function Some x -> f x | None -> []

type ('t1, 't2) call_conv =
  | Nothing    : (unit -> unit -> 'a , unit -> 'a) call_conv
  | Auth       : ('a -> unit -> 'b , 'a -> 'b) call_conv
  | Param      : (unit -> 'a -> 'b , 'a -> 'b) call_conv
  | Param2     : (unit -> ('a * 'b) -> 'c, 'a -> 'b -> 'c) call_conv
  | AuthParam  : ('a -> 'b -> 'c, 'a -> 'b -> 'c) call_conv
  | AuthParam2 : ('a -> ('b * 'c) -> 'd, 'a -> 'b -> 'c -> 'd) call_conv

let adapt_call (type t) (type t') (c : (t,t') call_conv) (f : t) : t' =
  match c with
    | Nothing -> fun () -> f () ()
    | Auth -> fun x -> f x ()
    | Param -> fun x -> f () x
    | Param2 -> fun x y -> f () (x,y)
    | AuthParam -> fun x y -> f x y
    | AuthParam2 -> fun x y z -> f x (y,z)

let apply_api_call call prefix endpoint =
  adapt_call call (apply_api prefix endpoint)
