(** Gadt spaggeti : A kind of composable function list *)

(** We need a monoid to group results *)
type 'a mono = { empty : 'a ; plus : 'a -> 'a -> 'a }
let list_mono = { empty = [] ; plus = (@) }

type (_,_,_) elem =
  | NoParam : ('enc,'out,'out) elem
  | Param : ('arg -> 'enc) -> ('enc,'out,'arg -> 'out) elem

type (_,_,_) t =
  | Nil : ('enc,'out,'out) t
  | Cons : ('enc,'a,'fu) elem * ('enc,'out,'a) t -> ('enc, 'out, 'fu) t

let (@+) t l = Cons (t,l)

let singleton t = Cons(t,Nil)
let singfun f = Cons(Param f, Nil)

let rec append : type x i o . (_,x,i) t -> (_,o,x) t -> (_,o,i) t =
  fun l1 l2 -> match l1 with
    | Cons(x,l) -> Cons(x,append l l2)
    | Nil -> l2

let glue mono const l =
  let rec aux : type enc out a .
        enc mono -> (enc -> out) -> enc -> (enc,out,a) t -> a =
    fun mono const acc -> function
    | Cons (NoParam,l) -> aux mono const acc l
    | Cons (Param f,l) -> (fun arg -> aux mono const (mono.plus (f arg) acc) l)
    | Nil -> const acc
  in
  aux mono const mono.empty l
