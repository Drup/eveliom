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
