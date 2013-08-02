
module SMap = Map.Make(String)

module Response = struct

  open Simplexmlparser

  exception Wrong of string

  type _ extract =
    | Tags : (string * string) list extract
    | Rowset : (string * string) list list extract
    | Rowsets : (string * string) list list list extract

  type 'a t = {
    version : string ;
    currentTime : string ;
    cachedUntil : string ;
    data : 'a
  }

  let map f
      { version ; currentTime ; cachedUntil ; data } =
    { version ; currentTime ; cachedUntil ; data = f data }

  let parse_row = function
    | Element ("row", l , []) -> l
    | _ -> raise (Wrong "row")

  let parse_rowset = function
    | Element ("rowset", [("name", name); ("key", key); ("columns", col)], rows)
      -> List.map parse_row rows
    | _ -> raise (Wrong "rowset")

  let parse_tag = function
    | Element (k, [], [PCData v]) -> (k, v)
    | _ -> raise (Wrong "tag")

  let parse_tags = List.map parse_tag

  let unwrap = function
    | [Element ("eveapi", [("version", version)],
         [ Element ("currentTime", [], [PCData currentTime]) ;
           Element ("result", [], data) ;
           Element ("cachedUntil", [], [PCData cachedUntil])
         ])]
      -> { version ; currentTime ; cachedUntil ; data }
    | _ -> raise (Wrong "unwrap")

  let extract_tags r = map parse_tags (unwrap r)

  let extract_rowset r  =
    let f = function
      | [ l ] -> parse_rowset l
      | _ -> raise (Wrong "rowset")
    in map f (unwrap r)

  let extract_rowsets r = map (List.map parse_rowset) (unwrap r)

  let extract (type a) (ex : a extract) xml : a t = match ex with
    | Tags -> extract_tags xml
    | Rowset -> extract_rowset xml
    | Rowsets -> extract_rowsets xml

end

(** Conveniance stuff *)

let aoc = List.assoc
let ios = int_of_string
let fos = float_of_string

module OS = Ocsigen_stream

(** Fetching stuff *)

let http_fetch prefix endpoint args =
  let open Ocsigen_http_client in
  lwt response = post_urlencoded ~host:prefix ~uri:endpoint ~content:args () in
  let stream = response.Ocsigen_http_frame.frame_content in
  lwt data = match stream with
    | None -> Lwt.return ""
    | Some s ->
        let body = OS.string_of_stream Sys.max_string_length (OS.get s) in
        lwt () = Ocsigen_stream.finalize s `Success in
        body
  in
  let xmldata = Simplexmlparser.xmlparser_string data in
  Lwt.return xmldata

(** Common part of all APIs *)

type cache = Long | Short | MShort

let tq = "https://api.eveonline.com"

let test = "https://api.testeveonline.com"

(** Typing madness ! *)

type apikey = < keyId : int ; vCode : string >
let apikey ~keyId ~vCode : apikey =
  object
    method keyId = keyId
    method vCode = vCode
  end
let encode_apikey k =
  [ ("keyID", string_of_int (k#keyId)) ; ("vCode", k#vCode) ]

type charkey = < keyId : int ; vCode : string ; characterID : int >
let apikey ~keyId ~vCode ~charId : charkey =
  object
    method keyId = keyId
    method vCode = vCode
    method characterID = charId
  end
let encode_charkey k =
  ("characterID", string_of_int (k#characterID)) :: encode_apikey k

type enc_param = (string * string) list

type (_,_) param =
  | NoParam : ('out,'out) param
  | Param : ('a -> enc_param) -> ('out,'a -> 'out) param

type (_,_) auth =
  | NoAuth : ('a, 'a) auth
  | ApiKey : ('a, apikey -> 'a) auth
  | CharKey : ('a, charkey -> 'a) auth

let api_call (type tf) (type tp) (type tout)
    (p : (tout, tp) param)
    (k : (tp, tf) auth)
    (cont : enc_param -> tout) : tf =
  match k, p with
    | NoAuth, NoParam -> cont []
    | ApiKey, NoParam -> fun a -> cont (encode_apikey a)
    | CharKey, NoParam -> fun a -> cont (encode_charkey a)
    | NoAuth, Param f -> fun b -> cont (f b)
    | ApiKey, Param f -> fun a b -> cont (encode_apikey a @ f b)
    | CharKey, Param f -> fun a b -> cont (encode_charkey a @ f b)

type ('extract, 'funtype , 'param, 'out) internal_api = {
  uri : string ;
  cache : cache ;
  result : 'extract Response.extract ;
  auth : ('param, 'funtype) auth ;
  param : ('out Lwt.t, 'param) param ;
  decode : 'extract -> 'out
}
type _ api = Api : (_, 'a, _ , _) internal_api -> 'a api

let apply_api prefix (Api endpoint) =
  let cont args =
    lwt response = http_fetch prefix endpoint.uri args in
    let data = Response.( (extract endpoint.result response).data ) in
    Lwt.return (endpoint.decode data)
  in
  api_call endpoint.param endpoint.auth cont

(** Spectific APIs *)

let characters =
  let dec = function
    | [ "name",name ; "characterID",characterID ; "corporationName",corpID ;
        "corporationName", corpName ] ->
        object
          method characterID = ios characterID
          method name = name
          method corporationID = ios corpID
          method corporationName = corpName
        end
    | _ -> raise (Response.Wrong "characters")
  in
  Api {
    uri = "/account/Characters.xml.aspx" ;
    cache = Short ;
    auth = ApiKey ;
    param = NoParam ;
    result = Response.Rowset ;
    decode = List.map dec ;
  }
let get_characters = apply_api tq characters

let characterInfo =
  let enc cID = [ ("characterID", cID) ] in
  Api {
    uri = "/eve/CharacterInfo.xml.aspx" ;
    cache = Short ;
    auth = NoAuth ;
    param = Param enc ;
    result = Response.Tags ;
    decode = fun i -> i ;
  }
let get_characterInfo = apply_api tq characterInfo

(* type (_,_) compo_list = *)
(*   | Nil : ('a,'a) compo_list *)
(*   | NoFun : ('a, 'b) compo_list -> ('a, 'b) compo_list *)
(*   | Fun : ('b -> 'c) * ('b, 'a) compo_list -> ('c, 'a) compo_list *)
(*   | FunParam : ('b -> 'p -> 'c) * ('b, 'a) compo_list -> ('p -> 'c, 'a) compo_list *)

(* let rec get_fun : type t1 t2 . (t1, t2) compo_list -> t2 -> t1 = function *)
(*   | Nil -> (fun x -> x) *)
(*   | NoFun l -> get_fun l *)
(*   | Fun (f,l) -> let fl = get_fun l in fun x -> (f (fl x)) *)
(*   | FunParam (f,l) -> let fl = get_fun l in fun x -> (f (fl x)) *)

(* let l = Fun (float ,FunParam ((+), Nil)) *)
