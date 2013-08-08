
module SMap = Map.Make(String)

module Response = struct

  open Simplexmlparser

  exception Wrong of string

  type 'a t = {
    version : string ;
    currentTime : string ;
    cachedUntil : string ;
    data : 'a
  }

  type 'a extract = Simplexmlparser.xml list -> 'a t

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
         [ Element ("currentTime", [], [PCData currentTime]) ;
           Element ("result", [], data) ;
           Element ("cachedUntil", [], [PCData cachedUntil])
         ])]
      -> { version ; currentTime ; cachedUntil ; data }
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

(** Convenience stuff *)

let aoc = List.assoc
let ios = int_of_string
let soi = string_of_int
let fos = float_of_string
let sof = string_of_float

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
let encode_apikey (k : apikey) =
  [ ("keyID", string_of_int (k#keyId)) ; ("vCode", k#vCode) ]

type charkey = < keyId : int ; vCode : string ; characterID : int >
let apikey ~keyId ~vCode ~charId : charkey =
  object
    method keyId = keyId
    method vCode = vCode
    method characterID = charId
  end
let encode_charkey (k : charkey) =
  ("characterID", string_of_int (k#characterID)) :: encode_apikey (k :> apikey)

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

type enc_param = (string * string) list

type ('extract, 'auth , 'param, 'out) internal_api = {
  cache : cache ;
  result : 'extract Response.extract ;
  auth : 'auth -> enc_param ;
  param : 'param -> enc_param ;
  decode : 'extract -> 'out ;
  uri : string ;
}

let apply_api prefix endpoint =
  let cont args =
    lwt response = http_fetch prefix endpoint.uri args in
    let data = Response.( (endpoint.result response).data ) in
    Lwt.return (endpoint.decode data)
  in
  let f auth param =
    cont (endpoint.auth auth @ endpoint.param param)
  in f

let apply_api_call call prefix endpoint =
  adapt_call call (apply_api prefix endpoint)

(** {2 Spectific APIs} *)

(** {5 Account} *)

let accountStatus =
  {
    uri = "/account/AccountStatus.xml.aspx" ;
    cache = Short ;
    auth = encode_apikey ;
    param = no_param ;
    result = Response.extract_tags;
    decode = no_decode ;
  }
let get_accountStatus = apply_api_call Auth tq accountStatus

let apiKeyInfo =
  {
    uri = "/account/APIKeyInfo.xml.aspx" ;
    cache = Short ;
    auth = encode_apikey ;
    param = no_param ;
    result = Response.extract_rowset;
    decode = fun i -> i ;
  }
let get_apiKeyInfo = apply_api_call Auth tq apiKeyInfo

let characters =
  let dec x = match x with
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
  {
    uri = "/account/Characters.xml.aspx" ;
    cache = Short ;
    auth = encode_apikey ;
    param = no_param ;
    result = Response.extract_rowset ;
    decode = List.map dec ;
  }
let get_characters = apply_api_call Auth tq characters

(** {5 Character } *)

let accountBalance =
  {
    uri = "/char/AccountBalance.xml.aspx" ;
    cache = Short ;
    auth = encode_charkey ;
    param = no_param ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_accountBalance = apply_api_call Auth tq accountBalance

let assetList =
  {
    uri = "/char/AssetList.xml.aspx" ;
    cache = Long ;
    auth = encode_charkey ;
    param = no_param ;
    result = Response.extract_rowset2 ;
    decode = fun i -> i ;
  }
let get_assetList = apply_api_call Auth tq assetList

(* http://wiki.eve-id.net/APIv2_Char_CalendarEventAttendees_XML *)
(* TODO multiple event virgule separated *)
let calendarEventAttendees =
  {
    uri = "/char/CalendarEventAttendees.xml.aspx" ;
    cache = MShort ;
    auth = encode_charkey ;
    param = (fun x -> [ "eventIDs", soi x ]) ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_calendarEventAttendees = apply_api tq calendarEventAttendees

(* http://wiki.eve-id.net/APIv2_Char_CharacterSheet_XML *)
let characterSheet =
  {
    uri = "/char/CharacterSheet.xml.aspx" ;
    cache = MShort ;
    auth = encode_charkey ;
    param = no_param ;
    result = Response.extract ;
    decode = fun i -> i ;
  }
let get_characterSheet = apply_api_call Auth tq characterSheet

(* http://wiki.eve-id.net/APIv2_Char_JournalEntries_XML *)
let walletJournal =
  let enc_fromID = function | Some i -> [ "fromID", soi i ] | None -> []
  and enc_rowCount = function | Some i -> [ "rowCount", soi i ] | None -> []
  in
  {
    uri = "/char/WalletJournal.xml.aspx" ;
    cache = MShort ;
    auth = encode_charkey ;
    param = enc_fromID ** enc_rowCount ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_walletJournal ?fromID ?rowCount ~key =
  apply_api tq walletJournal key (fromID,rowCount)

(* http://wiki.eve-id.net/APIv2_Char_MarketTransactions_XML *)
let walletTransactions =
  let enc_fromID = function | Some i -> [ "fromID", soi i ] | None -> []
  and enc_rowCount = function | Some i -> [ "rowCount", soi i ] | None -> []
  in
  {
    uri = "/char/WalletTransactions.xml.aspx" ;
    cache = MShort ;
    auth = encode_charkey ;
    param = enc_fromID ** enc_rowCount ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_walletTransactions ?fromID ?rowCount ~key =
  apply_api tq walletTransactions key (fromID,rowCount)

(** {5 Corporation } *)

(** {5 Eve } *)

let characterInfo =
  let enc cID = [ ("characterID", cID) ] in
  {
    uri = "/eve/CharacterInfo.xml.aspx" ;
    cache = Short ;
    auth = no_param ;
    param = enc ;
    result = Response.extract_tags ;
    decode = fun i -> i ;
  }
let get_characterInfo = apply_api_call Param tq characterInfo


(** {5 Map } *)

(** {5 Server } *)

(** {5 API } *)
