
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

type enc_param = (string * string) list

type ('extract, 'a , 'b, 'c) internal_api = {
  uri : string ;
  cache : cache ;
  result : 'extract Response.extract ;
  auth : (enc_param, 'b, 'c) Clist.t ;
  param : (enc_param, 'a Lwt.t, 'b) Clist.t ;
  decode : 'extract -> 'a
}
type _ api = Api : (_, _, _, 'a) internal_api -> 'a api

let get_cache (Api x) = x.cache
let get_uri (Api x) = x.uri

let apply_api prefix (Api endpoint) =
  let cont args =
    lwt response = http_fetch prefix endpoint.uri args in
    let data = Response.( (endpoint.result response).data ) in
    Lwt.return (endpoint.decode data)
  in
  Clist.(glue list_mono cont (append endpoint.auth endpoint.param))

(** {2 Spectific APIs} *)

(** {5 Account} *)

let accountStatus =
  Api {
    uri = "/account/AccountStatus.xml.aspx" ;
    cache = Short ;
    auth = Clist.singfun encode_apikey ;
    param = Clist.Nil ;
    result = Response.extract_tags;
    decode = fun i -> i ;
  }
let get_accountStatus = apply_api tq accountStatus

let apiKeyInfo =
  Api {
    uri = "/account/APIKeyInfo.xml.aspx" ;
    cache = Short ;
    auth = Clist.singfun encode_apikey ;
    param = Clist.Nil ;
    result = Response.extract_rowset;
    decode = fun i -> i ;
  }
let get_apiKeyInfo = apply_api tq apiKeyInfo

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
  Api {
    uri = "/account/Characters.xml.aspx" ;
    cache = Short ;
    auth = Clist.singfun encode_apikey ;
    param = Clist.Nil ;
    result = Response.extract_rowset ;
    decode = List.map dec ;
  }
let get_characters = apply_api tq characters

(** {5 Character } *)

let accountBalance =
  Api {
    uri = "/char/AccountBalance.xml.aspx" ;
    cache = Short ;
    auth = Clist.singfun encode_charkey ;
    param = Clist.Nil ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_accountBalance = apply_api tq accountBalance

let assetList =
  Api {
    uri = "/char/AssetList.xml.aspx" ;
    cache = Long ;
    auth = Clist.singfun encode_charkey ;
    param = Clist.Nil ;
    result = Response.extract_rowset2 ;
    decode = fun i -> i ;
  }
let get_assetList = apply_api tq assetList

(* http://wiki.eve-id.net/APIv2_Char_CalendarEventAttendees_XML *)
(* TODO multiple event virgule separated *)
let calendarEventAttendees =
  Api {
    uri = "/char/CalendarEventAttendees.xml.aspx" ;
    cache = MShort ;
    auth = Clist.singfun encode_charkey ;
    param = Clist.singfun (fun x -> [ "eventIDs", soi x ]) ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_calendarEventAttendees = apply_api tq calendarEventAttendees

(* http://wiki.eve-id.net/APIv2_Char_CharacterSheet_XML *)
let characterSheet =
  Api {
    uri = "/char/CharacterSheet.xml.aspx" ;
    cache = MShort ;
    auth = Clist.singfun encode_charkey ;
    param = Clist.Nil ;
    result = Response.extract ;
    decode = fun i -> i ;
  }
let get_characterSheet = apply_api tq characterSheet

(* http://wiki.eve-id.net/APIv2_Char_JournalEntries_XML *)
let walletJournal =
  let enc_fromID = function | Some i -> [ "fromID", soi i ] | None -> []
  and enc_rowCount = function | Some i -> [ "rowCount", soi i ] | None -> []
  in
  Api {
    uri = "/char/WalletJournal.xml.aspx" ;
    cache = MShort ;
    auth = Clist.singfun encode_charkey ;
    param = Clist.(Param enc_fromID @+ Param enc_rowCount @+ Nil) ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_walletJournal ?fromID ?rowCount ~key =
  apply_api tq walletJournal key fromID rowCount

(* http://wiki.eve-id.net/APIv2_Char_MarketTransactions_XML *)
let walletTransactions =
  let enc_fromID = function | Some i -> [ "fromID", soi i ] | None -> []
  and enc_rowCount = function | Some i -> [ "rowCount", soi i ] | None -> []
  in
  Api {
    uri = "/char/WalletTransactions.xml.aspx" ;
    cache = MShort ;
    auth = Clist.singfun encode_charkey ;
    param = Clist.(Param enc_fromID @+ Param enc_rowCount @+ Nil) ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_walletTransactions ?fromID ?rowCount ~key =
  apply_api tq walletTransactions key fromID rowCount

(** {5 Corporation } *)

(** {5 Eve } *)

let characterInfo =
  let enc cID = [ ("characterID", cID) ] in
  Api {
    uri = "/eve/CharacterInfo.xml.aspx" ;
    cache = Short ;
    auth = Clist.Nil ;
    param = Clist.singfun enc ;
    result = Response.extract_tags ;
    decode = fun i -> i ;
  }
let get_characterInfo = apply_api tq characterInfo


(** {5 Map } *)

(** {5 Server } *)

(** {5 API } *)
