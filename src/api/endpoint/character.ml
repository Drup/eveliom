open Apidsl

let accountBalance =
  {
    uri = "/char/AccountBalance.xml.aspx" ;
    cache = Short ;
    auth = charkey ;
    param = no_param ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_accountBalance = apply_api_call Auth tq accountBalance

let assetList =
  {
    uri = "/char/AssetList.xml.aspx" ;
    cache = Long ;
    auth = charkey ;
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
    auth = charkey ;
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
    auth = charkey ;
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
    auth = charkey ;
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
    auth = charkey ;
    param = enc_fromID ** enc_rowCount ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_walletTransactions ?fromID ?rowCount ~key =
  apply_api tq walletTransactions key (fromID,rowCount)
