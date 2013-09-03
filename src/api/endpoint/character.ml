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

(* http://wiki.eve-id.net/APIv2_Char_ContactList_XML *)
let contactList =
  let decode_contact = function
    | [ "contactID", id ; "contactName", name ;
        "inWatchList", watched; "standing", standing ;
        "contactTypeID", ctypeID ]
      -> (entity ~name ~id, ios standing, ios ctypeID)
    | [ "contactID", id ; "contactName", name ;
        "standing", standing ; "contactTypeID", ctypeID ]
      -> (entity ~name ~id, ios standing, ios ctypeID)
    | _ -> raise (Response.Wrong "contactListw")
  in
  let decode = function
    | [ char ; corp ; ally ] ->
        List.map decode_contact char, List.map decode_contact corp, List.map decode_contact ally
    | _ -> raise (Response.Wrong "contactList'")
  in
  {
    uri = "/char/ContactList.xml.aspx" ;
    cache = Long ;
    auth = charkey ;
    param = no_param ;
    result = Response.extract_rowsets ;
    decode ;
  }
let get_contactList = apply_api_call Auth tq contactList

(* http://wiki.eve-id.net/APIv2_Char_JournalEntries_XML *)
let walletJournal =
  let enc_fromID i = [ "fromID", soi i ]
  and enc_rowCount i = [ "rowCount", soi i ]
  in
  let decode_line = function
    | [ "date",date ; "refID",refID ; "refTypeID",refTypeID ;
        "ownerName1",ownerName1 ; "ownerID1",ownerID1 ; "ownerName2",ownerName2 ;
        "ownerID2",ownerID2 ; "argName1",argName1 ; "argID1",argID1 ;
        "amount",amount ; "balance",balance ; "reason",reason ;
        "taxReceiverID",taxReceiverID ; "taxAmount",taxAmount ] ->
        let owner1 = entity ownerName1 ownerID1 in
        let owner2 = entity ownerName2 ownerID2 in
        let arg = entity argName1 argID1 in
        {
           date = s2date date ;
           refID = ios refID ;
           refTypeID = ios refTypeID ;
           owner1 = owner1 ;
           owner2 = owner2 ;
           argument = arg ;
           amount = fos amount ;
           balance = fos balance ;
           reason = reason ;
           taxReceiverID = sopti taxReceiverID ;
           taxAmount = soptf taxAmount ;
        }
    | _ -> raise (Response.Wrong "walletJournal")
  in
  {
    uri = "/char/WalletJournal.xml.aspx" ;
    cache = MShort ;
    auth = charkey ;
    param = !? enc_fromID ** !? enc_rowCount ;
    result = Response.extract_rowset ;
    decode = List.map decode_line ;
  }
let get_walletJournal ?fromID ?rowCount ~key =
  apply_api tq walletJournal key (fromID,rowCount)

(* http://wiki.eve-id.net/APIv2_Char_MarketTransactions_XML *)
let walletTransactions =
  let enc_fromID i = [ "fromID", soi i ]
  and enc_rowCount i = [ "rowCount", soi i ]
  in
  {
    uri = "/char/WalletTransactions.xml.aspx" ;
    cache = MShort ;
    auth = charkey ;
    param = !? enc_fromID ** !? enc_rowCount ;
    result = Response.extract_rowset ;
    decode = fun i -> i ;
  }
let get_walletTransactions ?fromID ?rowCount ~key =
  apply_api tq walletTransactions key (fromID,rowCount)
