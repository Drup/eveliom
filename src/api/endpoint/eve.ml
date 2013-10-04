open Apidsl

let characterInfo =
  let enc cID = [ ("characterID", cID) ] in
  Api {
    uri = "/eve/CharacterInfo.xml.aspx" ;
    cache = Short ;
    auth = no_param ;
    param = enc ;
    result = Response.extract_tags ;
    decode = id ;
  }
let get_characterInfo = apply_api_call Param tq characterInfo

let allianceList =
  let extract r =
    let open Response in
    map (parse_rowset @> parse_row' @@ parse_rowset @> parse_row) (extract r)
  in
  let decode_corp = function
    | [ "corporationID", corpID ; "startDate", date ] ->
        (ios corpID, s2date date)
    | _ -> raise (Response.Wrong "AllianceList")
  in
  let decode_ally = function
    | [ "name", name ; "shortName", ticker ; "allianceID", id ;
        "executorCorpID", exCorpId ; "memberCount", member ; "startDate", date ], corp
      -> entity ~name ~id, ticker,
        ios exCorpId, ios member, s2date date, List.map decode_corp corp
    | _ -> raise (Response.Wrong "AllianceList")
  in
  Api {
    uri = "/eve/AllianceList.xml.aspx" ;
    cache = Short ;
    auth = no_param ;
    param = no_param ;
    result = extract ;
    decode = List.map decode_ally ;
  }
let get_allianceList = apply_api_call Nothing tq allianceList
