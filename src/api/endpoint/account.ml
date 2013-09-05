open Apidsl

let accountStatus =
  Api {
    uri = "/account/AccountStatus.xml.aspx" ;
    cache = Short ;
    auth = apikey ;
    param = no_param ;
    result = Response.extract_tags;
    decode = no_decode ;
  }
let get_accountStatus = apply_api_call Auth tq accountStatus

let apiKeyInfo =
  Api {
    uri = "/account/APIKeyInfo.xml.aspx" ;
    cache = Short ;
    auth = apikey ;
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
  Api {
    uri = "/account/Characters.xml.aspx" ;
    cache = Short ;
    auth = apikey ;
    param = no_param ;
    result = Response.extract_rowset ;
    decode = List.map dec ;
  }
let get_characters = apply_api_call Auth tq characters
