open Apidsl

let characterInfo =
  let enc cID = [ ("characterID", cID) ] in
  Api {
    uri = "/eve/CharacterInfo.xml.aspx" ;
    cache = Short ;
    auth = no_param ;
    param = enc ;
    result = Response.extract_tags ;
    decode = fun i -> i ;
  }
let get_characterInfo = apply_api_call Param tq characterInfo
