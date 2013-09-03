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

  let with_data r d = map (fun _ -> d) r

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

let tq = "api.eveonline.com"

let test = "api.testeveonline.com"

(** Typing madness ! *)

type apikey = < keyId : int ; vCode : string >
let apikey ~keyId ~vCode : apikey =
  object
    method keyId = keyId
    method vCode = vCode
  end

type charkey = < keyId : int ; vCode : string ; characterID : int >
let charkey ~keyId ~vCode ~charId : charkey =
  object
    method keyId = keyId
    method vCode = vCode
    method characterID = charId
  end

type enc_param = (string * string) list

type ('extract, 'auth , 'param, 'out) api = {
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
    let data = Response.map endpoint.decode (endpoint.result response) in
    Lwt.return data
  in
  let f auth param =
    cont (endpoint.auth auth @ endpoint.param param)
  in f
