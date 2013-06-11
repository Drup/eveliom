
module MiniScanf = struct 
  
  type _ proj = 
	  Int : int proj 
	| String : string proj
	| Bool : bool proj
	 
  let bool_of_string = function
	  "Yes" | "yes" | "true" -> true
	| _ -> false

  let rec proj : type a.(a proj -> (string -> a)) = fun p -> match p with
	Int -> int_of_string
  | String -> (fun x -> x)
  | Bool -> bool_of_string

end

module Igb_header = struct

  open MiniScanf

  let compose_name s = 
	Http_headers.name ("EVE_"^s) 

  let trusted = compose_name "TRUSTED", Bool
  let serverip = compose_name "SERVERIP", String (* this is discutable *)
  let charname = compose_name "CHARNAME", String
  let charid = compose_name "CHARID", Int
  let corpname = compose_name "CORPNAME", String
  let corpid = compose_name "CORPID", Int
  let alliancename = compose_name "ALLIANCENAME", String
  let allianceid = compose_name "ALLIANCEID", Int
  let regionname = compose_name "REGIONNAME", String
  let regionid = compose_name "REGIONID", Int
  let constellationname = compose_name "CONSTELLATIONNAME", String
  let solarsystemname = compose_name "SOLARSYSTEMNAME", String
  let stationname = compose_name "STATIONNAME", String
  let stationid = compose_name "STATIONID", Int
  let corprole = compose_name "CORPROLE", String
  let solarsystemid = compose_name "SOLARSYSTEMID", Int
  let warfactionid = compose_name "WARFACTIONID", Int
  let shipid = compose_name "SHIPID", Int
  let shipname = compose_name "SHIPNAME", String
  let shiptypeid = compose_name "SHIPTYPEID", Int
  let shiptypename = compose_name "SHIPTYPENAME", String

end

open Igb_header

(* Some general fonctions on headers *)

let fetch_headers () = 
  let open Ocsigen_http_frame in
  let open Http_header in
  let ri = Eliom_request_info.get_ri () in
  get_headers ri.Ocsigen_extensions.ri_http_frame.frame_header

let find_header_raw_value headers (name,_) = 
  Http_headers.find name headers
	
let find_header (name,p) headers = 
  try MiniScanf.proj p (Http_headers.find name headers) 
  with Not_found -> failwith (Http_headers.name_to_string name)

let find_opt_header (name,p) headers = 
  try Some (MiniScanf.proj p (Http_headers.find name headers))
  with Not_found -> None


(* We are going to use an eref to make the igb easier to access 
   This eref will have the Request scope to be updated correctly for each request and will contain the current state of the igb
*)

type igb_headers = 
  {
	trusted :  bool ;
	serverip :  string ;
	charname :  string ;
	charid :  int ;
	corpname :  string ;
	corpid :  int ;
	alliancename :  string option ;
	allianceid :  int option ;
	regionname :  string ;
	regionid :  int ;
	constellationname :  string ;
	solarsystemname :  string ;
	stationname :  string option ;
	stationid :  int option ;
	corprole :  string option ;
	solarsystemid :  int ;
	warfactionid :  int option;
	shipid :  int ;
	shipname :  string ;
	shiptypeid :  int ;
	shiptypename :  string 
  }

type igb_state = NotIGB | IGB | Trusted of igb_headers

(** The IGB object contains all the informations available right now *)
let igb_headers headers = 
  let get_value x = find_header x headers in
  let get_opt_value x = find_opt_header x headers in
  let open Igb_header in
  {	trusted = get_value trusted ;
	serverip = get_value serverip ;
	charname = get_value charname ;
	charid = get_value charid ;
	corpname = get_value corpname ;
	corpid = get_value corpid ;
	alliancename = get_opt_value alliancename ;
	allianceid = get_opt_value allianceid ;
	regionname = get_value regionname ;
	regionid = get_value regionid ;
	constellationname = get_value constellationname ;
	solarsystemname = get_value solarsystemname ;
	stationname = get_opt_value stationname ;
	stationid = get_opt_value stationid ;
	corprole = get_opt_value corprole ;
	solarsystemid = get_value solarsystemid ;
	warfactionid = get_opt_value warfactionid ;
	shipid = get_value shipid ;
	shipname = get_value shipname ;
	shiptypeid = get_value shiptypeid ;
	shiptypename = get_value shiptypename ;
  }

let init_igb_headers () =
  let headers = fetch_headers () in
  let is_trusted = find_opt_header trusted headers in
  match is_trusted with 
	| None -> NotIGB
	| Some false -> IGB
	| Some true -> Trusted (igb_headers headers)

let headers_ref = 
  Eliom_reference.Volatile.eref_from_fun
	~scope:`Request
	init_igb_headers

let get_igb_headers () = 
  Eliom_reference.Volatile.get headers_ref

let wrap_igb ~trusted ~nottrusted ~notigb () = match get_igb_headers () with
  | NotIGB -> notigb
  | IGB -> nottrusted
  | Trusted x -> trusted x
