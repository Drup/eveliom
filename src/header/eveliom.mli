
(** Igb headers access
	
	This module contains all the necessary informations to access the igb's headers. 
	This module is server-side only.
*)

(** Contains all the information provided by the IGB *)
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

(** The current state of igb headers. *)
type igb_state = NotIGB | IGB | Trusted of igb_headers

(** Return the current igb_state. Can only be executed inside a service. *)
val get_igb_headers : unit -> igb_state

(** This function will choose between three functions according to the igb state. Can only be executed inside a service. *)
val wrap_igb : trusted:(igb_headers -> 'a) -> nottrusted:'a -> notigb:'a -> unit -> 'a
