(** IGB headers access for Eliom.

    Takes advantage of Eliom's to provide easy and convenient access to IGB headers.
    Functions from this module should only be executed inside a service.
*)

type t = {
    trusted : bool;
    serverip : string;
    charname : string;
    charid : int;
    corpname : string;
    corpid : int;
    alliancename : string option;
    allianceid : int option;
    regionname : string;
    regionid : int;
    constellationname : string;
    solarsystemname : string;
    stationname : string option;
    stationid : int option;
    corprole : string option;
    solarsystemid : int;
    warfactionid : int option;
    shipid : int;
    shipname : string;
    shiptypeid : int;
    shiptypename : string;
  }

type state = NotIGB | IGB | Trusted of t

(** Return the current igb_state. Can only be executed inside a service. *)
val get : unit -> state

(** This function will choose between three functions according to the igb state. Can only be executed inside a service. *)
val wrap : trusted:(t -> 'a) -> nottrusted:'a -> notigb:'a -> unit -> 'a
