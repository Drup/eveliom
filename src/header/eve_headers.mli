(** IGB headers access

    This module allows to extract Eve online's IGB headers information.
    This module is server-side only.

    IGB headers are partially documented {{: https://wiki.eveonline.com/en/wiki/IGB_Headers}here}.
*)

(** An abstract Headers module.

    Corresponds exactly to Ocsigen's {! Http_headers }. *)
module type Headers = sig
  type t
  type name
  val name : string -> name
  val name_to_string : name -> string
  val find : name -> t -> string
end

(** Functorized implementation of IGB headers access. *)
module Make (H : Headers) : sig

  val is_trusted : H.t -> bool option

  (** Contains all the information provided by the IGB for a trusted website.*)
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

  val get : H.t -> t

  (** A safer interface, that prevents access to unavailable headers. *)
  type state = NotIGB | IGB | Trusted of t
  val get_state : H.t -> state

end
