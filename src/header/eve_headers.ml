
(** A small gadt based scanf *)
module MiniScanf = struct

  type _ proj =
    | Int : int proj
    | String : string proj
    | Bool : bool proj
    | Opt : 'a proj -> 'a option proj

  let bool_of_string = function
    | "Yes" | "yes" | "true" -> true
    | _ -> false

  let rec proj : type a .(a proj -> (string -> a)) = function
    | Int -> int_of_string
    | String -> (fun x -> x)
    | Bool -> bool_of_string
    | Opt p -> fun x -> Some (proj p x)

end

module type Headers = sig

  type t
  type name

  val name : string -> name
  val name_to_string : name -> string
  val find : name -> t -> string

end

(** Abstract igb headers access over the header library. *)
module Make (H : Headers) = struct

  open MiniScanf

  let name s = H.name ("EVE_"^s)

  let find (type a) headers name (p : a proj) : a =
    try MiniScanf.proj p (H.find (H.name name) headers)
    with Not_found ->
      match p with
        | Opt _ -> None
        | _ -> failwith name

  let is_trusted h = find h "TRUSTED" @@ Opt Bool

  (** The IGB object contains all the informations available when the website is trusted.*)
  type t = {
    trusted           : bool ;
    serverip          : string ;
    charname          : string ;
    charid            : int ;
    corpname          : string ;
    corpid            : int ;
    alliancename      : string option ;
    allianceid        : int option ;
    regionname        : string ;
    regionid          : int ;
    constellationname : string ;
    solarsystemname   : string ;
    stationname       : string option ;
    stationid         : int option ;
    corprole          : string option ;
    solarsystemid     : int ;
    warfactionid      : int option;
    shipid            : int ;
    shipname          : string ;
    shiptypeid        : int ;
    shiptypename      : string
  }

  let get h = {
    trusted           = find h "TRUSTED"           @@ Bool ;
    serverip          = find h "SERVERIP"          @@ String ;
    charname          = find h "CHARNAME"          @@ String ;
    charid            = find h "CHARID"            @@ Int ;
    corpname          = find h "CORPNAME"          @@ String ;
    corpid            = find h "CORPID"            @@ Int ;
    alliancename      = find h "ALLIANCENAME"      @@ Opt String ;
    allianceid        = find h "ALLIANCEID"        @@ Opt Int ;
    regionname        = find h "REGIONNAME"        @@ String;
    regionid          = find h "REGIONID"          @@ Int ;
    constellationname = find h "CONSTELLATIONNAME" @@ String;
    solarsystemname   = find h "SOLARSYSTEMNAME"   @@ String ;
    stationname       = find h "STATIONNAME"       @@ Opt String ;
    stationid         = find h "STATIONID"         @@ Opt Int ;
    corprole          = find h "CORPROLE"          @@ Opt String ;
    solarsystemid     = find h "SOLARSYSTEMID"     @@ Int;
    warfactionid      = find h "WARFACTIONID"      @@ Opt Int;
    shipid            = find h "SHIPID"            @@ Int ;
    shipname          = find h "SHIPNAME"          @@ String ;
    shiptypeid        = find h "SHIPTYPEID"        @@ Int ;
    shiptypename      = find h "SHIPTYPENAME"      @@ String ;
  }

  (* A safer interface. *)
  type state = NotIGB | IGB | Trusted of t
  let get_state h =
    match is_trusted h with
      | None -> NotIGB
      | Some false -> IGB
      | Some true -> Trusted (get h)

end
