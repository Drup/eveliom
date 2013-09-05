(** Api spectification and access. *)

module Response :
sig
  exception Wrong of string
  exception ApiError of (int * string)
  type 'a result = {
    version : string ;
    cachedUntil : Time.t ;
    data : 'a ;
  }
  type 'a t =
    | Result of 'a result
    | Error of (int * string)

end

val tq : string
(** {{: api.eveonline.com} api.eveonline.com} *)

val test : string
(** {{: api.testeveonline.com} api.testeveonline.com} *)

type apikey = Apidsl.apikey
val apikey : keyId:int -> vCode:string -> apikey

type charkey = Apidsl.charkey
val charkey : keyId:int -> vCode:string -> charId:int -> charkey

val add_char : key:apikey -> charId:int -> charkey

type ('auth, 'param, 'out) api = ('auth, 'param, 'out) Apidsl.api

val apply_api :
  ?https:bool -> string -> ('auth, 'param, 'out) api ->
  'auth -> 'param -> 'out Response.t Lwt.t

val periodic_update :
  (unit -> 'a Response.result Lwt.t) ->
  ('a Response.result -> ([> `Ok ] as 'b) Lwt.t) -> 'b Lwt.t
(**
   [ periodic_update call update ] will execute [ call ] and apply [ update ] to the result.

   If [update] returns [ `Ok ], then [ periodic_update call update ] will be called again when the result returned by the previous [call] is not cached anymore, according to the [ cachedUntil ] field.

   If [update] returns anything else than [ `Ok ], the thread returns imediatly with this result.

   the [call] and [update] functions should raise exceptions in the Lwt way.
   The thread returned can be canceled.
*)

(** {2 APIs} *)

type entity = Apidsl.entity = {name : string ; id : int }

type walletJournal =
  {
    date : CalendarLib.Calendar.Precise.t ;
    refID : int ;
    refTypeID : int ;
    owner1 : entity ;
    owner2 : entity ;
    argument : entity ;
    amount : float ;
    balance : float ;
    reason : string ;
    taxReceiverID : int ;
    taxAmount : float ;
  }

module Account : module type of Account

module Character : module type of Character

module Corporation : module type of Corporation

module Eve : module type of Eve

module EveMap : module type of Evemap

(** Simple module to assemble link to the image API server. *)
module Image : sig

  (** The two usual image server. *)

  val tq : string
  (** {{: http://image.eveonline.com} http://image.eveonline.com} *)

  val test : string
  (** {{: http://image.testeveonline.com} http://image.testeveonline.com} *)

  (** Each function correspond to an endpoint.
      The first optionnal argument is the prefix, default to {! tq }.
      The 2nd argument is a variant indicating the size of the picture, in px.
      The 3rd argument is the entity id.
  *)

  open Eliom_content_core.Xml
  val ally :   ?api:string -> [< `T32 | `T64 | `T128 ]                 -> int -> uri
  val corp :   ?api:string -> [< `T32 | `T64 | `T128 | `T256 ]         -> int -> uri
  val char :   ?api:string -> [< `T32 | `T64 | `T128 | `T256 | `T512 ] -> int -> uri
  val typ :    ?api:string -> [< `T32 | `T64 ]                         -> int -> uri
  val render : ?api:string -> [< `T32 | `T64 | `T128 | `T256 | `T512 ] -> int -> uri
end


(** {2 Specific modules} *)

module Wallet : module type of Wallet
