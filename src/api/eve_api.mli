
(** Api spectification and access. *)

module Response :
sig
  exception Wrong of string
  exception ApiError of (int * string)
  type 'a result = 'a Response.result = {
    version : string ;
    cachedUntil : CalendarLib.Calendar.t ;
    data : 'a ;
  }
  type 'a t = 'a Response.t =
    | Result of 'a result
    | Error of (int * string)

  val cast : 'a t -> 'a result
  val mapr : ('a -> 'b) -> 'a result -> 'b result

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
  ?log:([< CalendarLib.Period.date_field > `Day `Week ] CalendarLib.Calendar.Period.period -> unit) ->
  ?delay:([< CalendarLib.Period.date_field > `Day `Week ] CalendarLib.Calendar.Period.period as 'd)->
  call:(unit -> 'c Response.t Lwt.t) ->
  error:(int * string ->
    ([> `Delay of 'd
     | `KeepGoing
     | `Retry ]
     as 'a) Lwt.t) ->
  update:('c Response.result -> 'a Lwt.t) ->
  unit -> 'a Lwt.t
(**
   [ periodic_update call error update ] will execute [ call ] and apply [ update ] to the result or call [error] with the error code and message.

   If [update] or [error] returns [ `KeepGoing ], [`Retry] or [`Delay d], then [ periodic_update call update ] will be called again. The delay before recal depends on the argument :
   - If [`Retry] is returned, the recall is immediate
   - If [`Delay d] is returned, the recall will occur in [d].
   - [ update ] can return [ `KeepGoing], in this case, the recall will occur according to the [ cachedUntil ] field. [ error ] can return [`KeepGoing] only if ~delay is provided, if it's not, the recall will stop.

   If [update] or [error] returns anything else, the thread returns immediately with this result.

   the [call], [error]  and [update] functions should raise exceptions in the Lwt way.
   The optionnal argument [delay] allows to overwrite the normal delay provided by the [ cacheUntil ] field.
   The thread returned can be canceled.
*)

val handle_error :
  ?log:(string -> unit) -> int * string ->
  [> `Delay of CalendarLib.Calendar.Period.t
  | `Stop | `Retry ]


(** {2 APIs} *)

type entity = Apidsl.entity = {name : string ; id : int }

type walletJournal =
  {
    date : CalendarLib.Calendar.t ;
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

  open Eliom_content.Xml
  val ally :   ?api:string -> [< `T32 | `T64 | `T128 ]                 -> int -> uri
  val corp :   ?api:string -> [< `T32 | `T64 | `T128 | `T256 ]         -> int -> uri
  val char :   ?api:string -> [< `T32 | `T64 | `T128 | `T256 | `T512 ] -> int -> uri
  val typ :    ?api:string -> [< `T32 | `T64 ]                         -> int -> uri
  val render : ?api:string -> [< `T32 | `T64 | `T128 | `T256 | `T512 ] -> int -> uri
end


(** {2 Specific modules} *)

module Wallet : module type of Wallet
