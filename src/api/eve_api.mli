(** Api spectification and access. *)

module Response :
sig
  exception Wrong of string
  type 'a t = {
    version : string;
    currentTime : string;
    cachedUntil : string;
    data : 'a;
  }
end

type cache = Long | Short | MShort

val tq : string

val test : string

type apikey = Apidsl.apikey
val apikey : keyId:int -> vCode:string -> apikey

type charkey = Apidsl.charkey

val charkey : keyId:int -> vCode:string -> charId:int -> charkey

type enc_param = (string * string) list

type ('extract, 'auth, 'param, 'out) api =
  ('extract, 'auth, 'param, 'out) Apidsl.api

val apply_api :
  string -> ('a, 'b, 'c, 'd) api -> 'b -> 'c -> 'd Response.t Lwt.t

(** {2 APIs} *)

type date = string

type entity = Apidsl.entity = {name : string ; id : int }

type walletJournal =
  {
    date : date ;
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

(** {2 Specific modules} *)

module Wallet : module type of Wallet
