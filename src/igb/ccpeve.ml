open Js

class type not_trusted = object 
  method openEveMail : unit meth
  method showInfo : int32 -> int32 -> unit meth 
  method showInfo_typeID : int32 -> unit meth 
  method showRouteTo : int32 -> unit meth
  method showRouteTo_withorig : int32 -> int32 -> unit meth
  method showMap : unit meth
  method showMap_ontarget : int32 -> unit meth
  method showPreview : int32 -> unit meth
  method showFitting : js_string t -> unit meth
  method showContract : int32 -> int32 -> unit meth
  method showMarketDetails : int32 -> unit meth
  method requestTrust : js_string t -> unit meth
end

(* class type trusted = object *)
(*   inherit not_trusted *)
(*   method setDestination *)
(*   method addWaypoint *)
(*   method joinChannel *)
(*   method joinMailingList *)
(*   method createContract *)
(*   method sellItem *)
(*   method buyType *)
(*   method findInContracts *)
(*   method addToMarketQuickBar *)
(*   method showContents *)
(*   method addContact *)
(*   method addCorpContact *)
(*   method removeContact *)
(*   method removeCorpContact *)
(*   method block *)
(*   method inviteToFleet *)
(*   method startConversation *)
(*   method addBounty *)
(*   method showContracts *)
(*   method showOnMap *)
(*   method editMember *)
(*   method awardDecoration *)
(*   method sendMail *)
(*   method bookmark *)
(*   method clearAllWaypoints *)
(* end *)

let ccpeve : not_trusted t = (Unsafe.variable "CCPEVE") 
