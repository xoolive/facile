(* $Id: fcl_data.mli,v 1.3 2006-10-31 13:51:05 barnier Exp $ *)

(** {1 Bactrackable Data Structures} *)

(** This module provides "efficient" backtrackable data structures,
   i.e. with incremental setting and trailing. *)

module Array : sig
  val set : 'a array -> int -> 'a -> unit
  (** [set t i x] Backtrackable assignment of [t.(i)] with [x]. *)
end
(** Bactrackable arrays. *)

module Hashtbl : sig
  type ('a, 'b) t
  val create : int -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val persistent_add : ('a, 'b) t -> 'a -> 'b -> unit
  val find : ('a, 'b) t -> 'a -> 'b
  val mem : ('a, 'b) t -> 'a -> bool
  val remove : ('a, 'b) t -> 'a -> unit
  val replace : ('a, 'b) t -> 'a -> 'b -> unit
  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
end
(** Bactrackable hashtables.
   This module provides a subset of the hashtable interface
   of the OCaml standard library module Hashtbl (see {% ~\cite{ocaml}%}). *)

module type CONTAINER = sig
  type t
  val empty : t
  val add : int -> t -> t
  val remove : int -> t -> t
end

module type MEMOIZE = sig
  type 'a t
  type set
  val create : int -> 'a t
  val add : 'a t -> 'a -> int -> unit
  val bt_add : 'a t -> 'a -> int -> unit
  val mem : 'a t -> 'a -> bool
  val find : 'a t -> 'a -> set
  val find_opt : 'a t -> 'a -> set option
  val remove : 'a t -> 'a -> int -> unit
  val remove_isempty : 'a t -> 'a -> int -> bool
  val bt_remove : 'a t -> 'a -> int -> unit
  val bt_remove_isempty : 'a t -> 'a -> int -> bool
  val remove_all : 'a t -> 'a -> unit
  val bt_remove_all : 'a t -> 'a -> unit
  val iter : ('a -> set -> unit) -> 'a t -> unit
  val fold : ('a -> set -> 'c -> 'c) -> 'a t -> 'c -> 'c
end

module MakeMemoize : functor (C : CONTAINER) -> MEMOIZE with type set = C.t
(* Bactrackable memoize on containers of int. *)
