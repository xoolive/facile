(* $Id: fcl_invariant.mli,v 1.3 2007-08-01 15:59:17 barnier Exp $ *)

(** Backtrackable Invariant References *)

(** This module provides types and functions to create
   and handle Backtrackable Invariant References (noted BIR in the sequel).
   BIRs are single-valued variables whose values are restored
   upon backtracks and which are linked by "one-way" constraints.
   They maintain functional dependencies between "source" {e setable}
   BIRs (initialized with their creation value) and {e unsetable} BIRs
   built upon them. BIRs may be convenient to automatically handle
   heuristic criteria or the data structures of local search
   algorithms {% ~\cite{localizer97}%}. Note that circular dependencies
   are not allowed by the typing policy. *)

type ('a, 'b) t
type setable
type unsetable
type 'a setable_t = ('a, setable) t
type 'a unsetable_t = ('a, unsetable) t
(** Type of BIRs. Parameter ['a] stands for the
   type of the value of the BIR. Parameter ['b] is [setable] if the BIR
   can be assigned, [unsetable] if not. [setable_t] and [unsetable_t] are
   shortcuts. *)

(** {% \subsection{Creation and access} %} *)

val create : ?name:string -> 'a -> 'a setable_t
(** [create ~name:"" v] returns a setable BIR with initial content [v].
   An optional string can be given to name the BIR. *)

val constant : ?name:string -> 'a -> 'a unsetable_t
(** [constant ~name:"" cst] returns an unsetable BIR with initial
   content [cst]. An optional string can be given to name the BIR. *)

val set : 'a setable_t -> 'a -> unit
(** Assignment of a setable BIR. *)

val get : ('a, 'b) t -> 'a
(** Access to the content of a BIR. *)

val id : ('a, 'b) t -> int
(** [id r] returns a unique integer associated to BIR [r]. *)

val name : ('a, 'b) t -> string
(** [name r] returns the name (specified or generated) of BIR [r]. *)

val fprint : out_channel -> ?printer:(out_channel -> 'a -> unit) -> ('a, 'b) t -> unit
(** [fprint c ~printer:(fun _ _ -> ()) r] prints BIR [r] on channel [c].
   An optional custom printer can be given to display the value of [r]. *)

(** {% \subsection{Operations: generic, arithmetic, arrays} %} *)

val unary : ?name:string -> ('a -> 'b) -> (('a, 'c) t -> 'b unsetable_t)
(** [unary ~name:"Invariant.unary" f] wraps the unary function [f] into
   an operator on BIRs. An optional string can be given to name the
   operator. *)

val binary : ?name:string -> ('a -> 'b -> 'c) -> (('a, 'd) t -> ('b, 'e) t -> 'c unsetable_t)
(** [binary ~name:"Invariant.binary" f] wraps the binary function [f]
   into an operator on BIRs.
   An optional string can be given to name the operator. *)

val ternary : ?name:string -> ('a -> 'b -> 'c -> 'd) -> (('a, 'e) t -> ('b, 'f) t -> ('c, 'g) t -> 'd unsetable_t)
(** [ternary ~name:"Invariant.ternary" f] wraps the ternary function [f]
   into an operator on BIRs.
   An optional string can be given to name the operator. *)

val sum : (int, 'a) t array -> int unsetable_t
(** [sum a] returns a BIR equal to the sum of elements of [a]. *)

val prod : (int, 'a) t array -> int unsetable_t
(** [prod a] returns a BIR equal to the product of elements of [a]. *)

val sum_float : (float, 'a) t array -> float unsetable_t
(** [sum_float a] returns a BIR equal to the sum of elements of [a]. *)

val prod_float : (float, 'a) t array -> float unsetable_t
(** [prod_float a] returns a BIR equal to the product of elements of [a]. *)


module Array : sig

  val get : ('a, 'b) t array -> (int, 'c) t -> 'a unsetable_t
(** [get a i] returns the BIR element number [i] of array [a]. *)

  val argmin : ('a, 'b) t array -> ('a -> 'c) -> int unsetable_t
(** [argmin a c] returns the BIR index of the minimum BIR value of [a] for
   criterion [c]. *)

  val min : ('a, 'b) t array -> ('a -> 'c) -> 'a unsetable_t
(** [min a c] returns the minimum BIR value of [a] for criterion [c]. *)
end


(** {% \subsection{From domain variables to BIRs} %} *)

(** Generic signature. *)
module type FD = sig

  type fd
  (** Type of a finite domain variable. *)

  type elt
  (** Type of elements in the domain. *)

  val min : fd -> elt unsetable_t
  val max : fd -> elt unsetable_t
  val size : fd -> int unsetable_t
  val is_var : fd -> bool unsetable_t
  (** BIR variants of [Fd.Var] access functions. *)

  val unary : ?name:string -> (fd -> 'a) -> fd -> 'a unsetable_t
  (** [unary ~name:"Invariant.XxxFd.unary" f v] Wrapper of any access
     function over [fd] type. *)
end

module Fd : FD
with
  type fd = Fcl_var.Fd.t
  and type elt = Fcl_var.Fd.elt
(** Module for accessing finite integer domain variables with BIRs. *)

module SetFd : FD
with
   type fd = Fcl_var.SetFd.t
   and type elt = Fcl_var.SetFd.elt
(** Module for accessing set domain variables with BIRs. *)
