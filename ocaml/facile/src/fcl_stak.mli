(* $Id: fcl_stak.mli,v 1.26 2007-07-26 13:14:24 barnier Exp $ *)

(** {1 Global Stack of Goals, Backtrackable Operations} *)

(** This module provides functions to control the execution of the goal
   stack, as well as {e backtrackable references}, i.e. mutable data
   structures restored on backtracking. Nota: the module name
   [Stak] lacks a '[c]' because of a possible clash with the OCaml
   standard library module [Stack]. *)

(** {2 Access} *)

type level
(** Type of a level in the stack. *)

val older : level -> level -> bool
(** [older l1 l2] true if level [l1] precedes [l2]. *)

val size : unit -> int
(** Size of the stack, i.e. number of trailings. *)

val depth : unit -> int
(** Depth of the stack, i.e. number of active levels. *)

val level : unit -> level
(** Returns the current level. *)

val levels : unit -> level list
(** Returns the current active levels. *)

val nb_choice_points : unit -> int
(** Access to a global counter incremented at each choice point.
   Useful to implement search strategies such as Limited Discrepancy
   Search{% ~\cite{harvey95.lds}.%} *)


(** {2 Control} *)

exception Level_not_found of level
(** Raised by [cut]. *)

val cut : level -> unit
  (** [cut l] cuts the choice points left on the stack until level [l].
     Raise [Level_not_found] if level is not reached and stack is empty. *)

exception Fail of string
(** Raised during solving whenever a failure occurs. The string argument
   is informative. *)

val fail : string -> 'a
(** [fail x] equivalent to [raise (Fail x)]. *)


(** {2 Backtrackable References} *)

type 'a ref
(** Backtrackable reference of type ['a]. I.e. type of mutable
   data structures restored on backtracking. *)

val ref : 'a -> 'a ref
(** Returns a reference whose modifications will be trailed during the
   solving of a goal. *)

val set : 'a ref -> 'a -> unit
(** Sets a backtrackable reference. *)

val get : 'a ref -> 'a
(** Dereference. *)

val incr : int ref -> unit
(** [incr r] increments reference r. *)

val decr : int ref -> unit
(** [decr r] decrements reference r. *)


(**/**)

type gl = {
    name : string;
    call : unit -> gl option
  }
(** Concrete type of goals. Hidden in Facile. *)

exception Empty_stack
(** _Undocumented_ *)

val reset : unit -> unit
(** Empty the stack. *)

val save : gl list -> level
(** Push a choice point on the stack. *)

val backtrack : unit -> gl list
  (** _Undocumented_
     Pop a success continuation. May raise Empty_stack. *)

val backtrack_all : unit -> unit
  (** _Undocumented_
     Pop the whole stack. *)

val trail : (unit -> unit) -> unit
  (** _Undocumented_
     [trail undo] Push the closure [undo] on the stack. The closure will
     be called when poped from the stack. *)

val cut_bottom : level -> unit
  (** _Undocumented_
     Raise Level_not_found if level is not reached and stack is empty. *)

val unsafe_set : 'a ref -> 'a -> unit
(** _Undocumented_
   Unbacktrackable modification. *)


val print_current_level : out_channel -> unit
(** _Undocumented_ *)
