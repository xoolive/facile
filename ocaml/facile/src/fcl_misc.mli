(* $Id: fcl_misc.mli,v 1.14 2005-12-09 12:36:25 barnier Exp $ *)

(* Module [Misc]: straightforwardly eponymous *)

val last_and_length : 'a list -> 'a * int
(* _Undocumented_
   Returns the last element as well as the size of a list. Used by
   [Fcl_domain.make]. *)
val gen_int_fun : unit -> (unit -> int)
(* _Undocumented_
   Returns a function generating unique integers (modulo [max_int - min_int]).
   Used to generate identification keys (increasing from 0). *)
val arg_min_array : ('a -> 'b) -> 'a array -> (int * 'b)
val arg_max_array : ('a -> 'b) -> 'a array -> (int * 'b)
(* _Undocumented_
   [arg_min_array f a] (resp. [arg_max_array f a]) returns the index of
   the first element of [a] that minimizes (resp. maximizes) [f] and
   the corresponding optimal value. *)
val sort_unique : 'a list -> 'a list
(* _Undocumented_
   [sort_unique l] returns [l] sorted with duplicates removed. *)
val int_overflow : float -> bool
  (*  _Undocumented_
     [int_overflow x] returns [true] iff [float max_int < x] or
     [float min_int > x], [false] otherwise. Used in Operators and
     [Fcl_arith.expn_int]. *)

module Operators : sig
  val (=+) : int ref -> int -> unit
  val (=+.) : float ref -> float -> unit
(*  _Undocumented_
   [x =+ n] C-like increment operator. Equivalent to [x := !x+n]. *)

  val min : int -> int -> int
(* _Undocumented_
   Non-polymorphic [min] over integers. For optimization purpose. *)

  val max : int -> int -> int
(* _Undocumented_
   Non-polymorphic [max] over integers. For optimization purpose. *)

  val ( * ) : int -> int -> int
  val (+) : int -> int -> int
  val (-) : int -> int -> int
  (* _Undocumented_
     Standard integer arithmetic operators with overflow checking raising an
     assert failure. Disabled if compiled with the -noassert flag. Used
     in [Fcl_arith]. *)

  val sign : int -> int
  val ( /+ ) : int -> int -> int
  val ( /- ) : int -> int -> int
  (* _Undocumented_
     Used within arithmetic modules *)
end

val iter : ('a -> 'a) -> int -> 'a -> 'a
  (* _Undocumented_
     [iter f n z] computes [(f (f ... n] times [... (f z)))]. Used in
     Fcl_arith.( **~). *)

val goedel : (int -> 'a -> 'a) -> int -> 'a -> 'a
  (* _Undocumented_
     [godel f n z] computes [(f (n-1) (f (n-2) ... n] times [... (f 0 z)))].
     Used in [Fcl_gcc]. *)

val protect : string -> (unit -> 'a) -> 'a
  (* _Undocumented_
     [protect name f] calls [f] and controls that it is
     not called inside itself. Raises an exception using [name] if it is
     the case. Example: let my_fun my_arg = protect "my_fun" (fun () -> ...).
  *)
