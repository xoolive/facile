(* $Id: fcl_interval.mli,v 1.12 2004-07-26 15:55:25 barnier Exp $ *)

(** Variable Membership to an Interval *)

val is_member : Fcl_var.Fd.t -> int -> int -> Fcl_var.Fd.t
  (** [is_member v inf sup] returns a boolean variable which will
     be instantiated to [1] if [v] is in [inf..sup] and to [0] otherwise. *)

val cstr : Fcl_var.Fd.t -> int -> int -> Fcl_var.Fd.t -> Fcl_cstr.t
  (** [cstr v inf sup b] returns a constraint ensuring that the boolean
     variable [b] is instantiated to [1] if [v] is in [inf..sup] and to
     [0] otherwise.
     Not reifiable. *)

