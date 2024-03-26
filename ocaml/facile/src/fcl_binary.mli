(* $Id: fcl_binary.mli,v 1.8 2004-09-08 17:08:35 barnier Exp $ *)

(** {1 Extensive binary constraints filtering with AC3 algorithm} *)

val cstr : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t
(** [cstr var1 var2 nogoods] returns the constraint forbidding the integer
    couples contained in [nogoods] to be solution for the variables pair
    ([var1], [var2]) using the AC3 algorithm. Not reifiable. *)
