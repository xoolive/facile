(* $Id: fcl_ac6.mli,v 1.9 2004-09-08 17:07:52 barnier Exp $ *)

(* {1 Extensive Binary Constraints Filtering with AC6 Algorithm} *)

val cstr : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t
(* [cstr var1 var2 nogoods] returns the constraint forbidding the integer
   couples contained in [nogoods] to be solution for the variables pair
   ([var1], [var2]) using the AC6 algorithm. Not reifiable. *)

val cstr3 : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t

val cstr4 : ?forbidden:bool -> Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t

val cstr5 : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t
(* [cstr5 var1 var2 goods] returns the constraint forbidding the integer
   couples not contained in [goods] to be solution for the variables pair
   ([var1], [var2]) using the AC6 algorithm. Not reifiable. *)

val cstr6 : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t
(* [cstr6 var1 var2 goods] *)

val cstr_soft : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t
(* [cstr_soft var1 var2 goods] returns a reifiable binary constraint. *)

val cstr_soft_noid : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t
(* [cstr_soft_noid var1 var2 goods] returns a reifiable binary constraint. *)

val cstr_tight : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> Fcl_cstr.t
(* As [cstr6] but maintaining tightness. *)

val compl : Fcl_var.Fd.t -> Fcl_var.Fd.t -> (int * int) list -> (int * int) list
(* [compl var1 var2 pairs] returns the complementary of the set of couples
   [pairs] in the cross-product of the domains of [var1] and [var2]. *)

val reduce : Fcl_domain.t array -> (int * int * (int * int) list) list
  -> Fcl_domain.t array
      * (int * int * (int * int) list) list
      * Fcl_domain.t list
(* [reduce doms pairs] returns a triple [(rdoms, rpairs, ccs)] where
   [rdoms] are the reduced [doms], [rpairs] the reduced [pairs] and [ccs]
   the set of connected components. Pairs on the same variables are merged,
   interchangeable value are removed and domains and pairs are filtered
   accordingly. *)
