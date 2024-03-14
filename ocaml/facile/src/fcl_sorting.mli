(* $Id: fcl_sorting.mli,v 1.11 2003-02-03 15:50:48 brisset Exp $ *)

(** Sorting Constraint *)

val sort : Fcl_var.Fd.t array -> Fcl_var.Fd.t array
  (** [sort a] returns an array of variables constrained to be the variables
     in [a] sorted in increasing order. *)

val sortp : Fcl_var.Fd.t array -> Fcl_var.Fd.t array * Fcl_var.Fd.t array
  (** [sortp a] same as [sort] but returns a couple [(sorted, perm)]
     where [sorted] is the array of sorted variables and [perm] is an
     array of variables constrained to be the permutation between [a] and
     [sorted], i.e. [a.(i) = sorted.(perm.(i))]. *)

val cstr : Fcl_var.Fd.t array -> ?p:Fcl_var.Fd.t array option -> Fcl_var.Fd.t array -> Fcl_cstr.t
(** [cstr a (?perm:None) sorted] returns the constraint ensuring that
   [sorted] is the result of sorting array [a] according to the
   permutation [perm]. [perm] default value is [None], meaning the
   argument is irrelevant. Raises [Invalid_argument] if
   arrays have incompatible length. Not reifiable. *)
