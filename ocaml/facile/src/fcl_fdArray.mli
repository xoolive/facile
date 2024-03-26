(* $Id: fcl_fdArray.mli,v 1.16 2007-07-26 13:00:11 barnier Exp $ *)

(** Constraints over Arrays of Variables *)

val min : Fcl_var.Fd.t array -> Fcl_var.Fd.t
val max : Fcl_var.Fd.t array -> Fcl_var.Fd.t
(** [min vars] (resp. [max vars]) returns a variable constrained to be equal
   to the variable that will be instantiated to the minimal (resp. maximal)
   value among all the variables in the array [vars]. Raises
   [Invalid_argument] if [vars] is empty. Not reifiable. *)

val min_cstr : Fcl_var.Fd.t array -> Fcl_var.Fd.t -> Fcl_cstr.t
val max_cstr : Fcl_var.Fd.t array -> Fcl_var.Fd.t -> Fcl_cstr.t
(** [min_cstr vars mini] (resp. [max_cstr vars maxi]) returns the constraint
   [fd2e (min vars) =~ fd2e mini] (resp. [fd2e (max vars) =~ fd2e maxi]).
   Raises [Invalid_argument] if [vars] is empty. Not reifiable. *)

val get : Fcl_var.Fd.t array ->  Fcl_var.Fd.t -> Fcl_var.Fd.t
(** [get vars index] returns a variable constrained to be equal to
   [vars.(index)]. Variable [index] is constrained within the range of
   the valid indices of the array [(0..Array.length vars - 1)]. Raises
   [Invalid_argument] if [vars] is empty.
   Not reifiable. *)

val get_cstr : Fcl_var.Fd.t array -> Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_cstr.t
(** [get_cstr vars index v] returns the constraint
   [fd2e vars.(index) =~ fd2e v]. Variable [index] is constrained within
   the range of the valid indices of the array [(0..Array.length vars - 1)].
   Raises [Invalid_argument] if [vars] is empty. Not reifiable. *)

val sum : Fcl_var.Fd.t array -> Fcl_var.Fd.t
(** [sum vars] returns a variable constrained to be equal to the
   sum of the variables of array [vars]. *)

val sum_cstr : Fcl_var.Fd.t array -> Fcl_var.Fd.t -> Fcl_cstr.t
(** [sum_cstr vars s] returns the constraint [fd2e s =~ sum_fd vars]. *)
