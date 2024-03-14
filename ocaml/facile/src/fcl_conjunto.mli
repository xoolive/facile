(* $Id: fcl_conjunto.mli,v 1.9 2004-08-09 14:45:41 barnier Exp $ *)

(** Constraints on Finite Sets *)

val subset : Fcl_var.SetFd.t -> Fcl_var.SetFd.t -> Fcl_cstr.t
(** [subset v1 v2] ensures that [v1] is a subset of [v2]. Not reifiable. *)

val cardinal : Fcl_var.SetFd.t -> Fcl_var.Fd.t
(** [cardinal v] returns the cardinal (an integer variable) of the set [v]. Not reifiable. *)

val smallest : Fcl_var.SetFd.t -> Fcl_var.Fd.t
(** [smallest v] returns the smallest element (an integer variable) of [v]. *)

val union : Fcl_var.SetFd.t -> Fcl_var.SetFd.t -> Fcl_var.SetFd.t
val inter : Fcl_var.SetFd.t -> Fcl_var.SetFd.t -> Fcl_var.SetFd.t
(** Operations on sets. *)

val all_disjoint : Fcl_var.SetFd.t array -> Fcl_cstr.t
(** [all_disjoint vars] ensures that all the set variables of array [vars]
   are pairwise disjoint. Not reifiable. *)

val disjoint : Fcl_var.SetFd.t -> Fcl_var.SetFd.t -> Fcl_cstr.t
(** [disjoint v1 v2] defined by [all_disjoint [|v1; v2|]]. Not reifiable. *)

val inside : int -> Fcl_var.SetFd.t -> unit
val outside : int -> Fcl_var.SetFd.t -> unit
(** Basic refinements for labeling. *)

val mem : Fcl_var.Fd.t -> Fcl_var.SetFd.t -> Fcl_cstr.t
(** [mem x v] states that [x] belongs to [v]. *)

val inf_min : Fcl_var.SetFd.t -> Fcl_var.SetFd.t -> Fcl_cstr.t
(** [inf_min v1 v2] ensures that the minimal element
   of [v1] is less than or equal to the minimal element of [v2]. The empty set
   is smaller than any other set. Useful to break permutation symmetries for
   a set of set variables. Not reifiable. *)

val order : Fcl_var.SetFd.t -> Fcl_var.SetFd.t -> Fcl_cstr.t
(** [order v1 v2] ensures that [v1] is less than or equal to [v2]
   according to [Domain.compare] {% (see~\ref{val:Domain.compare})%}.
   Caution: [order] builds the cardinal
   variables of [v1] and [v2]; if they are already available, please use
   [order_with_card]. Not reifiable. *)

val order_with_card : Fcl_var.SetFd.t -> Fcl_var.Fd.t -> Fcl_var.SetFd.t -> Fcl_var.Fd.t -> Fcl_cstr.t
(** [order_with_card v1 card1 v2 card2] is equivalent to [order] but the cardinals
   of the variables must be provided too. Useful to sort a set of variables. *)

val member : Fcl_var.SetFd.t -> Fcl_setDomain.elt list -> Fcl_cstr.t
(** [member v l] ensures that [v] will have a value in [l]. Not reifiable. *)

val sum_weight : Fcl_var.SetFd.t -> (int * int) list -> Fcl_var.Fd.t
(** [sum_weight v weights] returns an integer variable equal to the sum
   of the weights associated with the value in [v]. [weights] must be a
   list of pairs [value, weight)] that associates a unique weight to each
   value possibly in [v]. All the weights must be positive integers. *)

(**/**)
val atmost1 : Fcl_var.SetFd.t array -> int -> unit
(** [atmost1 sets card] states that [sets] must have cardinality [card]
    and must intersect pairwise in atmost one element *)
