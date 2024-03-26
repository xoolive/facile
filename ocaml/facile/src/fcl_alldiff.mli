(* $Id: fcl_alldiff.mli,v 1.17 2004-09-08 17:07:03 barnier Exp $ *)

(** {1 The "All Different" Constraint}  *)

type algo = Lazy | Bin_matching of Fcl_var.Fd.event
val cstr : ?algo:algo -> Fcl_var.Fd.t array -> Fcl_cstr.t
(** [alldiff (?algo:Lazy) vars] States that the variables of [vars]
   are different from each other. The optional argument [algo]
   specifies the level of propagation.
   [Lazy]: waits for instantiation and removes the corresponding value
   from other domains.
   [Bin_matching c]: waits for event [c] (e.g. [Var.Fd.on_refine])
   and uses a bin matching algorithm to ensure the constraint is
   consistent. [algo] default value is [Lazy].
   Not reifiable. *)
