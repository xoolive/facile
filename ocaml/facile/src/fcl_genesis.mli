(* $Id: fcl_genesis.mli,v 1.5 2004-09-08 17:09:35 barnier Exp $ *)

(* {1 Uniform random binary CSP generation} *)

val urbcsp : int -> int -> int -> int -> (int * int * (int * int) list) list
  (* _Undocumented_
     [urbcsp nbvar sizedom cstrd tight] return the specifications of a
     uniform random binary CSP with [nbvar] variables whose domain size is
     [sizedom], with a constraindness of [cstrd]% (density of the constraint
     graph) and a tightness (density of each constraint) of [tight]%.
     The return value is a list of triples [(i, j, l)], [0<=i,j<nbvar] such
     that [l] is the list of nogoods (forbidden value couples) for variables
     [(vi,vj)]. *)
