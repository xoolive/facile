(* $Id: fcl_opti.mli,v 1.17 2004-07-28 15:52:47 barnier Exp $ *)

(** Module [Opti]: Branch and Bound optimization *)

type mode = Restart | Continue

(** Deprecated: use [Goals.minimize] instead.

    [minimize goal cost ?control ?step ?mode solution] runs a Branch
    and Bound algorithm on [goal] for bound [cost], with an improvment
    of a least [step] between each solution found. With [mode] equals
    to [Restart] (default), the search restarts from the beginning for
    every step while with mode [Continue] the search simply carries on
    with an update of the cost constraint. [solution] is called with
    the instantiation value of [cost] as argument each time a solution
    is found. The result is the value returned by [solution] on the
    last solution, embedded in an option type ([Some sol]); if no solution
    is found, [None] is returned.  [?control] is passed to [Goals.solve] as
    its first (optional) argument. Default [control] does nothing
    (i.e. [fun _ -> ()]).  Default [step] is 1. [Invalid_argument] exception
    is raised if [step] is negative or null. *)
val minimize : Fcl_goals.t -> Fcl_var.Fd.t -> ?control:(int -> unit) -> ?step:int -> ?mode:mode -> (int -> 'a) -> 'a option
