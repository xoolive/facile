(* $Id: fcl_reify.mli,v 1.21 2007-07-26 13:08:19 barnier Exp $ *)

(** Constraints Reification *)

(** All the reification functions and operators only work on
   {b reifiable} constraints, i.e. constraints endowed with a [check] function
   {b and} a [not] function (or built-in constraints for which the
   documentation does not mention "Not reifiable"). Otherwise a [Failure]
   (fatal error) exception is raised. *)

val boolean : ?delay_on_negation:bool -> ?name:string -> Fcl_cstr.t -> Fcl_var.Fd.t
(** [boolean ~delay_on_negation:true ~name:"" c] returns a boolean (0..1)
   variable associated to the constraint [c]. The constraint is
   satisfied iff the boolean variable is instantiated to
   1. Conversely, its negation is satisfied iff it is instantiated to
   0. The waking conditions of the contraint relating [c] and the
   boolean variable are the ones set by the [delay] function of [c] (set by the
   [delay] argument of [Cstr.create]). If the optional argument
   [delay_on_negation] is set to [true], the new constraint is also
   attached to the events that awake the negation of [c] (i.e. the constraint
   returned by the [not] function of [c]), otherwise it is not.
   [delay_on_negation] default value is [true]. *)

val cstr : ?delay_on_negation:bool -> Fcl_cstr.t -> Fcl_var.Fd.t -> Fcl_cstr.t
(** [cstr ~delay_on_negation:true c b] is equivalent to the
    constraint [boolean ?delay_on_negation c =~ b].
    [delay_on_negation] default value is [true]. *)

(** {% \subsection{Operators} %} *)

val (&&~~) : Fcl_cstr.t -> Fcl_cstr.t -> Fcl_cstr.t
val (||~~) : Fcl_cstr.t -> Fcl_cstr.t -> Fcl_cstr.t
val (=>~~) : Fcl_cstr.t -> Fcl_cstr.t -> Fcl_cstr.t
val (<=>~~) : Fcl_cstr.t -> Fcl_cstr.t -> Fcl_cstr.t
val xor : Fcl_cstr.t -> Fcl_cstr.t -> Fcl_cstr.t
val not : Fcl_cstr.t -> Fcl_cstr.t
(** Logical reification operators on constraints, namely
   and, or, implies, equivalent, exclusive or, not. *)
