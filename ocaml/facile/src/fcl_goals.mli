(* $Id: fcl_goals.mli,v 1.61 2010-07-22 13:01:57 barnier Exp $ *)

(** {1 Building and Solving Goals} *)


(** This module provides functions and operators to build goals that will
   control the search, i.e. mainly choose and instantiate variables.
*)


(** {2 Access} *)

type t
(** The type of goals. *)

val name : t -> string
(** [name g] returns the name of the goal [g]. *)

val fprint : out_channel -> t -> unit
(** [fprint chan g] prints the name of goal [g] on channel [chan]. *)

(** {2 Creation} *)

val fail : t
val success : t
  (** Failure (resp. success). Neutral element for the disjunction
     (resp. conjunction) over goals. Could be implemented as
     [create (fun () -> Stak.fail "fail")] (resp. [create (fun () -> ())]). *)

val atomic : ?name:string -> (unit -> unit) -> t
  (** [atomic ~name:"atomic" f] returns a goal calling function [f].
     [f] must take [()] as argument and return [()]. [name] default
     value is ["atomic"]. *)

val create : ?name:string -> ('a -> t) -> 'a -> t
  (** [create ~name:"create" f a] returns a goal calling [f a].
     [f] should return a goal (success to stop). [name]
     default value is ["create"]. *)

val create_rec : ?name:string -> (t -> t) -> t
  (** [create_rec ~name:"create_rec" f] returns a goal calling [f]. [f]
     takes the goal itself as argument and should return a goal
     (success to stop). Useful to write recursive goals. [name] default
     value is ["create_rec"]. *)


(** {2 Operators and Built-in Goals} *)

val (&&~) : t -> t -> t
val (||~) : t -> t -> t
  (** Conjunction and disjunction over goals. Note that these two operators
     do have the {b same priority}. Goals expressions must therefore be
     carefully parenthesized to produce the expected result. *)

val forto : int -> int -> (int -> t) -> t
val fordownto : int -> int -> (int -> t) -> t
  (** [forto min max g] (resp. [fordownto min max g]) returns the
     conjunctive iteration of goal [g] on increasing (resp. decreasing)
     integers from [min] (resp. [max]) to [max] (resp. [min]). *)

val once : t -> t
  (** [once g] cuts choice points left on goal [g]. *)

val sigma : ?domain:Fcl_domain.t -> (Fcl_var.Fd.t -> t) -> t
(** [sigma ~domain:Domain.int fgoal] creates the goal [(fgoal v)]
   where [v] is a new
   variable of domain [domain]. Default domain is the largest one. It can
   be considered as an existential quantification, hence the concrete
   notation [sigma] of this function (because existential quantification can be
   seen as a generalized  disjunction). *)

(** {3 Instantiation of Finite Domain Variables} *)

val unify : Fcl_var.Fd.t -> Fcl_var.Fd.elt -> t
(** [unify var x] instantiates variable [var] to [x]. *)

val indomain : Fcl_var.Fd.t -> t
(** Non-deterministic instantiation of a variable, by labeling its domain
   (in increasing order). *)

val assign : ?choose:(Fcl_domain.t -> Fcl_domain.elt) ->  Fcl_var.Fd.t -> t
(** Binary choice-point on a single assignment. *)

val random : Fcl_var.Fd.t -> t
(** Non-deterministic instantiation of a variable, by labeling its domain
   in random order. *)

val instantiate : (Fcl_domain.t -> Fcl_domain.elt) -> Fcl_var.Fd.t -> t
(** [instantiate choose var] Non-deterministic instantiation of a variable,
   by labeling its domain using the value returned by the [choose] function. *)

type order = Decr | Incr
val dichotomic : ?order:order -> Fcl_var.Fd.t -> t
(** [dichotomic (?order:Incr) var] Non-deterministic instantiation of
   variable [var], by dichotomic recursive exploration of its domain,
   according to order [order]. [order] default value is [Incr]. *)

(** {3 Instantiation of Set Variables} *)

module Conjunto : sig
  val indomain : Fcl_var.SetFd.t -> t
  (** Non-deterministic instantiation of set variables ([refine] of Gervet's
     Conjunto{% ~\cite{conjunto}%}). *)
end

(** {2 Operations on Array of Variables} *)

module Array : sig
  val foralli : ?select:('a array -> int) -> (int -> 'a -> t) -> 'a array -> t
  (** [foralli ?select g a] returns the conjunctive iteration
     of the application of goal [g] on the elements of array [a]
     and on their indices. The order is computed by the heuristic
     [?select] which must raise [Not_found] to terminate.
     Default heuristic is increasing order over indices. *)

  val forall : ?select:('a array -> int) -> ('a -> t) -> 'a array -> t
  (** [forall ?select g a] defined by [foralli ?select (fun _i x -> g x) a],
     i.e. indices of selected elements are not passed to goal [g]. *)

  val existsi : ?select:('a array -> int) -> (int -> 'a -> t) -> 'a array -> t
  (** [existsi ?select g a] returns the disjunctive iteration
     of the application of goal [g] on the elements of array [a]
     and on their indices. The order is computed by the heuristic
     [?select] which must raise [Not_found] to terminate.
     Default heuristic is increasing order over indices. *)

  val exists : ?select:('a array -> int) -> ('a -> t) -> 'a array -> t
  (** [exists ?select g a] defined by [existsi ?select (fun _i x -> g x) a],
     i.e. indices of selected elements are not passed to goal [g]. *)

  val choose_index : (Fcl_var.Fd.t -> Fcl_var.Fd.t -> bool) -> Fcl_var.Fd.t array -> int
  (** [choose_index order fds] returns the index of the best (minimun)
     free (not instantiated) variable in the array [fds] for the criterion
     [order]. Raises [Not_found] if all variables are bound (instantiated). *)

  val not_instantiated_fd : Fcl_var.Fd.t array -> int
  (** [not_instantiated_fd fds] returns the index of one element in [fds]
     which is not instantiated. Raises [Not_found] if all variables in array
     [fds] are bound. *)

  val labeling: Fcl_var.Fd.t array -> t
  (** Standard labeling, i.e. conjunctive non-deterministic instantiation of
     an array of variables. Defined as [forall indomain]. *)
end



(** {2 Operations on List of Variables} *)

module List : sig
  val forall : ?select:('a list -> 'a * 'a list) -> ('a -> t) -> 'a list -> t
  (** [forall ?select g [x1;x2;...;xn]] is [g x1 &&~ g x2 &&~ ... &&~ g xn],
     i.e. returns the conjunctive iteration of goal [g] on list [a]. *)

  val exists : ?select:('a list -> 'a * 'a list) -> ('a -> t) -> 'a list -> t
  (** [exists ?select g [x1;x2;...;xn]] is [g x1 ||~ g x2 ||~ ... ||~ g xn],
     i.e. returns the disjunctive iteration of goal [g] on list [a]. *)

  val member : Fcl_var.Fd.t -> int list -> t
(** [member v l] returns the disjunctive iteration of the instantiation of
   the variable [v] to the values in the integer list [l]. Defined by
   [fun v l -> exists (fun x -> create (fun () -> Fd.unify v x)) l]. *)

  val labeling: Fcl_var.Fd.t list -> t
  (** Standard labeling, i.e. conjunctive non-deterministic instantiation of
     a list of variables. Defined as [forall indomain]. *)
end

(** {2 Optimization} *)

type bb_mode = Restart | Continue | Dicho
(** Branch and bound mode. *)

val minimize : ?step:int -> ?mode:bb_mode -> t -> Fcl_var.Fd.t -> (int -> unit) -> t
(** [minimize ~step:1 ~mode:Continue goal cost solution] runs a
   Branch and Bound algorithm on [goal] for bound [cost], with an improvement
   of at least [step] between each solution found. Three modes are available :
   [Restart] and [Continue] (default) explore the cost domain monotonically,
   the former restarting from the root node of the search each time an
   improvement is found, while the latter simply carries on, only backtracking
   until a better cost may be found; the third mode, [Dicho], performs a
   dichotomic search, with restarts at each improvement. Each time a solution
   is found, the [solution] function is called with the instantiation value
   of [cost] (which {b must be instantiated} by [goal]) as argument; this
   function can therefore be used to store (e.g. in a reference) the current
   solution. Default [step] is 1. [minimize] {b always fails}. *)


(** {2 Search Strategy} *)

val lds : ?step:int -> t -> t
  (** [lds ~step:1 g] returns a goal which will iteratively search [g] with
     increasing limited discrepancy (see {% ~\cite{harvey95.lds}%}) by
     increment [step]. [step] default value is 1. *)


(** {2 Solving} *)

val on_choice_point : Fcl_cstr.event
(** Event occurring whenever a choice point is reach. *)

val solve : ?control:(int -> unit) -> t -> bool
  (** [solve ~control:(fun _ -> ()) g] solves the goal [g] and returns
     a success ([true]) or a
     failure ([false]). The execution can be precisely controlled
     with the [control] argument whose single argument is the number
     of bactracks since the beginning of the search. This function is called
     after every local failure:

   - it can raise [Stak.Fail] to force a failure of the search in the
     current branch (i.e. backtrack);

   - it can raise any other user exception to stop the search process;

   - it must return [unit] to continue the search; this is the default
     behavior. *)

(**/**)

val reset : unit -> unit
  (** _Undocumented_
     Resets the OR stack (it is not done by solve). *)
