(* $Id: fcl_cstr.mli,v 1.49 2005-12-15 13:40:56 barnier Exp $ *)

(** {1 Posting Constraints and Building New Ones} *)

(** This module defines the type [t] of constraints and functions to create
  and post constraints: mainly a [create] function which allows to build new
  constraints from scratch (this function is not needed when using
  standard FaCiLe predefined constraints) and the mostly useful [post]
  function which must be called to actually add a constraint to the
  constraint store.
*)

(** {2 Basic} *)

exception DontKnow
(**  Exception raised by the [check] function of a reified constraint when it
   is not known whether the constraint is satisfied or violated. *)

type priority
(**  Type of waking priority. *)

val immediate : priority
val normal : priority
val later : priority
val even_later : priority
(**  Available priorities:
- immediate: as soon as possible, for quick updates;
- normal: standard priority;
- later: for time consuming constraints (e.g. [Gcc.cstr], [Alldiff.cstr]...).

*)


type t
(**  The type of constraints. *)

val create : ?name:string -> ?nb_wakings:int -> ?fprint:(out_channel -> unit) -> ?priority:priority -> ?init:(unit -> bool) -> ?check:(unit -> bool) -> ?not:(unit -> t) -> ?freevars:(unit -> Fcl_domain.t) -> ?tightness:(unit -> float) -> (int -> bool) -> (t -> unit) -> t
(**  [create ?name ?nb_wakings ?fprint ?priority ?init ?check ?not update delay]
   builds a new constraint:
-  [name] is a describing string for the constraint. Default value
   is ["anonymous"].
-  [nb_wakings] is the number of calls to [Var.delay] with distinct
   "[waking_id]" arguments {% (see~\ref{val:Var.BASICVAR.delay})%} within the
   constraint own [delay] function (see below). Default value is [1].
   Beware that if [nb_wakings] is greater than 1 and the optional [init]
   argument is not provided, [init] default behaviour is to do nothing
   (i.e. the [update] function will not be called).
-  [fprint] should print the constraint on an output channel taken as
   its only argument. Default value is to print the [name] string.
-  [priority] is either [immediate], [normal] or [later]. Time costly
   constraints should be waken after quick ones. Default value is [normal].
-  [init] is useful to perform initialization of auxiliary data
   structures needed and maintained by the [update] function.
   [init ()] is called as soon as the constraint is posted. [init]
   must return a boolean indicating whether the constraint was solved
   or not by this initial propagation phase, like [update] does. Default
   value is to call [(update 0)] if [nb_wakings] is equal to 1 to
   perform this initial propagation; if [nb_wakings] is greater than 1,
   default value is [fun () -> false], i.e. it does nothing. Hence, a
   specific [init] argument must be provided if this is not the desired
   behaviour.
-  [check] must be specified if the constraint is to be reifiable
   (as well as the [not] function). When the constraint is reified,
   [check ()] is called to verify whether the constraint is satisfied
   or violated, i.e. the constraint itself or its negation is entailed
   by the constraint store. It should return [true] if the constraint
   is satisfied, [false] if it is violated and raise [DontKnow] when
   it is not known. [check] {b must not} change the domains of the
   variables involved in the constraint.
   Default: [Failure] exception is raised.
-  [not] must be specified if the constraint is reifiable (as well
   as [check]). [not ()] should return a constraint which is the
   negation of the constraint being defined. When the constraint is
   reified, it is called to post the negation of the constraint whenever
   [check ()] return [false], i.e. the negation is entailed by the
   constraint store. Default: [Failure] exception is raised.
-  [update] is a mandatory argument which propagates the constraint,
   i.e. filters domains and checks consistency. This function takes an
   integer as its unique parameter, according to the optional
   [waking_id] argument given to the [Var.delay] calls featured in the
   constraint own [delay] function (see below). When a waking event
   occurs, this function is called with the corresponding integer
   "[waking_id]", and must return [true] when the constraint is
   (partially) satisfied {e for this event}, [false] if further
   propagations have to be performed, and raise [Stak.Fail]
   whenever an inconsistency is detected. The whole
   constraint is solved when [update 0], ..., [update (nb_wakings-1)]
   have all returned [true]. E.g. a global constraint on an array
   of variables can be aware of which variable has triggered the
   awakening by providing the integer index of the variable as its
   "[waking_id]" to the [Var.delay] function. [update] is called with
   [0] by default when the [nb_wakings] argument has been omitted; in
   this case, the constraint is solved as soon as [update] returns [true].
-  [delay] schedules the awakening of the constraint [ct] (which is
   taken as its unique argument), i.e. the execution of its [update]
   function. If [update id] should be called (because it may propagates)
   when one of the events contained in the events
   {% (see~\ref{val:Var.ATTR.on-underscorerefine})%} list [es] occurred
   on variable [v], then [Var.delay es v ~waking_id:id ct] should be called
   within the body of the [delay] function. Beware that
   {b all the "[waking_id]s" must be contiguous integers ranging from}
   [0] {b to} [nb_wakings-1], otherwise the behaviour is unspecified.
   [delay] is a mandatory argument.

*)

val post : t -> unit
(**  [post c] posts the constraint [c] to the constraint store. *)

val one : t
val zero : t
(**  The constraint which succeeds (resp. fails) immediately. *)

(** {2 Access} *)

val id : t -> int
(**  [id c] returns a unique integer identifying the constraint [c]. *)

val name : t -> string
(**  [name c] returns the name of the constraint [c]. *)

val priority : t -> priority
(**  [priority c] returns the priority of the constraint [c]. *)

val fprint : out_channel -> t -> unit
(**  [fprint chan c] prints the constraint [c] on channel [chan]. Calls
   the [fprint] function passed to [create]. *)

val is_solved : t -> bool
(**  [is_solved c] returns [true] if [c] is satisfied and [false] otherwise. *)

val nb_wakings : t -> int

val weight : t -> int
(** [weight c] returns the weight of constraint [c], i.e. the number of
    time it has been violated. *)

val freevars : t -> Fcl_domain.t
(** [freevars c] returns the set (represented as a domain) of free
    variables indices of constraint [c] (if supported by [c], otherwise
    an internal error is raised). *)

val tightness : t -> float
(** [tightness c] returns the dynamic tightness (number of nogoods over
    the size of the cardinal product of the variables) of constraint [c]
    (if supported by [c] - currently binary constraints only -
    otherwise an internal error is raised). Relevant only when both variables
    are unbound. *)

val active_store : unit -> t list
(**  [active_store ()] returns the list of all active constraints, i.e. whose
   [update] functions have returned [false]. *)

val not : t -> t
(** [not c] returns the negation of [c]. *)

(**/**) (* Following values are undocumented *)

(**  An object with wakable constraints *)
type event
val new_event : unit -> event
val schedule : event -> unit
val register : event -> ?waking_id:int -> t -> unit
val registered : event -> (t * int) list
(**  Returns ALL constraints *)

val delay : event list -> ?waking_id:int -> t -> unit
(**  [delay event_list c] suspends constraint [c] on all the events in
   [event_list]. *)

val conjunction : t list -> t
(**  Posts a conjunction of constraints when posted ([one] if the list
     is empty). Not reifiable. *)

val reset_queue : unit -> unit
val assert_empty_queue : unit -> unit
(**  _Undocumented_
     [reset_queue ()] reset the constraint queue. *)

val wake_all : unit -> unit
(**  _Undocumented_
     [wake_all ()] wake all constraints respecting priority order. *)

val init : t -> unit
(**  _Undocumented_
     [init c] post the constraint deamon [c] (no wake and no add call). *)

val self_delay : t -> (t -> unit)
val self_init : t -> (unit -> unit)
val check : t -> unit -> bool
