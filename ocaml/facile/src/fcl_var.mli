(* $Id: fcl_var.mli,v 1.38 2006-01-30 15:48:49 allignol Exp $ *)

(** {1 Constrained, Attributed, Finite Domain Variables} *)

type ('a, 'b) concrete = Unk of 'a | Val of 'b
(** Concrete type of the value of finite domain variables. *)

module type BASICVAR = sig
  type t
  (** Type of finite domain variable. *)

  type domain
  (** Type of domains. *)

  type elt
  (** Type of elements of domains. *)

  type size
  (** Type of the size of a domain. *)

  type event
  (** Type of domain reduction events. *)

  (** {2 Creation} *)

  val create : ?name:string -> domain -> t
  (** [create ?name d] returns a new variable with domain [d]. If provided,
      [name] will be used by the pretty printer. *)

  val interval : ?name:string -> elt -> elt -> t
  (** [interval ?name inf sup] returns a new variable with domain [[inf..sup]].
      If provided, [name] will be used by the pretty printer.*)

  val array : ?name:string -> int -> elt -> elt -> t array
  (** [array n inf sup] returns an array of [n] new variables with domain
      [[inf..sup]]. If provided, [name] (suffixed with the index of the element)
      will be used by the pretty printer. *)

  val elt : elt -> t
  (** [elt e] returns a new variable instantiated to value [e]. *)

  (** {2 Access} *)

  val is_var : t -> bool
  (** [is_var v] returns [true] if [v] is not instantiated and [false]
      otherwise. *)

  val is_bound : t -> bool
  (** [is_bound v] returns [true] if [v] is instantiated and [false]
      otherwise. *)

  val value : t -> (domain, elt) concrete
  (** [value v] returns [Val n] if [v] is instantiated to [n], [Unk a] otherwise
      where [a] is the attribute of [v]. Should always be used in a matching:
      [match value v with Val n -> ... | Unk a -> ...]. *)

  val dom : t -> domain
  val min : t -> elt
  (** [min v] returns the lower bound of [v]. *)

  val max : t -> elt
  (** [max v] returns the upper bound of [v]. *)

  val min_max : t -> elt * elt
  (** [min_max v] returns both the lower and upper bounds of [v]. *)

  val elt_value : t -> elt
  (** [elt_value v] returns the value of [v] if it is instantiated and raises
      a [Failure] exception otherwise. *)

  val size : t -> size
  (** [size v] returns the number of integer values in the domain of [v]
      ([1] if [v] is instantiated). *)

  val member : t -> elt -> bool
  (** [member v n] returns [true] if [n] belongs to the domain of [v] and
      [false] otherwise. *)

  val id : t -> int
  (** [id v] returns a unique integer identifying the attribute associated
      with [v]. Must be called only on non ground variable, raise [Failure]
      otherwise. *)

  val name : t -> string
  (** [name v] returns the name of variable [v] (the empty string if
      it was not provided while created). Must be called only on non ground
      variable, raise [Failure] otherwise. *)

  val compare : t -> t -> int
  (** Compares two variables. Values (bound variables) are smaller than
      unknowns (unbound variables). Unknowns are sorted according to
      their attribute [id]. *)

  val equal : t -> t -> bool
  (** Tests if two variables are equal with respect to [compare]. *)

  val active_store : unit -> t list
  (** [active_store ()] returns the list of unbound variables. *)

  val fprint : out_channel -> t -> unit
  (** [fprint chan v] prints variable [v] on channel [chan]. *)

  val fprint_array : out_channel -> t array -> unit
  (** [fprint_array chan vs] prints array of variables [vs] on channel [chan]. *)

  val to_string : t -> string
  (** [to_string v] returns a string representation of variable [v]. *)

  val constraints_number : t -> int
  (** [constraints_number v] returns the number of different constraints
      attached to [v]. *)

  val wdeg : t -> int
  (** [wdeg a] returns the weighted degree of attribute [a]. *)

  val twdeg : t -> int
  (** [twdeg a] returns the true weighted degree of attribute [a], i.e. not
      taking constraints that have less than one fee variables into account. *)

  val tightness : t -> float
  (* Experimental / Not yet implemented *)


  (** {2 Refinement} *)

  val unify : t -> elt -> unit
  (** [unify v n] instantiates variable [v] with integer value [n]. Raises
      [Fcl_stak.Fail] in case of failure. [unify] may be called either on unbound
      variables or on instantiated variables. *)

  val refine : t -> domain -> unit
  (** [refine v d] reduces the domain of [v] with domain [d]. [d] must be
      included in the domain of [v], otherwise the behaviour is
      unspecified (corrupted system or exception raised). *)

  val refine_low : t -> elt -> unit
  (** [refine_low v inf] reduces the domain of [v] by cutting all values
      strictly less than [inf]. *)

  val refine_up : t -> elt -> unit
  (** [refine_up v sup] reduces the domain of [v] by cutting all values
      strictly greater than [sup]. *)

  val refine_low_up : t -> elt -> elt -> unit
  (** [refine_low_up v inf sup] reduces the domain of [v] by cutting all values
      strictly less than [inf] and greater than [sup]. Robust even if [v]
      is already bound (checks that [inf] <= [v] <= [sup], otherwise fails). *)

  (** {2 Events and suspending} *)

  val on_refine : event
  (** Event occuring when a variable is changed, i.e. its domain modified. *)

  val on_subst : event
  (** Event occuring when a variable is instantiated. *)

  val on_min : event
  val on_max : event
  (** Event occuring when the lower (resp. upper) bound of a variable decreases. *)

  val delay : event list -> t -> ?waking_id:int -> Fcl_cstr.t -> unit
  (** [delay event_list v ~waking_id:id c] suspends constraint [c] on all
      the events in [event_list] occurring on [v]. An optional integer
      [id] may be associated to the wakening: it must be unique and range
      from 0 to [nb_wakings-1], [nb_wakings] being the argument of [Cstr.create]
      specifying the number of calls to [delay] with distinct [waking_id]
      arguments. These integers are arguments to the "update" function of
      constraints and aim at discriminating waking events to fire the
      appropriate propagation rule. [waking_id] default value is 0.
      This function has no effect on instantiated variables (as no event
      could occur on a ground variable). *)

  (**/**)

  val int : elt -> t
  val subst : t -> elt -> unit
  (** [subst v n] instantiates variable [v] with integer value [n]. Raises
      [Fcl_stak.Fail] in case of failure. Must be called only on unbound
      (not instantiated) variable, otherwise a [Failure] exception is raised. *)

  val unify_cstr : t -> elt -> Fcl_cstr.t
end
(** Common variables module signature. *)

(** Extended signature for finite domain variable (with added functions
    irrelevant to set or float variables). *)
module type FD = sig
  include BASICVAR

  val remove : t -> elt -> unit
  (** [remove v a] removes [a] from the domain of [v]. Leaves the domain
      unchanged if [a] does not belong to it. *)

  val values : t -> elt list
  (** [values v] returns the list of all integers in the domain of [v]. If
      [v] is instantiated to [n], returns the singleton list containing [n]. *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f v] iterates f on each integer in the domain of [v]. *)

  val vars2ids : t list -> Fcl_domain.t
end

module Fd : FD with
         type domain = Fcl_domain.t
       and type elt = Fcl_domain.elt
       and type size = int
(** Concrete finite domain variable module. *)

module SetFd : BASICVAR
       with
         type domain = Fcl_setDomain.t
       and type elt = Fcl_setDomain.S.t
       and type size = int
(** Concrete integer set variable module. *)

module type DOMAIN = sig
  type elt
  type t
  val is_empty : t -> bool
  val is_bound : t -> bool
  val elt_value : t -> elt
  val dom_changed : t -> t -> bool
  val min_changed : t -> t -> bool
  val max_changed : t -> t -> bool
  val compare_elt : elt -> elt -> int
  val fprint_elt : out_channel -> elt -> unit
  val fprint : out_channel -> t -> unit
  val to_string : t -> string
  val min : t -> elt
  val max : t -> elt
  val min_max : t -> elt * elt
  val mem : elt -> t -> bool
  val interval : elt -> elt -> t
  val elt : elt -> t
  val remove_low : elt -> t -> t
  val remove_up : elt -> t -> t
  val remove_low_up : elt -> elt -> t -> t
  val included : t -> t -> bool
  type size
  val size : t -> size
  val strictly_inf : elt -> elt -> bool
                                     (* [strictly_inf x1 x2] checks if [x1 < x2] when it is already known that [x1 <= x2] *)
end

module MakeVar : functor (Domain : DOMAIN) -> BASICVAR with type domain = Domain.t and type elt = Domain.elt and type size = Domain.size
