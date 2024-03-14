(* $Id: fcl_setDomain.mli,v 1.3 2005-10-25 12:48:54 barnier Exp $ *)

(** {1 Integer Set Domain Operations} *)

(** Implementation of sets of integers.
   The signature is freely inspired by the Set module of the
   standard OCaml library. *)
module S : sig
  type t = Fcl_domain.t
  val empty: t
  val is_empty: t -> bool
  val mem: int -> t -> bool
  val add: int -> t -> t
  val singleton: int -> t
  val remove: int -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (int -> unit) -> t -> unit
  val cardinal: t -> int
  val elements: t -> int list
  val min_elt: t -> int
  val max_elt: t -> int
  val choose: t -> int
  val remove_up : int -> t -> t
  val remove_low : int -> t -> t
end

type elt = S.t
(** Type of elements of set domains. They are sets themselves. *)

type t
(** Type of finite domains of integer sets: a domain is a powerset lattice
of sets bounded by definite elements or {e glb} (Greater Lower Bound) and
possible elements or {e lub} (Lower Upper Bounds). The glb is a subset of
the lub. Note that the empty domain cannot be represented. *)

(** {2 Creation} *)

val elt_of_list : int list -> elt
(** Creates a set from a list of integers. *)

val interval : elt -> elt -> t
(** [interval glb lub] builds the domain of sets greater than [glb] and
smaller than [lub]. An [Invalid_argument] exception is raised if [glb]
is not a subset of [lub]. *)

val elt : elt -> t
(** [interval elt] returns a bound domain with element [elt]. *)

(** {2 Access and Operations} *)

val is_empty : t -> bool
val is_bound : t -> bool

val elt_value : t -> elt
(* [elt_value d] returns the single element corresponding to [d] iff [d]
   is bound. *)

val dom_changed : t -> t -> bool
val min_changed : t -> t -> bool
val max_changed : t -> t -> bool
(* [dom_changed old current] (resp. [min_changed old current] and
   [max_changed old current]) returns [true] whenever the domain
   (resp. the min of the domain, and the max of the domain)
   has changed between [old] and [current]. *)

type size = int
val size : t -> size
(** [size d] returns |glb(d)|-|lub(d)|+1, i.e. the height of the lattice,
not its number of elements, or 0 if the domain is empty. *)

val min : t -> elt
val max : t -> elt
val min_max : t -> elt * elt
(** Access to glb and lub. *)

val fprint_elt : out_channel -> elt -> unit
val fprint : out_channel -> t -> unit
(** Pretty printing of elements and domains. *)

val to_string : t -> string
(** [to_string d] returns a string représentation of domain [d]. *)

val mem : elt -> t -> bool
(** [mem x d] tests whether [x] belongs to the domain [d]. *)

val included : t -> t -> bool
(** [included d1 d2] tests whether the domain [d1] is included in [d2],
   i.e. glb([d2]) included in glb([d1]) and lub([d1]) included in lub([d2]). *)

val iter : (elt -> unit) -> t -> unit
(** Iteration on values of the domain. {b Exponential} with the [size]
   of the domain. *)

val values : t -> elt list
(** Returns values of a domain. {b Exponential} with the [size] of the
   domain. *)

(**/**)

val intersection : elt -> elt -> elt

val strictly_inf : elt -> elt -> bool

val compare_elt : elt -> elt -> int

val unsafe_interval : elt -> elt -> t
(** [glb] <= [lub] not checked. *)

val remove_up : elt -> t -> t
val remove_low : elt -> t -> t
val remove_low_up : elt -> elt -> t -> t
