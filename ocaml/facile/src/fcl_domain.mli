(* $Id: fcl_domain.mli,v 1.54 2011-03-10 16:37:24 barnier Exp $ *)

(** Domain Operations *)

(** This module provides functions to create
   and handle domains, which are useful to build variables and perform
   propagation (i.e. domain filtering). *)

type elt = int
(** Type of element of domains (for generic interface,
   {% see~\ref{moduletype:Var.ATTR}%}). *)

type t
(** Type of finite domains of integers (functional: no in-place
   modifications, domains can be shared). Standard equality and
   comparison can be used on domains. *)

(** {% \subsection{Building New Domains} %} *)

val empty : t
(** The empty domain. *)

val create : elt list -> t
(** [create l] builds a new domain containing the values of [l]. Removes
   duplicates and sorts values. Returns [empty] if [l] is
   empty. *)

val unsafe_create : elt list -> t
(** [unsafe_create l] builds a new domain containing the values of [l]. [l] must
be sorted and must not contain duplicate values, otherwise the behaviour is
unspecified. Returns [empty] if [l] is empty. It is a more efficient variant
of [create]. *)

val interval : elt -> elt -> t
(** [interval inf sup] returns the domain of all integers in the closed
    interval [[inf..sup]]. Raise [Invalid_argument] if [inf > sup]. *)

val elt : elt -> t
(** [elt x] returns a bound domain with element [x]. *)

val int : t
(** The largest representable domain. Handy to create variables for which
    bounds cannot be previously known. It is actually much smaller
    than [interval min_int max_int] (which really is the biggest one) to
    try to prevent overflows while computing bounds of expressions
    involving such variables. *)

val boolean : t
(** The domain containing [0] and [1]. *)

(** {% \subsection{Access} %} *)

val is_empty : t -> bool
(** [is_empty d] tests whether the domain [d] is empty or not. *)

val is_bound : t -> bool
(** [is_bound d] tests whether the domain [d] is bound (instantiated)
   or not. *)

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
(** [size d] returns the number of integers in [d]. *)

val min : t -> elt
val max : t -> elt
(** [min d] (resp. [max d]) returns the lower (resp. upper) bound of [d].
   If [d] is empty, the behaviour is unspecified (incorrect return value
   or exception raised). *)

val min_max : t -> elt * elt
(** [min_max d] returns both the lower and upper bound of [d]. If [d] is empty,
   the behaviour is unspecified (incorrect return value or exception
   raised). *)

val iter : (elt -> unit) -> t -> unit
(** [iter f d] successively applies function [f] to all element of [d] in
   increasing order. *)

val interval_iter : (elt -> elt -> unit) -> t -> unit
(** [interval_iter f d] successively applies function [f] to the bounds
   of all the disjoint intervals of [d] in increasing order. E.g. a
   suitable function [f] to print a domain can be defined as
   [fun mini maxi -> Printf.printf "%d..%d " mini maxi]. *)

val interval_fold_right : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a
(** [interval_fold_right f d a] is similar to [List.fold_right] applied
   to the intervals of [d]. *)

val interval_fold_left : ('a -> elt -> elt -> 'a) -> 'a -> t -> 'a
(** [interval_fold_left f a d] is similar to [List.fold_left] applied
   to the intervals of [d]. *)

val arg_exists_opt : (elt -> bool) -> t -> elt option
(** [arg_exists_opt pred d] returns the first element of [d] satisfying [p]
    or [None] otherwise. *)

val interval_list : t -> (elt * elt) list
(** [interval_list d] returns the list of intervals of domain [d]. *)

val member : t -> elt -> bool
(** [member d n] tests if [n] belongs to [d]. *)

val mem : elt -> t -> bool

val values : t -> elt list
(** [value d] returns the list of values of the domain [d] *)

val fprint_elt : out_channel -> elt -> unit
val fprint : out_channel -> t -> unit
(** Pretty printing of elements and domains. *)

val to_string : t -> string
(** [to_string d] returns a string representation of [d]. *)

val included : t -> t -> bool
(** [included d1 d2] tests whether domain [d1] is included in domain [d2]. *)

val smallest_geq : t -> elt -> elt
val greatest_leq : t -> elt -> elt
(** [smallest_geq dom val] (resp. [greatest_leq dom val]) returns the
    smallest (resp. greatest) value in [dom] greater (resp. smaller) than
    or equal to [val]. Raises [Not_found] if [max dom < val] (resp.
    [min dom > val]). *)

val largest_hole_around : t -> elt -> elt * elt
(** [largest_hole_around dom val] returns the largest hole (interval)
    in [dom] centred around [val]. Returns [(val, val)] if [val]
    belongs to [dom] and raises [Not_found] if [val] is outside
    [dom] bounds. Equivalent to
    [(greatest_leq dom val, smallest_geq dom val)] but faster. *)

val smallest_common_elt_greater_than : t -> t -> elt -> elt option
(** [smallest_common_elt_greater_than d1 d2 lb] returns the smallest
    element in the intersection of [d1] and [d2] greater than [lb],
    or [None] if there is none. *)

val nearest : t -> elt -> elt
(** [nearest dom val] returns the value in [dom] nearest to [val]. *)

val choose : (elt -> elt -> bool) -> t -> elt
(** [choose ord d] returns the mininum value of [d] for order [ord].
    Raises [Not_found] if [d] is empty. *)

val random : t -> elt
(** [random d] returns one random element of [d] *)

(** {% \subsection{Operations} %} *)

val add : elt -> t -> t
(** [add n d] returns [d] {% $\cup$%} [{n}]. *)

val remove : elt -> t -> t
(** [remove n d] returns [d] {% $\setminus$ %} [{n}]. Returns [d] if [n]
    is not in [d]. *)

val remove_up : elt -> t -> t
val remove_low : elt -> t -> t
(** [remove_up n d] (resp. [remove_low n d]) returns
    [d] {% $\setminus$ %} [[n+1..max_int]] (resp.
    [d] {% $\setminus$ %} [[min_int..n-1]]), i.e. removes values
    stricly greater (resp. less) than [n]. *)

val remove_low_up : elt -> elt -> t -> t
(** [remove_low_up low up d] is a shortcut for
    [remove_up up (remove_low low d)]. *)

val remove_closed_inter : elt -> elt -> t -> t
(** [remove_closed_inter inf sup d] returns
    [d] {% $\setminus$ %} [[inf..sup]], i.e. removes
    values greater than or equal to [inf] and less or equal to [sup] in [d].
    Returns [d] if [inf > sup]. *)

val remove_min : t -> t
val remove_max : t -> t
(** [remove_min d] (resp. [remove_max d]) returns [d] without its lower
    (resp. upper) bound. Raises [Invalid_argument] if [d] is empty. *)

val intersection : t -> t -> t
val union : t -> t -> t
(** Intersection (resp. union) on domains. *)

val difference : t -> t -> t
(** [difference big small] returns [big] {% $\setminus$ %} [small].
    [small] must be included in [big], otherwise the behaviour is
    unspecified (incorrect return value or exception raised). *)

val diff : t -> t -> t
(** [diff d1 d2] returns [d1] {% $\setminus$ %} [d2], i.e. domain of
    elements in [d1] which are not in [d2]. *)

val minus : t -> t
(** [minus d] returns the domain of opposite values of [d]. *)

val plus : t -> elt -> t
(** [plus d n] translates a domain by [n]. *)

val times : t -> elt -> t
(** [times d k] expands a domain by factor [k]. *)

val compare : t -> t -> elt
(** [compare d1 d2] is a comparison function working first on the cardinal,
    then (if [d1] and [d2] have the same size) on the lexicographic order
    of the domains (expressed in extension). *)

val compare_elt : elt -> elt -> elt
(** [compare_elt e1 e2] is a comparison function on elements of domains.
    Convenient to use the [Domain] module as a functor argument as in
    module [Var]{% ~\ref{module:Var}%}. *)

val disjoint : t -> t -> bool
(** [disjoint d1 d2] tests whether [d1] and [d2] are disjoint. *)


(**/**)
val strictly_inf : elt -> elt -> bool
