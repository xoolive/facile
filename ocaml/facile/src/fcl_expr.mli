module type S = sig
  (** Arithmetic Expressions over Variables of Type [Var.Fd.t] *)

  (** This module provides functions and operators to build arithmetic
      expressions. *)

  type t
  (** Type of arithmetic expressions over variables of type [Var.Fd.t] and
      integers. *)

  val fprint : out_channel -> t -> unit
  (** [fprint chan e] prints expression [e] on channel [chan]. *)

  val eval : t -> int
  (** [eval e] returns the integer numerical value of a fully instantiated
      expression [e]. Raises [Invalid_argument] if [e] is not instantiated. *)

  val min_of_expr : t -> int
  val max_of_expr : t -> int
  (** [min_of_expr e] (resp. [max_of_expr e]) returns the minimal (resp. maximal)
      possible value of expression [e]. *)

  val min_max_of_expr : t -> (int * int)
  (** [min_max_of_expr e] is equivalent to [(min_of_expr e, max_of_expr e)]. *)

end

type agg_op = Pluse | Multe
type bin_op = Dive | Mode
type un_op = Abse
type var = Var of Fcl_var.Fd.t | Aux of int
type t =
    Agg of agg_op * (int * t) list * int
  | Bin of bin_op * t * t
  | Un of un_op * t
  | Inte of int
  | Fde of var

include S with type t := t

val compare_expr : t -> t -> int
val compare_intexpr : (int * t ) -> (int * t ) -> int

val reduce : t -> t
(** [reduce e] normalizes expression [e]. *)

val constrain : t -> Fcl_linear.operator -> Fcl_cstr.t
(** [constrain e op] returns the constraint [e op = 0] and post
    intermediate constraints. *)
