(** {1 Arithmetic Expressions and Constraints} *)

(** {% \paragraph{Overview} %} *)
(** This module provides functions and operators to build arithmetic
   expressions and state (in/dis)equation constraints on them.
*)

(** {2 Basics} *)

type t
(** Type of arithmetic expressions over variables of type [Var.Fd.t] and
   integers. *)

(*** Conversion *)
val i2e : int -> t
(** [i2e n] returns an expression which evaluates to [n]. *)

val fd2e : Fcl_var.Fd.t -> t
(** [fd2e v] returns an expression which evaluates to [n] if the
    variable [v] is instantiated to [n]. *)

val e2fd : t -> Fcl_var.Fd.t
(** [e2fd e] creates and returns a new variable [v] and posts the constraint
    [fd2e v =~ e]. *)

(** {2 Construction of Arithmetic Expressions} *)

(**
   {b Only} if compiled in bytecode (using [facile.cma]),
   the arithmetic operators check whether any integer overflow
   (i.e. the result of an arithmetic operation on integers is
   less than [min_int] or greater than [max_int]) occurs during
   constraints internal computations and raise an assert failure.
   Arithmetic operations are taken modulo {% $2^{31}$%} otherwise
   (or {% $2^{63}$%} on 64-bit processors, see the OCaml reference
   manual{% ~\cite{ocaml}%}), thus incomplete failures may happen
   with native code compiled programs. *)

val ( +~ ) : t -> t -> t
val ( -~ ) : t -> t -> t
val ( *~ ) : t -> t -> t
(** Addition, substraction, multiplication on expressions. *)

val ( **~ ) : t -> int -> t
(** Exponentiation of an expression to an integer value. *)

val ( /~ ) : t -> t -> t
val ( %~ ) : t -> t -> t
(** Division and modulo on expressions. The exception [Division_by_zero]
   is raised whenever the second argument evaluates to 0. *)

val abs : t -> t
(** Absolute value on expressions. *)

val sum : t array -> t
val sum_fd : Fcl_var.Fd.t array -> t
(** [sum exps] (resp. [sum_fd vars]) returns the sum of all the elements of an
   array of expressions (resp. variables). Returns an expression that evaluates
   to 0 if the array is empty. *)

val scalprod : int array -> t array -> t
val scalprod_fd : int array -> Fcl_var.Fd.t array -> t
(** [scalprod coeffs exps] (resp. [scalprod_fd coeffs vars]) returns the
   scalar product of an array of integers and an array of expressions
   (resp. variables).
   Returns an expression that evaluates to 0 if both arrays are empty.
   Raises [Invalid_argument] if the arrays don't have the same length. *)

val prod : t array -> t
val prod_fd : Fcl_var.Fd.t array -> t
(** [prod exps] (resp. [prod_fd vars]) returns the product of all the
   elements of an array of expressions (resp. variables).
   Returns an expression that evaluates to 1 if the array is empty. *)


(** {2 Access} *)

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


(** {2 Arithmetic Constraints on Expressions} *)

(**
   FaCiLe processes arithmetic constraints to try to simplify and factorize
   common subexpressions. Furthermore, auxilliary variables are created to
   handle non-linear expressions and substituted to the original terms.
   So printing an arithmetic constraint may produce something quite
   different from the user's input. *)


val (<~)  : t -> t -> Fcl_cstr.t
val (<=~) : t -> t -> Fcl_cstr.t
val (=~)  : t -> t -> Fcl_cstr.t
val (>=~) : t -> t -> Fcl_cstr.t
val (>~)  : t -> t -> Fcl_cstr.t
val (<>~)  : t -> t -> Fcl_cstr.t
(** Strictly less, less or equal, equal, greater or equal,
   strictly greater and different constraints on expressions. *)

val shift : Fcl_var.Fd.t -> int -> Fcl_var.Fd.t
(** [shift x d] returns a finite domain variable constrained to be
   equal to [x+d]. *)

(** {2 Reification} *)

(**
   The following operators are  shortcuts to lighten the writing of reified
   expressions. They replace the corresponding constraint by an expression
   equal to a boolean variable that is instantiated to [1] when the constraint is
   satisfied and to [0] if it is violated.
   See module [Reify] {% \ref{module:Reify}%}. *)

(** [e1 op~~ e2] is equivalent to [fd2e (Reify.boolean (e1 op~ e2))]. *)

val (<~~)  : t -> t -> t
val (<=~~) : t -> t -> t
val (=~~)  : t -> t -> t
val (>=~~) : t -> t -> t
val (>~~)  : t -> t -> t
val (<>~~)  : t -> t -> t
(** Reified strictly less, less or equal, equal, greater or equal,
   strictly greater and different. *)

(** {2 Boolean sums setting}

   FaCiLe tries to automatically optimize the processing of
   boolean (0-1 variables) sums whenever their sizes are large enough. *)

val get_boolsum_threshold : unit -> int
(** Returns the minimum size for boolean sums optimization. (Default: 5) *)

val set_boolsum_threshold : int -> unit
(** Set the minimum size for boolean sums optimization.
   [boolsum_threshold max_int] disables it. *)
