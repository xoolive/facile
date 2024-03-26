type operator = LessThan | Equal | Diff

val min_max_plus_inter : int -> int -> int -> int -> (int * int)
val min_max_minus_inter : int -> int -> int -> int -> (int * int)

val cstr : ?boolsum:int ->
  (int * Fcl_var.Fd.t) list -> operator -> int -> Fcl_cstr.t
(* [cstr (?boolsum:int) coef_vars op d] returns the linear constraint
   [sum coef_vars op d] and automatically optimizes boolean sums larger
   than [boolsum] variables (default: 5). *)

val linear_aux : (int * Fcl_var.Fd.t) list -> int -> Fcl_var.Fd.t
val shift_cstr : Fcl_var.Fd.t -> Fcl_var.Fd.t  -> int -> Fcl_cstr.t
(** [shift_cstr y x d] returns the constraint [y = x+d] *)

val get_boolsum_threshold : unit -> int
(** Returns the minimum size for boolean sums optimization. *)

val set_boolsum_threshold : int -> unit
(** Set the minimum size for boolean sums optimization.
   [boolsum_threshold max_int] disables it. *)
