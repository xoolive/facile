val min_of_absmod_inter : int -> int -> int -> int -> int
val max_of_absmod_inter : int -> int -> int -> int -> int
val diffsign : int -> int -> bool
val diffeqsign : int -> int -> bool
val udiffsign : int -> int -> bool

val min_max_mult_inter : int -> int -> int -> int -> (int * int)
val min_max_div_inter : int -> int -> int -> int -> (int * int)
val min_max_mod_inter : int -> int -> int -> int -> (int * int)
val min_max_abs_inter : int -> int -> (int * int)
val min_max_expn_inter : int -> int -> int -> (int * int)

val expn_int : int -> int -> int
(* [expn_int x n] computes [x^n]. *)

val monome : Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_cstr.t
val monome_aux : Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_var.Fd.t
val absolute : Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_cstr.t
val absolute_aux : Fcl_var.Fd.t -> Fcl_var.Fd.t
val division : Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_cstr.t
val division_aux : Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_var.Fd.t
val modulo : Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_cstr.t
val modulo_aux : Fcl_var.Fd.t -> Fcl_var.Fd.t -> Fcl_var.Fd.t
val expn : Fcl_var.Fd.t -> Fcl_var.Fd.t -> int -> Fcl_cstr.t
val expn_aux : Fcl_var.Fd.t -> int -> Fcl_var.Fd.t
