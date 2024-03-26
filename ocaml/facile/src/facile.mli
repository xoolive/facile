(* $Id: facile.mli,v 1.112 2011-03-17 15:57:08 barnier Exp $ *)

(* Module [Facile]: end-user interface to the FaCiLe library *)

module Domain : sig

  (** @inline *)
  include module type of Fcl_domain with type t = Fcl_domain.t
                                     and type elt = Fcl_domain.elt
end

module SetDomain : sig

  (** @inline *)
  include module type of Fcl_setDomain with type t = Fcl_setDomain.t
                                        and type elt = Fcl_setDomain.elt
end

module Stak : sig

  (** @inline *)
  include module type of Fcl_stak with type level = Fcl_stak.level
                                   and type 'a ref = 'a Fcl_stak.ref
end

module Data : sig

  (** @inline *)
  include module type of Fcl_data with type ('a, 'b) Hashtbl.t = ('a, 'b) Fcl_data.Hashtbl.t
end

module Cstr : sig

  (** @inline *)
  include module type of Fcl_cstr with type priority = Fcl_cstr.priority
                                   and type t = Fcl_cstr.t
                                   and type event = Fcl_cstr.event
end

module Var : sig

  (** @inline *)
  include module type of Fcl_var with type ('a, 'b) concrete = ('a, 'b) Fcl_var.concrete
                                  and type Fd.t = Fcl_var.Fd.t
                                  and type Fd.domain = Fcl_var.Fd.domain
                                  and type Fd.elt = Fcl_var.Fd.elt
                                  and type Fd.size = Fcl_var.Fd.size
                                  and type Fd.event = Fcl_var.Fd.event
                                  and type SetFd.t = Fcl_var.SetFd.t
                                  and type SetFd.domain = Fcl_var.SetFd.domain
                                  and type SetFd.elt = Fcl_var.SetFd.elt
                                  and type SetFd.size = Fcl_var.SetFd.size
                                  and type SetFd.event = Fcl_var.SetFd.event
end

module Reify : sig (** @inline *) include module type of Fcl_reify end

module Alldiff : sig

  (** @inline *)
  include module type of Fcl_alldiff with type algo = Fcl_alldiff.algo
end

module Binary : sig

  type algo = AC3 | AC6 | AC6_3 | AC6_4 | AC6_5 | AC6_6 | AC6_T | AC6_S | AC6_S2

  val reduce : Domain.t array -> (int * int * (int * int) list) list
               -> Domain.t array * (int * int * (int * int) list) list * Domain.t list

  val cstr : ?algo:algo -> ?nogoods:bool -> ?unsafe:bool -> Var.Fd.t -> Var.Fd.t -> (int * int) list -> Cstr.t

end

module Goals : sig

  (** @inline *)
  include module type of Fcl_goals with type t = Fcl_goals.t
                                    and type order = Fcl_goals.order
                                    and type bb_mode = Fcl_goals.bb_mode
end

module Sorting : sig (** @inline *) include module type of Fcl_sorting end
module Boolean : sig (** @inline *) include module type of Fcl_boolean end
module Expr : sig (** @inline *) include Fcl_expr.S end

module Arith : sig

  (** @inline *)
  include module type of Fcl_arith with type t = Fcl_arith.t
end

module Invariant : sig

  (** @inline *)
  include module type of Fcl_invariant
                         with type ('a, 'b) t = ('a, 'b) Fcl_invariant.t
                          and type setable = Fcl_invariant.setable
                          and type unsetable = Fcl_invariant.unsetable
                          and type 'a setable_t = 'a Fcl_invariant.setable_t
                          and type 'a unsetable_t = 'a Fcl_invariant.unsetable_t
                          and type Fd.fd = Fcl_invariant.Fd.fd
                          and type Fd.elt = Fcl_invariant.Fd.elt
                          and type SetFd.fd = Fcl_invariant.SetFd.fd
                          and type SetFd.elt = Fcl_invariant.SetFd.elt
end

module Interval : sig (** @inline *) include module type of Fcl_interval end
module FdArray : sig (** @inline *) include module type of Fcl_fdArray end

module Gcc : sig

  (** @inline *)
  include module type of Fcl_gcc with type level = Fcl_gcc.level
end

module Opti : sig

  (** @inline *)
  include module type of Fcl_opti with type mode = Fcl_opti.mode
end

module Conjunto : sig (** @inline *) include module type of Fcl_conjunto end

module Easy : sig
  val i2e : int -> Arith.t
  val fd2e : Var.Fd.t -> Arith.t
  val ( +~ ) : Arith.t -> Arith.t -> Arith.t
  val ( *~ ) : Arith.t -> Arith.t -> Arith.t
  val ( -~ ) : Arith.t -> Arith.t -> Arith.t
  val ( /~ ) : Arith.t -> Arith.t -> Arith.t
  val ( **~ ) : Arith.t -> int -> Arith.t
  val ( %~ ) : Arith.t -> Arith.t -> Arith.t
  val ( <=~ ) : Arith.t -> Arith.t -> Cstr.t
  val ( <~ ) : Arith.t -> Arith.t -> Cstr.t
  val ( >~ ) : Arith.t -> Arith.t -> Cstr.t
  val ( =~ ) : Arith.t -> Arith.t -> Cstr.t
  val ( <>~ ) : Arith.t -> Arith.t -> Cstr.t
  val ( >=~ ) : Arith.t -> Arith.t -> Cstr.t
  val ( <=~~ ) : Arith.t -> Arith.t -> Arith.t
  val ( <~~ ) : Arith.t -> Arith.t -> Arith.t
  val ( >~~ ) : Arith.t -> Arith.t -> Arith.t
  val ( =~~ ) : Arith.t -> Arith.t -> Arith.t
  val ( <>~~ ) : Arith.t -> Arith.t -> Arith.t
  val ( >=~~ ) : Arith.t -> Arith.t -> Arith.t
  val ( &&~~) : Cstr.t -> Cstr.t -> Cstr.t
  val ( ||~~) : Cstr.t -> Cstr.t -> Cstr.t
  val ( =>~~) : Cstr.t -> Cstr.t -> Cstr.t
  val ( <=>~~) : Cstr.t -> Cstr.t -> Cstr.t
  val ( &&~ ) : Goals.t -> Goals.t -> Goals.t
  val ( ||~ ) : Goals.t -> Goals.t -> Goals.t
  module Fd : Var.FD with
           type t = Var.Fd.t and
           type domain = Domain.t and
           type elt = Domain.elt and
           type size = int and
           type event = Var.Fd.event
  type ('a, 'b) concrete' = ('a, 'b) Var.concrete = Unk of 'a | Val of 'b
  type concrete_fd = (Domain.t, Fd.elt) concrete'
end
