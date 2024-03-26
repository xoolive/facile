(* $Id: facile.ml,v 1.26 2007-07-26 13:02:59 barnier Exp $ *)

module Data = Fcl_data
module Cstr = Fcl_cstr
module Reify = Fcl_reify
module Alldiff = Fcl_alldiff
module Debug = Fcl_debug
module Goals = Fcl_goals
module Sorting = Fcl_sorting
module Boolean = Fcl_boolean
module Expr = Fcl_expr
module Arith = Fcl_arith
module Domain = Fcl_domain
module Interval = Fcl_interval
module Stak = Fcl_stak
module FdArray = Fcl_fdArray
module Misc = Fcl_misc
module Var = Fcl_var
module Gcc = Fcl_gcc
module Binary =
  struct
    type algo = AC3 | AC6 | AC6_3 | AC6_4 |  AC6_5 | AC6_6 | AC6_T
		| AC6_S | AC6_S2

    let filter v1 v2 ng =
      let sng = Fcl_misc.sort_unique ng in
      List.fold_right
        (fun ((i, j) as ij) r ->
          if Fcl_var.Fd.member v1 i && Fcl_var.Fd.member v2 j then ij :: r
          else r)
        sng []

(* + unsafe ? *)
    let cstr ?(algo = AC6) ?(nogoods = true) ?(unsafe = false) var1 var2 ng =
      let ng = if unsafe then ng else filter var1 var2 ng in
      match algo with
	AC6 ->
          let ng = if nogoods then ng else Fcl_ac6.compl var1 var2 ng in
          Fcl_ac6.cstr var1 var2 ng
      |	AC6_3 ->
          let ng = if nogoods then ng else Fcl_ac6.compl var1 var2 ng in
          Fcl_ac6.cstr3 var1 var2 ng
      | AC6_4 ->
          let size_doms = Fcl_var.Fd.size var1 * Fcl_var.Fd.size var2 in
          let size_pairs = List.length ng in
          (*Printf.printf "AC6_4: %d/%d\n%!" size_pairs (size_doms / 2);*)
          let (forbidden, pairs) =
            if size_doms <= 1 || size_pairs < size_doms / 2 then (nogoods, ng)
            else begin
              (*Printf.printf "switch\n%!";*)
              let cng = Fcl_ac6.compl var1 var2 ng in
              (not nogoods, cng) end in
          Fcl_ac6.cstr4 ~forbidden var1 var2 pairs
      | AC6_5 ->
          let goods = if nogoods then Fcl_ac6.compl var1 var2 ng else ng in
          Fcl_ac6.cstr5 var1 var2 goods
      | AC6_6 ->
          let goods = if nogoods then Fcl_ac6.compl var1 var2 ng else ng in
          Fcl_ac6.cstr6 var1 var2 goods
      | AC6_T ->
          let goods = if nogoods then Fcl_ac6.compl var1 var2 ng else ng in
          Fcl_ac6.cstr_tight var1 var2 goods
      | AC6_S ->
          let goods = if nogoods then Fcl_ac6.compl var1 var2 ng else ng in
          Fcl_ac6.cstr_soft var1 var2 goods
      | AC6_S2 ->
          let goods = if nogoods then Fcl_ac6.compl var1 var2 ng else ng in
          Fcl_ac6.cstr_soft_noid var1 var2 goods
      | AC3 ->
          let ng = if nogoods then ng else Fcl_ac6.compl var1 var2 ng in
          Fcl_binary.cstr var1 var2 ng

    let reduce = Fcl_ac6.reduce
  end
module Opti = Fcl_opti
module Conjunto = Fcl_conjunto
module SetDomain = Fcl_setDomain
module Invariant = Fcl_invariant
module Easy = struct
  let i2e = Arith.i2e
  let fd2e = Arith.fd2e
  let ( +~ ) = Arith.( +~ )
  let ( *~ ) = Arith.( *~ )
  let ( -~ ) = Arith.( -~ )
  let ( /~ ) = Arith.( /~ )
  let ( **~ ) = Arith.( **~ )
  let ( %~ ) = Arith.( %~ )
  let ( <=~ ) = Arith.( <=~ )
  let ( <~ ) = Arith.( <~ )
  let ( >~ )  = Arith.( >~ )
  let ( =~ )  = Arith.( =~ )
  let ( <>~ ) = Arith.( <>~ )
  let ( >=~ ) = Arith.( >=~ )
  let ( <=~~ ) = Arith.( <=~~ )
  let ( <~~ )  = Arith.( <~~ )
  let ( >~~ )  = Arith.( >~~ )
  let ( =~~ )  = Arith.( =~~ )
  let ( <>~~ ) = Arith.( <>~~ )
  let ( >=~~ ) = Arith.( >=~~ )
  let (&&~~) = Reify.(&&~~)
  let (||~~) = Reify.(||~~)
  let (=>~~) = Reify.(=>~~)
  let (<=>~~) = Reify.(<=>~~)
  let ( &&~ ) = Goals.( &&~ )
  let ( ||~ ) = Goals.( ||~ )
  module Fd = Var.Fd
  type ('a, 'b) concrete' = ('a, 'b) Var.concrete = Unk of 'a | Val of 'b
  type concrete_fd = (Domain.t, Fd.elt) concrete'
end
