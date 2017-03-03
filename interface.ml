open Facile
open Easy

(**
 * In the following externals, the first integer corresponds to the index in the
 * callback array.
 *)

external on_backtrack: int -> int -> unit = "ml_backtrack_callback"
external goal_atomic: int -> unit -> unit = "ml_atomic_callback"
external on_solution: int -> int -> unit = "ml_onsol_callback"
external assign_atomic: int -> Facile.Var.Fd.t -> unit = "ml_assign_atomic"
external strategy_cb: int -> Facile.Var.Fd.t -> Facile.Var.Fd.t -> bool =
  "ml_strategy_cb"

let _ =

  (**
   * All callback registrations
   *)

  (* Variable instantiation *)

  Callback.register "Fd.interval" (fun a b -> Fd.interval a b);
  Callback.register "Fd.create" (fun d -> Fd.create d);
  (* Retrocompatibility *)
  Callback.register "Fd.name" (fun v -> try Fd.name v with Failure _ -> "");
  Callback.register "Fd.is_bound" Fd.is_bound;

  (* Domains *)

  Callback.register "Fd.dom" (fun v -> Domain.create (Fd.values v));
  Callback.register "Domain.size" Domain.size;
  Callback.register "Domain.values" Domain.values;
  Callback.register "Domain.create" (fun a -> Domain.create (Array.to_list a));

  (* Arithmetic *)

  Callback.register "i2e" i2e;
  Callback.register "fd2e" fd2e;
  Callback.register "e2fd" Arith.e2fd;

  Callback.register "lt" ( <~ );
  Callback.register "le" ( <=~ );
  Callback.register "ne" ( <>~ );
  Callback.register "eq" ( =~ );
  Callback.register "gt" ( >~ );
  Callback.register "ge" ( >=~ );

  Callback.register "arith_add" ( +~ );
  Callback.register "arith_sub" ( -~ );
  Callback.register "arith_mul" ( *~ );
  Callback.register "arith_div" ( /~ );
  Callback.register "arith_mod" ( %~ );
  Callback.register "arith_abs" Arith.abs;

  Callback.register "FdArray.get" FdArray.get;
  Callback.register "FdArray.max" FdArray.max;
  Callback.register "FdArray.min" FdArray.min;

  (* Constraint expression *)

  Callback.register "Cstr.post" Cstr.post;
  Callback.register "Cstr.name" Cstr.name;
  Callback.register "Cstr.alldiff" (
    fun a -> let algo =
               if a>0 then (Alldiff.Bin_matching Var.Fd.on_refine)
               else Alldiff.Lazy in Alldiff.cstr ~algo:algo);
  Callback.register "Cstr.or" (fun a b -> a ||~~ b);
  Callback.register "Cstr.and" (fun a b -> a &&~~ b);
  Callback.register "Cstr.boolean" (fun c -> Reify.boolean c);
  Callback.register "Cstr.not" Reify.not;
  Callback.register "Cstr.xor" Reify.xor;

  Callback.register "Interval.is_member" Interval.is_member;
  Callback.register "Sorting.sort" Sorting.sort;
  Callback.register "Gcc.cstr" (Gcc.cstr ~level:Gcc.High);

  (* Goal expression *)

  Callback.register "Fcl.interrupt"
    (fun _ -> Stak.fail "Manual interruption from Python interface");

  Callback.register "Goals.solve"
    (fun i x -> Goals.solve ~control:(on_backtrack i) x);

  Callback.register "Goals.atomic" (fun x -> Goals.atomic (goal_atomic x));
  Callback.register "Goals.unify" Goals.unify;
  Callback.register "Goals.success" Goals.success;
  Callback.register "Goals.fail" Goals.fail;
  Callback.register "Goals.and" (fun a b -> a &&~ b);
  Callback.register "Goals.or" (fun a b -> a ||~ b);
  Callback.register "Goals.forall" Goals.Array.forall ;
  Callback.register "Goals.minimize"
    (fun goal criteria i -> Goals.minimize goal criteria (on_solution i));

  (* Generic goal creation on variables *)

  Callback.register "Assignment.indomain" Goals.indomain;

  Callback.register "Assignment.assign" (fun v -> Goals.assign v);

  Callback.register "Assignment.dichotomic" Goals.dichotomic;

  Callback.register "Assignment.atomic"
    (fun i -> fun v -> Goals.atomic (fun () -> assign_atomic i v));

  Callback.register "Assignment.or"
    (fun a1 a2 -> (fun (v: Facile.Var.Fd.t) -> (a1 v) ||~ (a2 v)));

  Callback.register "Assignment.and"
    (fun a1 a2 -> (fun (v: Facile.Var.Fd.t) -> (a1 v) &&~ (a2 v)));


  (* Strategies on arrays *)

  Callback.register "Strategy.min_min"
    (Goals.Array.choose_index
       (fun a1 a2 -> Fd.min a1 < Fd.min a2));

  Callback.register "Strategy.min_domain"
    (Goals.Array.choose_index
       (fun a1 a2 -> Fd.size a1 < Fd.size a2));

  Callback.register "Strategy.queen"
    (let h a = (Fd.size a, Fd.min a) in
      Goals.Array.choose_index (fun a1 a2 -> h a1 < h a2));

  Callback.register "Strategy.callback"
    (fun i -> Goals.Array.choose_index (strategy_cb i));

  ()
