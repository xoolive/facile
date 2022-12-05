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
external strategy_cb: int -> Facile.Var.Fd.t array -> int = "ml_strategy_cb"
external update_cb: int -> int -> int = "ml_update_cb"
external delay_cb: int -> Facile.Cstr.t -> unit = "ml_delay_cb"

external selector_select: int -> 'a array -> int = "ml_selector_select"
external selector_labelling: int -> int -> Facile.Goals.t =
  "ml_selector_labelling"
external goal_creator: int -> unit -> Facile.Goals.t =
  "ml_goal_creator"

let _ =

  (**
   * All callback registrations
   *)

  Callback.register_exception "Facile.Stak.Fail"
    (Facile.Stak.Fail "Python callback");

  (* Variable instantiation *)

  Callback.register "Fd.interval" (fun a b -> Fd.interval a b);
  Callback.register "Fd.create" (fun d -> Fd.create d);
  (* Retrocompatibility *)
  Callback.register "Fd.name" (fun v -> try Fd.name v with Failure _ -> "");
  Callback.register "Fd.is_bound" Fd.is_bound;
  Callback.register "Fd.min" Fd.min;
  Callback.register "Fd.max" Fd.max;
  Callback.register "Fd.refine" Fd.refine;
  Callback.register "Fd.delay" (fun event_list var cstr ->
      Fd.delay (Array.to_list event_list) var cstr);

  (* Domains *)

  Callback.register "Fd.dom" (fun v -> Domain.create (Fd.values v));
  Callback.register "Domain.size" Domain.size;
  Callback.register "Domain.values" Domain.values;
  Callback.register "Domain.create" (fun a -> Domain.create (Array.to_list a));
  Callback.register "Domain.remove_low" Domain.remove_low;
  Callback.register "Domain.remove_up" Domain.remove_up;

  (* Events *)

  Callback.register "Fd.on_max" Fd.on_max;
  Callback.register "Fd.on_min" Fd.on_min;
  Callback.register "Fd.on_refine" Fd.on_refine;
  Callback.register "Fd.on_subst" Fd.on_subst;

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
  Callback.register "FdArray.sum" FdArray.sum;

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

  Callback.register "Cstr.create" (fun update delay ->
      let update_stak x =
        match update_cb update x with
        | 0 -> false
        | 1 -> true
        | _ -> raise (Stak.Fail "Python update callback") in
      Cstr.create ~priority:Cstr.later update_stak (delay_cb delay)
    );

  (* Goal expression *)

  Callback.register "Goals.solve"
    (fun i x -> Goals.solve ~control:(on_backtrack i) x);

  Callback.register "Goals.create"
    (fun x -> Goals.create (goal_creator x) ());
  Callback.register "Goals.atomic" (fun x -> Goals.atomic (goal_atomic x));
  Callback.register "Goals.unify" Goals.unify;
  Callback.register "Goals.success" Goals.success;
  Callback.register "Goals.fail" Goals.fail;
  Callback.register "Goals.and" (fun a b -> a &&~ b);
  Callback.register "Goals.or" (fun a b -> a ||~ b);
  Callback.register "Goals.forall" Goals.Array.forall;
  Callback.register "Goals.minimize"
    (fun mode goal criteria i ->
       Goals.minimize ~mode goal criteria (on_solution i));

  Callback.register "Goals.continue" Goals.Continue;
  Callback.register "Goals.restart" Goals.Restart;
  Callback.register "Goals.dicho" Goals.Dicho;


  (* Selectors *)

  Callback.register "Goals.selector.labelling"
    (fun i -> selector_labelling i);

  Callback.register "Goals.selector.select"
    (fun i a -> let p = selector_select i a in
      match p with
      -1 -> raise Not_found
      | _ -> p);

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

  Callback.register "Strategy.lexicographic"
    (Goals.Array.choose_index
       (fun a1 a2 -> Fd.is_var a1 > Fd.is_var a2));

  Callback.register "Strategy.min_min"
    (Goals.Array.choose_index
       (fun a1 a2 -> Fd.min a1 < Fd.min a2));

  Callback.register "Strategy.min_domain"
    (Goals.Array.choose_index
       (fun a1 a2 -> Fd.size a1 < Fd.size a2));

  Callback.register "Strategy.queen"
    (let h a = (Fd.size a, Fd.min a) in
      Goals.Array.choose_index (fun a1 a2 -> h a1 < h a2));

  Callback.register "Strategy.callback" (fun i a ->
      match strategy_cb i a with
      | -1 -> raise Not_found
      | n -> n);


  (* Stack references *)

  Callback.register "Stak.ref" (fun (b: bool) -> Facile.Stak.ref b);
  Callback.register "Stak.set" (fun r (b: bool) -> Facile.Stak.set r b);
  Callback.register "Stak.get"
    (fun (r: bool Facile.Stak.ref) -> Facile.Stak.get r);

  Callback.register "Stak.trail" (fun i -> (Stak.trail (goal_atomic i)));

  ()
