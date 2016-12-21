open Facile
open Easy

(**
 * In the following externals, the first integer corresponds to the index in the
 * callback array.
 *)

external on_backtrack: int -> int -> unit = "ml_backtrack_callback"
external goal_atomic: int -> unit -> unit = "ml_atomic_callback"
external on_solution: int -> int -> unit = "ml_onsol_callback"

let _ =

  (** 
   * All callback registrations
   *)

  (* Variable instantiation *)

  Callback.register "Fd.interval" (fun a b -> Fd.interval a b);
  Callback.register "Fd.name" Fd.name;
  Callback.register "Fd.min_max" Fd.min_max;
  Callback.register "Fd.is_bound" Fd.is_bound;

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
  Callback.register "Cstr.boolean" (Reify.boolean ~delay_on_negation:true);
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

  Callback.register "Goals.success" Goals.success;
  Callback.register "Goals.fail" Goals.fail;
  Callback.register "Goals.and" (fun a b -> a &&~ b);
  Callback.register "Goals.or" (fun a b -> a ||~ b);
  Callback.register "Goals.forall" Goals.Array.forall;
  Callback.register "Goals.minimize"
    (fun goal criteria i -> Goals.minimize goal criteria (on_solution i));

  Callback.register "Goals.indomain" Goals.indomain;

  Callback.register "Strategy.min_value"
    (Goals.Array.choose_index
       (fun a1 a2 -> Var.Attr.min a1 < Var.Attr.min a2));

  Callback.register "Strategy.min_domain"
    (Goals.Array.choose_index
       (fun a1 a2 -> Var.Attr.size a1 < Var.Attr.size a2));

  Callback.register "Strategy.min_min"
    (let h a = (Var.Attr.size a, Var.Attr.min a) in
      Goals.Array.choose_index (fun a1 a2 -> h a1 < h a2));

  (* TODO trash *)

  let try_min v = (* Instantiates to min or remove min *)
    match Fd.value v with
      Unk attr ->
      Goals.unify v (Var.Attr.min attr)
      ||~
      Goals.atomic
        (fun () -> Fd.refine v (Domain.remove_min (Var.Attr.dom attr)))
    | _ -> failwith "Tiles.try_min: v should be bound" in

  (*   Callback.register "Goals.indomain" try_min ; *)


  ()
