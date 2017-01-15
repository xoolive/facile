open Facile
open Easy

(**
 * In the following externals, the first integer corresponds to the index in the
 * callback array.
 *)

external on_backtrack: int -> int -> unit = "ml_backtrack_callback"
external goal_atomic: int -> unit -> unit = "ml_atomic_callback"
external on_solution: int -> int -> unit = "ml_onsol_callback"
external goal_forvar: int -> Goals.t -> Var.Fd.t -> Goals.t = "ml_goal_forvar_callback"

let _ =

  (**
   * All callback registrations
   *)

  (* Variable instantiation *)

  Callback.register "Fd.interval" (fun a b -> Fd.interval a b);
  Callback.register "Fd.name" Fd.name;
  Callback.register "Fd.min_max" Fd.min_max;
  Callback.register "Fd.is_bound" Fd.is_bound;

  (* Domains *)
  Callback.register "Fd.refine" Fd.refine;
  Callback.register "Fd.dom" (fun v -> Domain.create (Fd.values v));
  Callback.register "Domain.size" Domain.size;
  Callback.register "Domain.values" Domain.values;
  Callback.register "Domain.remove" Domain.remove;

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
  Callback.register "Goals.unify" Goals.unify;

  Callback.register "Goals.success" Goals.success;
  Callback.register "Goals.fail" Goals.fail;
  Callback.register "Goals.and" (fun a b -> a &&~ b);
  Callback.register "Goals.or" (fun a b -> a ||~ b);
  Callback.register "Goals.forall" Goals.Array.forall ;
  Callback.register "Goals.minimize"
    (fun goal criteria i -> Goals.minimize goal criteria (on_solution i));

  (* Generic goal creation on variables *)

  Callback.register "Goals.indomain" Goals.indomain;

  (**
   * Coded as follows
   *
   * let indomain var =
   *  create_rec ~name:"indomain"
   *    (fun self ->
   *       match Fd.value var with
   *         Val _ -> success
   *       | Unk var_ ->
   *         let mini = Fcl_domain.min (Attr.dom var_) in
   *         atomic (fun () -> Fd.subst var mini) ||~
   *         atomic
   *           (fun () -> Fd.refine var (Fcl_domain.remove mini (Attr.dom var_)))
   *         &&~ self);;
   *)

  Callback.register "Goals.create_on_var" (fun i var ->
        let g = Goals.create_rec (fun self -> (goal_forvar i) self var) in
        Printf.printf "goal created %d %s" i (Fd.name var); print_newline();
    g);

  (** From the tiles.ml example
   *
   *  How to code this in Python?
   *)
       let try_min v = (* Instantiates to min or remove min *)

         Goals.create_rec (fun self ->
         Printf.printf "enter try_min for var %s" (Fd.name v);
         print_newline ();
         match Fd.value v with
           Unk attr ->
           let mini = Fd.min v in
           Goals.unify v (mini) ||~
           Goals.atomic
             (fun () -> Fd.refine v (Domain.remove mini (Domain.create (Fd.values v))))
         | _ -> failwith "Tiles.try_min: v should be bound") in

     (* Callback.register "Goals.create_on_var" (
        fun i var -> let g = try_min var in
        Printf.printf "goal created %d %s" i (Fd.name var); print_newline();
        g);*)

  (* Strategies on arrays *)

  Callback.register "Strategy.min_value"
    (Goals.Array.choose_index
       (fun a1 a2 -> Var.Attr.min a1 < Var.Attr.min a2));

  Callback.register "Strategy.min_domain"
    (Goals.Array.choose_index
       (fun a1 a2 -> Var.Attr.size a1 < Var.Attr.size a2));

  Callback.register "Strategy.min_min"
    (let h a = (Var.Attr.size a, Var.Attr.min a) in
      Goals.Array.choose_index (fun a1 a2 -> h a1 < h a2));

  ()
