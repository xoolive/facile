open Facile
open Easy

type heuristic = No_heuristic | Min_size | Min_value | Min_min

let goal x = function
  No_heuristic -> Goals.Array.labeling x
| Min_size ->
    let min_size = Goals.Array.choose_index
                     (fun a1 a2 -> Var.Attr.size a1 < Var.Attr.size a2) in
    Goals.Array.forall ~select:min_size Goals.indomain x
| Min_value ->
    let min_val = Goals.Array.choose_index
                    (fun a1 a2 -> Var.Attr.min a1 < Var.Attr.min a2) in
    Goals.Array.forall ~select:min_val Goals.indomain x
| Min_min ->
    let h a = (Var.Attr.size a, Var.Attr.min a) in
    let min_min = Goals.Array.choose_index (fun a1 a2 -> h a1 < h a2) in
    Goals.Array.forall ~select:min_min Goals.indomain x

let _ =

  Callback.register "Fd.interval" (fun a b -> Fd.interval a b);
  Callback.register "Fd.name" Fd.name;
  Callback.register "Fd.min_max" Fd.min_max;
  Callback.register "Fd.is_bound" Fd.is_bound;

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

  Callback.register "Cstr.post" Cstr.post;
  Callback.register "Cstr.name" Cstr.name;
  Callback.register "Cstr.alldiff" (Alldiff.cstr ~algo:Alldiff.Lazy);
  Callback.register "Cstr.or" (fun a b -> a ||~~ b);
  Callback.register "Cstr.and" (fun a b -> a &&~~ b);

  Callback.register "FdArray.get" FdArray.get;

  Callback.register "FdArray.count_eq" (
    fun array j ->
      let is_equal_to i x = fd2e x =~~ i2e i in
      Arith.sum (Array.map (is_equal_to j) array)
  );

  Callback.register "Goals.Array.solve"
    (fun x h -> Goals.solve (goal x h));

  Callback.register "Gools.Array.solve_all"
    (fun x ->
       let store = ref [] in
       let store_res res =
         Fd.fprint_array stdout res;
         let vals = Array.map Fd.min res in
         store := vals::!store in
       let _ = Goals.solve ( (Goals.Array.labeling x
                              &&~ Goals.atomic (fun () -> store_res x)
                              &&~ Goals.fail) ||~ Goals.success) in
       !store
    );

  Callback.register "Goals.Array.solve_bt" (
    fun x h ->
      let backtrack = ref 0 in
      let p = Goals.solve ~control:(fun n -> backtrack := n) (goal x h) in
      p, !backtrack);

  Callback.register "Goals.minimize" (
    fun x expr ->
      let store = ref None in
      let g = Goals.minimize (Goals.Array.labeling x) (Arith.e2fd expr)
                (fun cc -> store := Some(cc, Array.map Fd.elt_value x)) in
      let _ = Goals.solve (g ||~ Goals.success) in
      !store
  );

  ()
