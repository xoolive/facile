(* $Id: fcl_opti.ml,v 1.11 2001-06-07 14:07:07 barnier Exp $ *)

open Fcl_var
open Fcl_arith
open Fcl_goals

let minimize_restart goal (cost : Fd.t) ?control step compute_solution =
  let solution = ref None
  and best_cost = ref (Fd.max cost + step) in
  try
    let bound = fun () -> Fcl_cstr.post (fd2e cost <=~ i2e (!best_cost - step)) in
    while solve ?control (atomic bound &&~ goal) do
      let m = Fd.elt_value cost in
      solution := Some (compute_solution m);
      Fcl_stak.backtrack_all ();
      best_cost := m
    done;
    !solution
  with Exit -> !solution;;


let minimize_continue goal (cost : Fd.t) ?(control = (fun _ -> ())) step compute_solution =
  let rec bt_until c = (* Backtrack until lower bound better than current cost *)
    let gs = Fcl_stak.backtrack () in
    if Fd.min cost < c then begin
      ignore (Fcl_stak.save gs)
    end else
      bt_until c in

  let solution = ref None
  and best_cost = ref (Fd.max cost) in
  let restore_max bt =
    control bt;
    match Fd.value cost with
	Val v -> if v > !best_cost then Fcl_stak.fail "restore_max"
      | Unk _attr ->
	  Fd.refine_up cost !best_cost in

  let found_one =
    Fcl_goals.atomic ~name:"found_one"
      (fun () ->
        let c = Fd.elt_value cost in
      	solution := Some (compute_solution c);
	best_cost := c - step;
	bt_until c;
      	Fcl_stak.fail "Opti.minimize_more") in

  ignore (solve ~control:restore_max (goal &&~ found_one));
  !solution

type mode = Restart | Continue

let minimize g c ?control ?(step = 1) ?(mode = Restart) cs =
  if step <= 0 then invalid_arg "Opti.minimize: step must be non negative";
  match mode with
    Restart -> minimize_restart g c ?control step cs
  | Continue -> minimize_continue g c ?control step cs
