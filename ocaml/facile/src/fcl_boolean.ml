open Fcl_var
open Fcl_misc.Operators

(* sum xi = v ou sum xi <> v*)
let linear (terms : Fd.t array) v shared_min shared_max equal =
  let name = "Boolean.linear" in
  let monotonic_propagate subst =
    Array.fold_left
      (fun sum x ->
	match Fd.value x with
	  Val 0 -> sum
	| Val 1 -> 1 + sum
	| Unk _attr -> begin Fd.subst x subst; subst + sum end
	| _ -> Fcl_debug.internal_error (name ^ ": non boolean variable"))
      0 terms in

  let delay c =
    Array.iter (fun x -> Fd.delay [Fd.on_subst] x c) terms;
    Fd.delay [Fd.on_min] v c;
    Fd.delay [Fd.on_max] v c
  and fprint c =
    Printf.fprintf c "%a %s(bool) " Fd.fprint v (if equal then "=" else "<>");
    if Array.length terms > 0 then begin
      Fd.fprint c terms.(0);
      for i = 1 to Array.length terms - 1 do
	Printf.fprintf c "+%a" Fd.fprint terms.(i) done end;
    flush c
  and update _ =
    let shared_min = Fcl_stak.get shared_min
    and shared_max = Fcl_stak.get shared_max in
    if equal then
    (* if the maximum of v is reached, all other variables can be set to 0 *)
      if shared_min = Fd.max v then begin
      	if monotonic_propagate 0 > Fd.max v then
	  Fcl_stak.fail (name ^ ": monotonic_propagate > max");
      	Fd.unify v shared_min;
	true end
    (* and vice versa *)
      else if shared_max = Fd.min v then begin
       	if monotonic_propagate 1 < Fd.min v then
	  Fcl_stak.fail (name ^ ": monotonic_propagate < min");
       	Fd.unify v shared_max;
	true end
      else if shared_min = shared_max then begin
	Fd.unify v shared_min; true end
      else begin Fd.refine_low_up v shared_min shared_max; false end

    else begin (* not equal *)
      if shared_min = shared_max then begin
	begin match Fd.value v with
	  Val x -> if x = shared_min then Fcl_stak.fail (name ^ ": (<>)")
	| Unk dom ->
	    Fd.refine v (Fcl_domain.remove shared_min dom) end;
	true end
      else (shared_min > Fd.max v || shared_max < Fd.min v) end in

  Fcl_cstr.create ~name ~fprint update delay


let demon xs shared_min shared_max =
  let name = "Boolean.demon" in
  let delay c =
    Array.iteri (fun i xi -> Fd.delay [Fd.on_subst] xi ~waking_id:i c) xs
  and fprint c = Printf.fprintf c "%s: %a" name Fd.fprint_array xs
  and init () = false

  and update i =
    begin match Fd.value xs.(i) with
      	Val 0 -> Fcl_stak.decr shared_max
      | Val 1 -> Fcl_stak.incr shared_min
      | _ -> Fcl_debug.internal_error "boolean_demon : variable is not ground or not boolean" end;
    true in

  Fcl_cstr.create ~name ~fprint ~init update ~nb_wakings:(Array.length xs) delay

let is_boolean x =
  let min_x, max_x = Fcl_var.Fd.min_max x in min_x = 0 && max_x = 1
let is_boolean_array l =
  try
    Array.iter (fun b -> if not (is_boolean b) then raise Exit) l;
    true
  with Exit -> false

let cstr bools sum =
  assert (is_boolean_array bools);
  let size = Array.length bools in
  let shared_min = Fcl_stak.ref 0 and shared_max = Fcl_stak.ref size in
  Fcl_cstr.init (demon bools shared_min shared_max);
  linear bools sum shared_min shared_max true

let sum bools =
  assert (is_boolean_array bools);
  let size = Array.length bools in
  let shared_min = Fcl_stak.ref 0 and shared_max = Fcl_stak.ref size in
  Fcl_cstr.init (demon bools shared_min shared_max);
  let sum = Fd.create (Fcl_domain.interval 0 size) in
  Fcl_cstr.post (linear bools sum shared_min shared_max true);
  sum
