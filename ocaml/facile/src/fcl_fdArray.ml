(* $Id: fcl_fdArray.ml,v 1.28 2007-07-26 13:00:11 barnier Exp $ *)
open Fcl_var
open Fcl_arith
module C = Fcl_cstr
open Printf

let new_min_array xs x =
  let n = Array.length xs in
  let name = "FdArray.min" in
  let delay c =
    Array.iter (fun x -> Fd.delay [Fd.on_min; Fd.on_max] x c) xs;
    Fd.delay [Fd.on_min; Fd.on_max] x c
  and update _ =
    (* Try to decide which one is the smallest *)
    Fcl_debug.call 'm' (fun f -> fprintf f "min(%a)=%a\n" Fd.fprint_array xs Fd.fprint x);
    let smallest = ref 0 and min_smallest = ref (Fd.min xs.(0)) in
    for i = 1 to n - 1 do
      let min_i = Fd.min xs.(i) in
      if min_i < !min_smallest then begin
	smallest := i; min_smallest := min_i end
    done;
    let max_smallest = Fd.max xs.(!smallest) in
    let (minx, maxx) = Fd.min_max x in
    try
      for i = 0 to n - 1 do
	let mi = Fd.min xs.(i) in
	(* Check is xs.(i) is a candidate to be the min *)
	if i <> !smallest &&
	  mi < max_smallest && mi <= maxx && Fd.max xs.(i) >= minx then raise Exit
      done;
      Fcl_cstr.post (fd2e x =~ fd2e xs.(!smallest));
      true
    with Exit ->
      (* All the xs are greater than the min of min *)
      let x_ge_min xi =
	match Fd.value xi with
	  Val v -> if minx > v then Fcl_stak.fail name
	| Unk a -> if minx > Fcl_domain.min a then Fd.refine_low xi minx in
      Array.iter x_ge_min xs;
      (* smallest min of xs <= x <= smallest max of xs *)
      let smallest_max =
	Array.fold_left
	  (fun r xi -> Fcl_misc.Operators.min (Fd.max xi) r) max_int xs in
      match Fd.value x with
	Val x ->
	  if not (!min_smallest <= x && x <= smallest_max) then
	    Fcl_stak.fail name
	  else false
      | Unk _a ->
          Fd.refine_low_up x !min_smallest smallest_max;
	  false in
  C.create ~name update delay


let new_max_array xs x =
  let n = Array.length xs in
  let name = "FdArray.max" in
  let delay c =
    Array.iter (fun x -> Fd.delay [Fd.on_min; Fd.on_max] x c) xs;
    Fd.delay [Fd.on_min; Fd.on_max] x c
  and update _ =
    (* Try to decide which one is the greatest *)
    let greatest = ref 0 and max_greatest = ref (Fd.max xs.(0)) in
    for i = 1 to n - 1 do
      let max_i = Fd.max xs.(i) in
      if max_i > !max_greatest then begin
	greatest := i; max_greatest := max_i
      end
    done;
    let min_greatest = Fd.min xs.(!greatest) in
    try
      for i = 0 to n - 1 do
	if i <> !greatest && Fd.max xs.(i) > min_greatest then
	  raise Exit
      done; (* We have found the greatest element *)
      Fcl_cstr.post (fd2e x =~ fd2e xs.(!greatest));
      true
    with
      Exit ->
	(* All the xs are smaller than the max of max *)
      	let maxx = Fd.max x in
      	let x_leq_max xi =
	  match Fd.value xi with
	    Val v -> if maxx < v then Fcl_stak.fail name
	  | Unk a -> if maxx < Fcl_domain.max a then Fd.refine_up xi maxx in
	Array.iter x_leq_max xs;
	(* greatest min of xs <= x <= greatest max of xs *)
      	let greatest_min =
	  Array.fold_left
	    (fun r xi -> Fcl_misc.Operators.max (Fd.min xi) r) min_int xs in
	begin
	  match Fd.value x with
	    Val x -> assert (x >= greatest_min)
      	  | Unk _a -> Fd.refine_low_up x greatest_min (Fd.max xs.(!greatest))
	end;
	false in

  C.create ~name update delay

let min_cstr xs x =
  if Array.length xs = 0 then invalid_arg "FdArray.min_cstr";
  (* To prevent array modifications by the user *)
  let xs = Array.copy xs in
  new_min_array xs x

let min xs =
  if Array.length xs = 1 then xs.(0) else
  let x = Fd.create Fcl_domain.int in
  let c = min_cstr xs x in
  Fcl_cstr.post c;
  x

let max_cstr xs x =
  if Array.length xs = 0 then invalid_arg "FdArray.max_cstr";
  (* To prevent array modifications by the user *)
  let xs = Array.copy xs in
  new_max_array xs x

let max xs =
  if Array.length xs = 1 then xs.(0) else
  let x = Fd.create Fcl_domain.int in
  let c = max_cstr xs x in
  Fcl_cstr.post c;
  x

open Printf

let new_element index array value =
  let n = Array.length array in
  let name = "FdArray.get" in

  let bound_index i =
    assert(i >= 0 && i < n);
    Fcl_debug.call 'e' (fun s -> fprintf s "%s: bound_index=%d\n" name i);
    Fcl_cstr.post (fd2e value =~ fd2e array.(i));
    Fcl_debug.call 'e' (fun s -> fprintf s "value=%a\n" Fd.fprint value);
    true in

  let delay x =
    Fd.delay [Fd.on_refine] index x;
    Fd.delay [Fd.on_refine] value x;
    Array.iter (fun v -> Fd.delay [Fd.on_refine] v x) array
  and update _ =
    Fcl_debug.call 'e' (fun s -> fprintf s "[|%a|].(%a) = %a\n" (fun s -> Array.iter (fun v -> fprintf s "%a " Fd.fprint v)) array Fd.fprint index Fd.fprint value);
    let dom_value = Fd.dom value in
    let index_to_keep = ref []
    and new_dom_value = ref Fcl_domain.empty in
    match Fd.value index with
      Val i ->
	bound_index i
    | Unk index_ ->
	Fcl_domain.iter
	  (fun i ->
	    let inter = Fcl_domain.intersection (Fd.dom array.(i)) dom_value in
	    if not (Fcl_domain.is_empty inter) then begin
	      index_to_keep := i :: !index_to_keep;
	      new_dom_value := Fcl_domain.union inter !new_dom_value end)
          index_;
	Fd.refine index (Fcl_domain.unsafe_create (List.rev !index_to_keep));
	match Fd.value index with
	  Val i ->
	    bound_index i
    	| Unk _index_ ->
	    begin
	      match Fd.value value with
	      	Val _ -> Fcl_debug.call 'e' (fun s -> fprintf s "After: [|%a|].(%a) = %a\n" (fun s -> Array.iter (fun v -> fprintf s "%a " Fd.fprint v)) array Fd.fprint index Fd.fprint value) (* Something more to do ? *)
	      | Unk _ ->
		  Fd.refine value !new_dom_value;
		  Fcl_debug.call 'e' (fun s -> fprintf s "After: [|%a|].(%a) = %a\n" (fun s -> Array.iter (fun v -> fprintf s "%a " Fd.fprint v)) array Fd.fprint index Fd.fprint value)
	    end;
	    false in

  let init () =
    begin match Fd.value index with
      Val i ->
	if i >= 0 && i < n then ignore (bound_index i)
	else Fcl_stak.fail (name ^ ": index out of bound")
    | Unk _index_attr ->	Fd.refine_low_up index 0 (n - 1) end;
    update 0 in

  C.create ~name ~init update delay

(* Acces to an array of integers *)
let new_element_of_ints index array value =
  let n = Array.length array in
  assert(0 <= Fd.min index && Fd.max index < n);

  let bound_index i =
    assert(i >= 0 && i < n);
    Fcl_debug.call 'e' (fun s -> fprintf s "Element: bound_index=%d\n" i);
    Fd.unify value array.(i);
    Fcl_debug.call 'e' (fun s -> fprintf s "value=%a\n" Fd.fprint value);
    true in

  let index_size = Fcl_stak.ref 0 in

  let name = "FdArray.get_ints"
  and delay x =
    Fd.delay [Fd.on_refine] index x;
    Fd.delay [Fd.on_refine] value x
  and update _ =
    let index_has_changed = Fd.size index <> Fcl_stak.get index_size in
    Fcl_debug.call 'e' (fun s -> fprintf s "[|%a|].(%a) = %a\n" (fun s -> Array.iter (fun v -> fprintf s "%d " v)) array Fd.fprint index Fd.fprint value);
    let dom_value = Fd.dom value in
    match Fd.value index with
      Val i -> bound_index i
    | Unk index_ ->
    	let index_to_keep = ref []
    	and new_values = ref [] in
	Fcl_domain.iter
	  (fun i ->
	    if Fcl_domain.member dom_value array.(i) then begin
	      index_to_keep := i :: !index_to_keep;
	      new_values := array.(i) :: !new_values end)
	  index_;
	let new_dom_index =
	  Fcl_domain.unsafe_create (List.rev !index_to_keep) in
	Fd.refine index new_dom_index;
	Fcl_stak.set index_size (Fcl_domain.size new_dom_index);
	match Fd.value index with
	  Val i ->
	    bound_index i
    	| Unk _index_ ->
	    if index_has_changed then
	      match Fd.value value with
	      	Val _ -> (* index already has the correct domain; finished*)
		  Fcl_debug.call 'e' (fun s -> fprintf s "After: [|%a|].(%a) = %a\n" (fun s -> Array.iter (fun v -> fprintf s "%d " v)) array Fd.fprint index Fd.fprint value);
		  true
	      | Unk _ ->
		  Fd.refine value (Fcl_domain.create !new_values);
		  Fcl_debug.call 'e' (fun s -> fprintf s "After: [|%a|].(%a) = %a\n" (fun s -> Array.iter (fun v -> fprintf s "%d " v)) array Fd.fprint index Fd.fprint value);
		  Fd.size value = 1
	    else false in

  let init () =
    begin match Fd.value index with
      Val i ->
	if i >= 0 && i < n then ignore (bound_index i)
	else Fcl_stak.fail (name ^ ": index out of bound")
    | Unk _index_attr ->	Fd.refine_low_up index 0 (n - 1) end;
    update 0 in

  C.create ~name ~init update delay


let array_forall p a =
  let n = Array.length a in
  try
    for i = 0 to n-1 do if not (p a.(i)) then raise Exit done; true
  with Exit -> false

let get_cstr array index value =
  let n = Array.length array in
  if n = 0 then invalid_arg "FdArray.get_cstr: empty array";
  match Fd.value index with
    Val i ->
      if 0 <= i && i < n then fd2e value =~ fd2e array.(i) else Fcl_cstr.zero
  | _ ->
      if array_forall (fun x -> not (Fd.is_var x)) array then
	let ints = Array.map Fd.elt_value array in (* Only integers *)
	if array_forall (fun x -> x = ints.(0)) ints then (* All equal ! *)
	  Fcl_var.Fd.unify_cstr value ints.(0)
	else new_element_of_ints index ints value
      else new_element index array value

let get array index =
  match Fd.value index with
    Val i -> (* Index connu *)
      if i >= 0 && i < Array.length array then array.(i)
      else Fcl_stak.fail "FdArray.get: index out of bound"
  | _ ->
      let (mi, ma) =
      	Array.fold_left
	  (fun (mi, ma) e ->
	    (Stdlib.min mi (Fd.min e), Stdlib.max ma (Fd.max e)))
	  (max_int, min_int) array in
      if mi = ma then Fd.int mi else
      let value = Fd.create (Fcl_domain.interval mi ma) in
      (* To prevent array modifications by the user *)
      let array = Array.copy array in
      Fcl_cstr.post (get_cstr array index value);
      value

let sum_cstr a s = fd2e s =~ sum_fd a

let sum a =
  match Array.length a with
    0 -> invalid_arg "FdArray.sum: empty array"
  | 1 -> a.(0)
  | _ ->
      let sexp = sum_fd a in
      let (mini, maxi) = min_max_of_expr sexp in
      let s = Fd.interval mini maxi in
      Fcl_cstr.post (fd2e s =~ sexp);
      s
