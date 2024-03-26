open Fcl_var
open Fcl_misc.Operators
open Printf
module C = Fcl_cstr

type operator = LessThan | Equal | Diff

let string_of_op = function Equal -> "=" | LessThan -> "<=" | Diff -> "<>"

let min_max_plus_inter a b c d = (a + c, b + d)

let min_max_minus_inter a b c d = (a - d, b - c)

(* x1 <= x2 + d *)
(* specialized binary leq constraint *)
let rec less_than x1 x2 d =
  let name = "Fcl_linear.less_than" in
  let update _ =
    match Fd.value x1, Fd.value x2 with
      Unk a1, Unk a2 ->
	let max1 = Fcl_domain.max a1
	and max2d = Fcl_domain.max a2 + d in
	if max1 > max2d then
	  Fd.refine x1 (Fcl_domain.remove_up max2d a1);
	let min2 = Fcl_domain.min a2
	and min1d = Fcl_domain.min a1 - d in
	if min1d > min2 then
	  Fd.refine x2 (Fcl_domain.remove_low min1d a2);
	Fd.max x1 <= Fd.min x2 + d
    | Val v1, Unk a2 ->
	let min2 = Fcl_domain.min a2 in
	if min2 < v1 - d then
	  Fd.refine x2 (Fcl_domain.remove_low (v1-d) a2);
	true
    | Unk a1, Val v2 ->
	let max1 = Fcl_domain.max a1 in
	if max1 > v2+d then
	  Fd.refine x1 (Fcl_domain.remove_up (v2+d) a1);
	true
    | Val v1, Val v2 ->
	v1 <= v2+d || Fcl_stak.fail name
  and check () =
    let min1, max1 = Fd.min_max x1
    and min2, max2 = Fd.min_max x2 in
    max1 <= min2 + d || (if min1 > max2 + d then false else raise C.DontKnow)
  and not () = less_than x2 x1 (1-d)
  and delay ct =
    Fd.delay [Fd.on_min] x1 ct;
    Fd.delay [Fd.on_max] x2 ct
  and fprint f =
    Printf.fprintf f "%s: %a <= %a + %d" name Fd.fprint x1 Fd.fprint x2 d
  in
  C.create ~not ~check ~name ~fprint update delay

(* x1 = k*x2 *)
(* specialized binary eq and diff constraint *)
(* propagations on domain bounds: less powerful than linear when k = 1 *)
let rec equalc x1 k x2 =
  let bounds () = (Fd.min_max x1, Fd.min_max x2) in
  let name = "Fcl_linear.equalc" in
  let update _ =
    Fcl_debug.call 'a' (fun s -> fprintf s "equalc - before update: %a = %d * %a\n" Fd.fprint x1 k Fd.fprint x2);
    if k = 0 then begin Fd.unify x1 0; true end else
    let rec loop () =
      match Fd.value x1, Fd.value x2 with
	Unk _a1, Unk _a2 ->
	  let cbounds = bounds () in
	  let (c, d) = Fd.min_max x2 in
	  let (kc, kd) = if k > 0 then (k*c, k*d) else (k*d, k*c) in
	  Fd.refine_low_up x1 kc kd;
	  let (a, b) = Fd.min_max x1 in
	  let (ak, bk) = if k > 0 then (a /+ k, b /- k) else (b /+ k, a /- k) in
	  Fd.refine_low_up x2 ak bk;
	  if cbounds <> bounds () then loop () else false
      | Val v1, Unk _a2 ->
	  if v1 mod k = 0 then
	    begin Fd.unify x2 (v1/k); true end
	  else
	    Fcl_stak.fail name
      | Unk _a1, Val v2 -> Fd.unify x1 (k*v2); true
      | Val v1, Val v2 -> v1 = k*v2 || Fcl_stak.fail name in
    loop ()

  and check () =
    match Fd.value x1, Fd.value x2 with
      Unk _a1, Unk _a2 ->
	let (a, b) = Fd.min_max x1 in
	let (c, d) = Fd.min_max x2 in
	let (kc, kd) = if k > 0 then (k*c, k*d) else (k*d, k*c) in
	if a > kd || b < kc then false else raise C.DontKnow
    | Val v1, Unk a2 ->
	if v1 mod k <> 0 || not (Fcl_domain.member a2 (v1/k)) then
	  false
	else raise C.DontKnow
    | Unk a1, Val v2 ->
	if not (Fcl_domain.member a1 (k*v2)) then
	  false
	else raise C.DontKnow
    | Val v1, Val v2 ->	v1 = k*v2

  and not () = diffc x1 k x2
  and delay ct =
    Fd.delay [Fd.on_min; Fd.on_max] x1 ct;
    Fd.delay [Fd.on_min; Fd.on_max] x2 ct
  and fprint f =
    Printf.fprintf f "%s: %a = %d * %a" name Fd.fprint x1 k Fd.fprint x2 in
  C.create ~name ~fprint ~not ~check update delay

(* x1 <> k*x2 *)
and diffc x1 k x2 =
  let name = "Fcl_linear.diffc" in
  let update _ =
    if k = 0 then
      match Fd.value x1 with
	Unk a1 -> Fd.refine x1 (Fcl_domain.remove 0 a1); true
      |	Val v1 -> v1 <> 0 || Fcl_stak.fail name
    else
      match Fd.value x1, Fd.value x2 with
	Unk _a1, Unk _a2 ->
	  let (c, d) = Fd.min_max x2 in
	  let (kc, kd) = if k > 0 then (k*c, k*d) else (k*d, k*c) in
	  let (a, b) = Fd.min_max x1 in
	  (a > kd || b < kc)
      | Val v1, Unk a2 ->
	  v1 mod k <> 0 || not (Fcl_domain.member a2 (v1/k))
      | Unk a1, Val v2 -> not (Fcl_domain.member a1 (k*v2))
      | Val v1, Val v2 -> v1 <> k*v2 || Fcl_stak.fail name

  and check () =
    match Fd.value x1, Fd.value x2 with
      Unk _a1, Unk _a2 ->
	let (a, b) = Fd.min_max x1 in
	let (c, d) = Fd.min_max x2 in
	let (kc, kd) = if k > 0 then (k*c, k*d) else (k*d, k*c) in
	(a > kd || b < kc) || raise C.DontKnow
    | Val v1, Unk a2 ->
	v1 mod k <> 0 || not (Fcl_domain.member a2 (v1/k))
      || raise C.DontKnow
    | Unk a1, Val v2 ->
	not (Fcl_domain.member a1 (k*v2)) || raise C.DontKnow
    | Val v1, Val v2 ->	v1 <> k*v2

  and not () = equalc x1 k x2
  and delay ct =
    Fd.delay [Fd.on_min; Fd.on_max] x1 ct;
    Fd.delay [Fd.on_min; Fd.on_max] x2 ct
  and fprint f =
    Printf.fprintf f "%s: %a <> %d * %a" name Fd.fprint x1 k Fd.fprint x2 in
  C.create ~name ~fprint ~not ~check update delay


let remove_constants terms =
  let modif = ref false in
  let r =
    List.fold_left
      (fun (cst, vars) ((a, x) as ax) ->
	match Fd.value x with
	  Unk _ -> (cst, ax::vars)
	| Val v -> modif := true; (cst+a*v, vars))
      (0, [])
      terms in
  if !modif then r else raise Not_found

let compute_inf_sup pos_terms neg_terms =
  let neg_inf_sum, neg_sup_sum =
    List.fold_left
      (fun (inf,sup) (c,x) ->
	let mi, ma = Fd.min_max x in (inf+c*ma, sup+c*mi))
      (0, 0)
      neg_terms in
  List.fold_left
    (fun (inf,sup) (c,x) ->
      let mi,ma = Fd.min_max x in (inf+c*mi, sup+c*ma))
    (neg_inf_sum, neg_sup_sum)
    pos_terms

let part_pos_neg l =
  let rec loop pos neg = function
      [] -> (pos, neg)
    | (0, _) :: axs -> loop pos neg axs
    | (a, _x) as ax :: axs ->
	if a > 0 then loop (ax :: pos) neg axs
	else loop pos (ax :: neg) axs in
  loop [] [] l

(* terms = c *)
let linear_aux terms c =
  let (pos, neg) = part_pos_neg terms in
  let (a, b) = compute_inf_sup pos neg in
  let (ac, bc) = (a - c, b - c) in
  Fd.interval ac bc

(* In case all terms are positive (resp. negative), one can try to refine
   individually each term before computing the expression bounds
   (e.g. avoids an integer overflow in huge magic sequences caused) *)
let basic_refinements pos_terms neg_terms d op =
  if op <> Diff then
    if List.for_all (fun (_a, x) -> Fd.min x >= 0 ) pos_terms &&
      List.for_all (fun (_a, x) -> Fd.max x <= 0 ) neg_terms then begin
	List.iter (fun (a, x) -> Fd.refine_up x (d/a)) pos_terms;
	List.iter (fun (a, x) -> Fd.refine_low x (d/a)) neg_terms end
    else if op = Equal &&
      List.for_all (fun (_a, x) -> Fd.max x <= 0 ) pos_terms &&
      List.for_all (fun (_a, x) -> Fd.min x >= 0 ) neg_terms then begin
	List.iter (fun (a, x) -> Fd.refine_low x (d/a)) pos_terms;
	List.iter (fun (a, x) -> Fd.refine_up x (d/a)) neg_terms end

(* linear constraint a1*x1+...+an*xn=d *)
let rec linear_cstr terms pos_terms1 neg_terms1 op d1 =
  let pos_terms = Fcl_stak.ref pos_terms1
  and neg_terms = Fcl_stak.ref neg_terms1
  and d = Fcl_stak.ref d1 in

  let name = "Fcl_linear.linear" in
  let delay c =
    List.iter
      (fun (a, x) ->
	match op with
	  Diff -> Fd.delay [Fd.on_subst] x c
	| Equal -> Fd.delay [Fd.on_refine] x c
	| LessThan -> Fd.delay [if a > 0 then Fd.on_min else Fd.on_max] x c)
      terms in
  let fprint c =
    Printf.fprintf c "%s:" name;
    List.iter (fun (a,x) -> Printf.fprintf c " +%d.%a" a Fd.fprint x)
      (Fcl_stak.get pos_terms);
    List.iter (fun (a,x) -> Printf.fprintf c " %d.%a" a Fd.fprint x)
      (Fcl_stak.get neg_terms);
    Printf.fprintf c " %s %d" (string_of_op op) (Fcl_stak.get d);
    flush c in

  let remove_constants () =
    begin try
      let (cst, new_pos_terms) = remove_constants (Fcl_stak.get pos_terms) in
      if cst <> 0 then Fcl_stak.set d (Fcl_stak.get d - cst);
      Fcl_stak.set pos_terms new_pos_terms
    with Not_found -> () end; (* No new constant positive term *)
    begin try
      let (cst, new_neg_terms) = remove_constants (Fcl_stak.get neg_terms) in
      if cst <> 0 then Fcl_stak.set d (Fcl_stak.get d - cst);
      Fcl_stak.set neg_terms new_neg_terms
    with Not_found -> () end in (* No new constant negative term *)

  let not () =
    let terms = Fcl_stak.get pos_terms @ Fcl_stak.get neg_terms
    and d = Fcl_stak.get d in
    match op with
      Equal -> cstr terms Diff d
    | Diff -> cstr terms Equal d
    | LessThan ->
	cstr (List.map (fun (a,x) -> (-a, x)) terms) LessThan (-1 - d)

  and check () =
    remove_constants ();
    let d = Fcl_stak.get d in
    let pos_terms = Fcl_stak.get pos_terms
    and neg_terms = Fcl_stak.get neg_terms in

    match (pos_terms, neg_terms) with
      ([], []) -> begin
	match op with
	  Diff -> d <> 0
	| Equal -> d = 0
	| LessThan -> d >= 0 end
    | (([(a, x)],[]) | ([], [(a, x)])) when (op = Diff || op = Equal) ->
	if d mod a <> 0 then op = Diff
	else if not (Fd.member x (d / a)) then op = Diff
	else raise C.DontKnow
    | _ -> begin
	let inf_sum, sup_sum = compute_inf_sup pos_terms neg_terms in
	match op with
	  Equal ->
	    if inf_sum = sup_sum then inf_sum = d else
	    if sup_sum < d || inf_sum > d then false else
	    raise C.DontKnow
	| Diff ->
 	    if inf_sum = sup_sum then inf_sum <> d else
	    sup_sum < d || inf_sum > d || raise C.DontKnow
	| LessThan ->
	    sup_sum <= d ||
	    (if inf_sum > d then false else raise C.DontKnow) end in

  let update i =
    Fcl_debug.call 'a' (fun s -> fprintf s "%s - before update: %t\n" name fprint);
    assert(i = 0);
    remove_constants ();
    let d = Fcl_stak.get d in
    let result =
    match (Fcl_stak.get pos_terms, Fcl_stak.get neg_terms) with
      ([], []) -> begin
	match op with
	  Diff when d <> 0 -> true
	| Equal when d = 0 -> true
	| LessThan when d >= 0 -> true
	| _ -> Fcl_stak.fail "linear#update []" end
    | (([(a, x)],[]) | ([],[(a, x)])) when op = Equal ->
	if d mod a = 0 then
	  begin Fd.subst x (d/a); true end
	else Fcl_stak.fail "linear#update [(a,x)] Equal"
    | (([(a, x)],[]) | ([],[(a, x)])) when op = Diff ->
	if d mod a <> 0 then true else
	begin match Fd.value x with
	  Unk attr ->
	    Fd.refine x (Fcl_domain.remove (d/a) attr); true
	| Val _ ->
	    Fcl_debug.internal_error
	      "linear#update ([(a, x)]) Diff" end
    (* propagation on domains *)
    | ([(1, x1)], [(-1,x2)]) when op = Equal -> (* x1 = x2 + d *)
	(*** Printf.printf "x1 = x2 + d"; ***)
	begin match Fd.value x1, Fd.value x2 with
	  Unk d1, Unk d2 ->
	    let newd1 = Fcl_domain.intersection d1 (Fcl_domain.plus d2 d) in
	    Fd.refine x1 newd1;
	    let newd2 = Fcl_domain.plus newd1 (-d) in
	    Fd.refine x2 newd2
	| _ -> Fcl_debug.internal_error "Arith 1 -1" end;
	Fd.is_bound x1 && Fd.is_bound x2
    | ([(1, x1); (1, x2)], []) when op = Equal -> (* x1 = d - x2 *)
	begin match Fd.value x1, Fd.value x2 with
	  Unk d1, Unk d2 ->
	    let newd1 =
	      Fcl_domain.intersection d1
		(Fcl_domain.plus (Fcl_domain.minus d2) d) in
	    Fd.refine x1 newd1;
	    let newd2 = Fcl_domain.plus (Fcl_domain.minus newd1) d in
	    Fd.refine x2 newd2
	| _ -> Fcl_debug.internal_error "Arith 1 -1" end;
	Fd.is_bound x1 && Fd.is_bound x2
    | ([], [(-1, x1); (-1, x2)]) when op = Equal -> (* x1 = -d - x2 *)
	begin match Fd.value x1, Fd.value x2 with
	  Unk d1, Unk d2 ->
	    let newd1 =
	      Fcl_domain.intersection d1
		(Fcl_domain.plus (Fcl_domain.minus d2) (-d)) in
	    Fd.refine x1 newd1;
	    let newd2 = Fcl_domain.plus (Fcl_domain.minus newd1) (-d) in
	    Fd.refine x2 newd2;
	| _ -> Fcl_debug.internal_error "Arith 1 -1" end;
	Fd.is_bound x1 && Fd.is_bound x2
    | ([(1, x1)], [(-1,x2)]) when op = LessThan -> (* x1 <= x2 + d *)
	begin match Fd.value x1, Fd.value x2 with
	  Unk a1, Unk a2 ->
	    let (min1, max1) = Fcl_domain.min_max a1
	    and (min2, max2) = Fcl_domain.min_max a2 in
	    if max1 > max2 + d then
	      Fd.refine x1 (Fcl_domain.remove_up (max2+d) a1);
	    if min1 > min2 + d then
	      Fd.refine x2 (Fcl_domain.remove_low (min1-d) a2);
	    (Fd.max x1 <= Fd.min x2 + d)
	| _ -> Fcl_debug.internal_error "Arith 1 -1" end
    | _ ->
	if op = Diff then false	else begin (* waiting for instantiation *)
	  let modif = ref true (* Only for Equal *)
	  and instantiated = ref true
	  and solved = ref false in
	  while !modif || !instantiated do
	    let last_modif = !modif in
	    modif := false;
	    instantiated := false;
	    let (inf_sum, sup_sum) =
	      compute_inf_sup
		(Fcl_stak.get pos_terms) (Fcl_stak.get neg_terms) in
	    let d_inf_sum = d - inf_sum and d_sup_sum = d - sup_sum in
	    if (op = LessThan && 0 <= d_sup_sum) ||
	    (d_sup_sum = 0 && d_inf_sum = 0) then solved := true
	    else if 0 > d_inf_sum || (op = Equal && 0 < d_sup_sum) then
	      Fcl_stak.fail "linear d_inf_sum"
            (* We stop here if only instantiated *)
	    else if last_modif then begin
	      (* Let's update bounds of variables *)
	      let update_pos (a, x) =
		match Fd.value x with
		  Unk domx ->
		    let mi = Fcl_domain.min domx and
			ma = Fcl_domain.max domx in
		    let new_sup = min ma ((d_inf_sum + a*mi)/a) in
		    if op = Equal then begin
		      let new_inf = max mi ((d_sup_sum + a*ma) /+ a) in
		      if new_sup < ma || new_inf > mi then begin
			modif := true;
			Fd.refine_low_up x new_inf new_sup end end
		    else if new_sup < ma then
		      Fd.refine x (Fcl_domain.remove_up new_sup domx);
		    if Fd.is_bound x then instantiated := true
		(* because it may be the last variable of the expression and
		   the constraint is now solved *)
		| Val vx ->
		    let new_sup = (d_inf_sum + a*vx) / a in
		    if new_sup < vx then Fcl_stak.fail "Arith.update_pos sup";
		    if op = Equal then
		      let new_inf = (d_sup_sum + a*vx) /+ a in
		      if new_inf > vx then
			Fcl_stak.fail "Arith.update_pos inf" in
	      List.iter update_pos (Fcl_stak.get pos_terms);

	      let update_neg (a, x) =
		match Fd.value x with
		  Unk domx ->
		    let mi = Fcl_domain.min domx
		    and ma = Fcl_domain.max domx in
		    let new_inf = max mi ((d_inf_sum + a*ma) /+ a) in
		    if op = Equal then begin
		      let new_sup = min ma ((d_sup_sum + a*mi)/a) in
		      if new_sup < ma || new_inf > mi then begin
			modif := true;
			Fd.refine_low_up x new_inf new_sup end end
		    else if new_inf > mi then begin
		      Fcl_debug.call 'a'
			(fun s -> fprintf s
			    "linear#update, refine_low %d\n" new_inf);
		      Fd.refine x (Fcl_domain.remove_low new_inf domx) end;
		    if Fd.is_bound x then instantiated := true
               (* because it may be the last variable of the expression and
		  the constraint is now solved *)
		| Val vx ->
		    let new_inf = (d_inf_sum + a*vx) /+ a in
		    if new_inf > vx then Fcl_stak.fail "Arith.update_neg inf";
		    if op = Equal then
		      let new_sup = (d_sup_sum + a*vx) / a in
		      if new_sup < vx then
			Fcl_stak.fail "Arith.update_neg sup" in
	      List.iter update_neg (Fcl_stak.get neg_terms)
	    end
	  done;
	  !solved
	end in
    Fcl_debug.call 'a' (fun s -> fprintf s "%s - after update: %t\n" name fprint);
    result in

  let init () =
    basic_refinements
      (Fcl_stak.get pos_terms) (Fcl_stak.get neg_terms) (Fcl_stak.get d) op;
    update 0 in

  C.create ~init ~name ~fprint ~check ~not update delay

and cstr (terms : (int*Fd.t) list) op dd =
  let pos_terms, neg_terms = part_pos_neg terms in
  match pos_terms, neg_terms, op, dd with
    [1, x1], [-1, x2], LessThan, d -> (* x1 <= x2 + d *)
      less_than x1 x2 d
  | [1, x1], [k, x2], Equal, 0 when k <> -1 -> (* x1 = -k * x2 *)
      equalc x1 (0 - k) x2
  | [k, x1], [-1, x2], Equal, 0 when k <> 1 -> (* k * x1 = x2 *)
      equalc x2 k x1
  | [1, x1], [k, x2], Diff, 0 when k <> -1 -> (* x1 <> -k * x2 *)
      diffc x1 (0 - k) x2
  | [k, x1], [-1, x2], Diff, 0 when k <> 1 -> (* k * x1 <> x2 *)
      diffc x2 k x1
  | _ -> linear_cstr terms pos_terms neg_terms op dd


(*** Automatic handling of boolean sub-expressions ***)

let boolsum_threshold = ref 5
let get_boolsum_threshold () = !boolsum_threshold
let set_boolsum_threshold x = boolsum_threshold := x

let is_boolean x =
  let min_x, max_x = Fcl_var.Fd.min_max x in min_x = 0 && max_x = 1

(* flatten : ('a * 'b) list -> ('a * 'b list) list -> ('a * 'b) list *)
let rec flatten rest = function
    [] -> rest
  | (a, l) :: ls ->
      List.fold_left (fun r x -> (a,x)::r) (flatten rest ls) l

(* Optimized boolean sums are used for boolean subexprs larger than
   boolsum only *)
let cstr ?(boolsum = !boolsum_threshold) terms op d =
  let (bools, others) = List.partition (fun (_a, x) -> is_boolean x) terms in
  (* partition of bools by coefficient *)
  let h = Hashtbl.create 17 in
  let add (a, x) =
    try
      let refxs = Hashtbl.find h a in refxs := x :: !refxs
    with Not_found -> Hashtbl.add h a (ref [x]) in
  List.iter add bools;
  (* coefficients with less than boolsum_threshold variables
     are discarded, otherwise an optimized boolean sum is build
     and substituted to the whole term *)
  let bool_sums = ref [] and short_bools = ref [] in
  Hashtbl.iter
    (fun a refxs ->
      if List.length !refxs >= boolsum then begin
	let bools = Array.of_list !refxs in
	Fcl_debug.call 'a' (fun c -> Printf.fprintf c "boolean sum (size %d) optimized\n" (Array.length bools));
	let sumxs = Fcl_boolean.sum bools in
	bool_sums := (a, sumxs) :: !bool_sums end
      else short_bools := (a, !refxs) :: !short_bools)
    h;
  let short_bools = flatten [] !short_bools in
  cstr (!bool_sums @ short_bools @ others) op d


(* x1 = x2 + d *)
let shift_cstr x1 x2 d =
  let name = "Fcl_linear.shift" in
  let update _ =
    match Fd.value x1, Fd.value x2 with
      Unk a1, Unk a2 ->
	let max1 = Fcl_domain.max a1
	and max2d = Fcl_domain.max a2 + d in
	if max1 > max2d then
	  Fd.refine x1 (Fcl_domain.remove_up max2d a1)
	else if max1 < max2d then
	  Fd.refine x2 (Fcl_domain.remove_up (max1-d) a2);
	let min2 = Fcl_domain.min a2
	and min1d = Fcl_domain.min a1 - d in
	if min1d < min2 then
	  Fd.refine x1 (Fcl_domain.remove_low (min2+d) a1)
	else if min1d > min2 then
	  Fd.refine x2 (Fcl_domain.remove_low min1d a2);
	max1 <= min2 + d
    | Val v1, Unk _ ->
	Fd.unify x2 (v1-d);
	true
    | Unk _, Val v2 ->
	Fd.unify x1 (v2+d);
	true
    | Val v1, Val v2 ->
	if v1 <> v2+d then Fcl_stak.fail "shift";
	true
  and delay ct =
    Fd.delay [Fd.on_min; Fd.on_max] x1 ct;
    Fd.delay [Fd.on_max; Fd.on_min] x2 ct
  and fprint f =
    Printf.fprintf f "%s: %a = %a + %d" name Fd.fprint x1 Fd.fprint x2 d in
  C.create ~name ~fprint update delay
