(* $Id: fcl_conjunto.ml,v 1.18 2004-08-12 15:22:07 barnier Exp $ *)

open Fcl_var
module S = Fcl_setDomain.S

(* Gervet C., Interval Propagation to Reason about Sets: Definition
and Implementation of a Practical Language, in CONSTRAINTS journal,
ed. E.C. Freuder,1(3), pp 191-244, 1997
   http://www.icparc.ic.ac.uk/~cg6/ *)

let safe_refine ?(mess = "Conjunto.refine") d glb lub =
  if not (S.subset glb lub) then Fcl_stak.fail mess;
  SetFd.refine d (Fcl_setDomain.unsafe_interval glb lub)

let refine_low_up x inf sup =
  match Fd.value x with
    Val v -> if v < inf || v > sup then Fcl_stak.fail "Conjunto.refine_low_up"
  | Unk _ax -> Fd.refine_low_up x inf sup

let size_cstr d cardinal =
  let update _x =
    let (m,n) = Fd.min_max cardinal
    and glb, lub = SetFd.min_max d in
    let glb_size = S.cardinal glb
    and lub_size = S.cardinal lub in
    (* Rule T12 *)
    let m' = max m glb_size
    and n' = min n lub_size in
    if n' < m' then Fcl_stak.fail "cardinal";
    if m' <> m || n' <> n then refine_low_up cardinal m' n';
    if glb_size = n' then begin     (* Rule T13 *)
      SetFd.unify d glb;
      Fd.unify cardinal n'; (* + T12 *)
      true
    end else if lub_size = m' then begin    (* Rule T14 *)
      SetFd.unify d lub;
      Fd.unify cardinal m'; (* + T12 *)
      true
    end else false
  and delay ct =
    Fd.delay [Fd.on_min;Fd.on_max] cardinal ct;
    SetFd.delay [SetFd.on_min;SetFd.on_max] d ct in
  Fcl_cstr.create ~name:"Conjunto.cardinal" update delay;;


let cardinal d =
  let glb, lub = SetFd.min_max d in
  let glb_size = S.cardinal glb
  and lub_size = S.cardinal lub in
  let name = Printf.sprintf "card(%s)" (SetFd.name d) in
  let c = Fd.interval ~name glb_size lub_size in
  Fcl_cstr.post (size_cstr d c);
  c

let inter_cstr d1 d2 d =
  let update _waking =
    Fcl_debug.call 's' (fun c -> Printf.fprintf c ">inter_cstr %a %a %a\n" SetFd.fprint d1  SetFd.fprint d2  SetFd.fprint d);
    let glb1, lub1 = SetFd.min_max d1
    and glb2, lub2 = SetFd.min_max d2
    and glb, lub = SetFd.min_max d in

    (* T7 *)
    let glb1' = S.union glb1 glb
    and lub1' = S.diff lub1 (S.diff glb2 lub) in
    safe_refine ~mess:"Conjunto.inter T7 1" d1 glb1' lub1';

    let glb2' = S.union glb2 glb
    and lub2' = S.diff lub2 (S.diff glb1 lub) in
    safe_refine ~mess:"Conjunto.inter T7 2" d2 glb2' lub2';

    (* T8 *)
    let glb' = S.inter glb1' glb2' (* glb is alredy in glb1' and glb2' *)
    and lub' = S.inter lub (S.inter lub1' lub2') in
    assert(S.subset glb' lub');
    SetFd.refine d (Fcl_setDomain.interval glb' lub');
    (* Fixpoint ? *)

    Fcl_debug.call 's' (fun c -> Printf.fprintf c "<inter_cstr %a %a %a\n" SetFd.fprint d1  SetFd.fprint d2  SetFd.fprint d);

    not (SetFd.is_var d || SetFd.is_var d1 || SetFd.is_var d2) (* better ? *)

  and delay ct =
    SetFd.delay [SetFd.on_min;SetFd.on_max] d ct;
    SetFd.delay [SetFd.on_min;SetFd.on_max] d1 ct;
    SetFd.delay [SetFd.on_min;SetFd.on_max] d2 ct in

  Fcl_cstr.create ~name:"Conjunto.inter" update delay;;

let inter d1 d2 =
  let glb1, lub1 = SetFd.min_max d1
  and glb2, lub2 = SetFd.min_max d2 in
  let d = SetFd.interval (S.inter glb1 glb2) (S.inter lub1 lub2) in
  Fcl_cstr.post (inter_cstr d1 d2 d);
  d;;

let union_cstr d1 d2 d =
  let update _ =
    let glb1, lub1 = SetFd.min_max d1
    and glb2, lub2 = SetFd.min_max d2
    and glb, lub = SetFd.min_max d in

    (* T5 *)
    let glb1' = S.union glb1 (S.diff glb lub2)
    and lub1' = S.inter lub1 lub in
    safe_refine ~mess:"Conjunto.union T5 1" d1 glb1' lub1';

    let glb2' = S.union glb2 (S.diff glb lub1)
    and lub2' = S.inter lub2 lub in
    safe_refine ~mess:"Conjunto.union T5 2" d2 glb2' lub2';

    (* T6 *)
    let glb' = S.union glb (S.union glb1' glb2')
    and lub' = S.union lub1' lub2' in (* lub is already in lub1' and lub2' *)
    assert(S.subset glb' lub');
    SetFd.refine d (Fcl_setDomain.interval glb' lub');

    not (SetFd.is_var d || SetFd.is_var d1 || SetFd.is_var d2) (* better ? *)

  and delay ct =
    SetFd.delay [SetFd.on_min;SetFd.on_max] d ct;
    SetFd.delay [SetFd.on_min;SetFd.on_max] d1 ct;
    SetFd.delay [SetFd.on_min;SetFd.on_max] d2 ct in

  Fcl_cstr.create ~name:"Conjunto.union" update delay;;

let union d1 d2 =
  let glb1, lub1 = SetFd.min_max d1
  and glb2, lub2 = SetFd.min_max d2 in
  let d = SetFd.interval (S.union glb1 glb2) (S.union lub1 lub2) in
  Fcl_cstr.post (union_cstr d1 d2 d);
  d;;

let ith_diff_from_others ds i =
  let update _ =
    let glbi = SetFd.min ds.(i) in
    for j = 0 to Array.length ds -1 do
      if j <> i then
	let glbj, lubj = SetFd.min_max ds.(j) in
	(* Rule T3 (T4) *)
	let lubj' = S.diff lubj glbi in
	if not (S.subset glbj lubj') then Fcl_stak.fail "Conjunto.disjoint";
	SetFd.refine ds.(j) (Fcl_setDomain.interval glbj lubj')
    done;
    not (SetFd.is_var ds.(i))
  and delay ct =
    SetFd.delay [SetFd.on_min] ds.(i) ct in
  Fcl_cstr.create ~name:"Conjunto.ith_diff_from_others" update delay;;

let all_disjoint ds =
  Fcl_cstr.conjunction
    (Fcl_misc.goedel
       (fun i r -> ith_diff_from_others ds i :: r) (Array.length ds) [])

let inside x d =
  let glb,lub = SetFd.min_max d in
  if not (S.mem x lub) then Fcl_stak.fail "inside";
  if not (S.mem x glb) then
    SetFd.refine d (Fcl_setDomain.interval (S.add x glb) lub);;

let outside x d =
  let glb,lub = SetFd.min_max d in
  if S.mem x glb then Fcl_stak.fail "outside";
  if S.mem x lub then
    SetFd.refine d (Fcl_setDomain.interval glb (S.remove x lub));;

let smallest_cstr d x =
  let update _ =
    let glb, lub = SetFd.min_max d in
    if S.is_empty lub then
      Fcl_debug.fatal_error "Conjunto.smallest: empty set";
    if S.is_empty glb then
      refine_low_up x (max (Fd.min x) (S.min_elt lub)) (S.max_elt lub)
    else
      refine_low_up x (S.min_elt lub) (S.min_elt glb);


    let mi = Fd.min x in
    let lub' = S.remove_low mi lub in
    if lub <> lub' then
      SetFd.refine d (Fcl_setDomain.interval glb lub');

    not (Fd.is_var x)

  and delay ct =
    SetFd.delay [SetFd.on_min; SetFd.on_max] d ct;
    Fd.delay [Fd.on_min] x ct

  in

  Fcl_cstr.create ~name:"Conjunto.smallest" update delay;;

let smallest d =
  let lub = SetFd.max d in
  let x = Fd.interval (S.min_elt lub) (S.max_elt lub) in
  Fcl_cstr.post (smallest_cstr d x);
  x;;

let disjoint d1 d2 = all_disjoint [|d1; d2|];;

let subset d1 d2 =
  let update _ =
    let glb1, lub1 = SetFd.min_max d1
    and glb2, lub2 = SetFd.min_max d2 in

     (* Rule T1: *)
    let lub1' = S.inter lub1 lub2 in
    safe_refine ~mess:"Conjunto.subset T1" d1 glb1 lub1';

     (* Rule T2 *)
    let glb2' = S.union glb2 glb1 in
    safe_refine ~mess:"Conjunto.subset T2" d2 glb2' lub2;

    not (SetFd.is_var d1) || not (SetFd.is_var d2)

  and delay ct =
    SetFd.delay [SetFd.on_max] d1 ct;
    SetFd.delay [SetFd.on_min] d2 ct in

  Fcl_cstr.create ~name:"Conjunto.subset" update delay;;

let mem_check x d () =
  let glb, lub = SetFd.min_max d in
  match Fd.value x with
    Val x ->
      if S.mem x glb then
 	true
      else if not (S.mem x lub) then
	false
      else
	raise Fcl_cstr.DontKnow
  | Unk domx ->
      if S.subset domx glb then
	true
      else if S.is_empty (S.inter domx lub) then
	false
      else
      	raise Fcl_cstr.DontKnow

let rec mem x d =
  let update _ =
    match Fd.value x with
      Val x ->
	inside x d;
	true
    | Unk ax ->
	Fd.refine x (S.inter (SetFd.max d) ax);
	not (Fd.is_var x) || not (SetFd.is_var d)

  and delay ct =
    SetFd.delay [SetFd.on_max] d ct;
    Fd.delay [Fd.on_refine] x ct

  and not () = not_mem x d in

  Fcl_cstr.create ~name:"Conjunto.mem" ~check:(mem_check x d) ~not update delay

and not_mem x d =
  let update _ =
    match Fd.value x with
      Val x ->
	outside x d;
	true
    | Unk ax ->
	Fd.refine x (Fcl_domain.diff ax (SetFd.min d));
	not (Fd.is_var x) || not (SetFd.is_var d)

  and delay ct =
    SetFd.delay [SetFd.on_min] d ct;
    Fd.delay [Fd.on_refine] x ct

  and check () = not (mem_check x d ())

  and not () = mem x d in

  Fcl_cstr.create ~name:"Conjunto.not_mem" ~check ~not update delay

(* Order based on the minimum element. The empty set is the smallest element. *)
let inf_min d1 d2 =

  let update _ =
    let glb1, lub1 = SetFd.min_max d1
    and glb2, lub2 = SetFd.min_max d2 in
    if S.is_empty lub1 then
      true
    else if S.is_empty lub2 && not (S.is_empty glb1) then
      Fcl_stak.fail "Conjunto.inf_min: d2 empty"
    else if not (S.is_empty glb1) && S.min_elt glb1 <= S.min_elt lub2 then
      true
    else if not (S.is_empty glb2) && S.min_elt lub1 > S.min_elt glb2 then
      Fcl_stak.fail "Conjunto.inf_min: d1 > d2"
    else if not (S.is_empty lub2) && S.min_elt lub2 < S.min_elt lub1 then begin
      let lub2' = S.remove_low (S.min_elt lub1) lub2 in
      SetFd.refine d2 (Fcl_setDomain.interval glb2 lub2');
      false end
    else false

  and delay ct =
    SetFd.delay [SetFd.on_min; SetFd.on_max] d1 ct;
    SetFd.delay [SetFd.on_min; SetFd.on_max] d2 ct in

  Fcl_cstr.create ~name:"Conjunto.inf_min" update delay;;

(* Order like Domain.compare *)
let order_with_card d1 card1 d2 card2 =
  Fcl_cstr.post (Fcl_arith.(<=~) (Fcl_arith.fd2e card1) (Fcl_arith.fd2e card2));

  let update _ =
    if Fd.max card1 < Fd.min card2 then true else

    match SetFd.value d1, SetFd.value d2 with
      Val v1, Val v2 -> begin
        (* equal cards :
	   if d1 and d2 are ground, their cards too (priority later)
	   and with the preceding test they should be equal (otherwise
	   true or failure) *)
	assert (not (Fd.is_var card1 || Fd.is_var card2)
		  && (Fd.elt_value card1 = Fd.elt_value card2));
	S.compare v1 v2 <= 0 || Fcl_stak.fail "Conjunto.order" end
    | _, _ -> false

  and delay ct =
    SetFd.delay [SetFd.on_subst] d1 ct;
    SetFd.delay [SetFd.on_subst] d2 ct in

  Fcl_cstr.create ~name:"Conjunto.order" ~priority:Fcl_cstr.later update delay;;

let order d1 d2 =
  let card1 = cardinal d1 and card2 = cardinal d2 in
  order_with_card d1 card1 d2 card2

(* Member *)

let unicise l =
  let sl = List.sort S.compare l in
  let rec loop = function
      [] -> []
    | [_] as le -> le
    | e1 :: ((e2 :: _) as e2r) ->
	if S.compare e1 e2 = 0 then loop e2r else e1 :: loop e2r in
  loop sl

let filter glb lub sets =
  List.filter (fun set -> S.subset glb set && S.subset set lub) sets

let member s sets =
  (* On pourrait faire l'intersection des sets pour rajouter des éléments
     dans glb et leur union pour en enlever dans lub, mais c'est coûteux - il
     faudrait la recalculer à chaque fois et on en a pas besoin pour la
     sectorisation *)
  let sets = unicise sets in
  let sets = Fcl_stak.ref sets in

  let update _ =
    match SetFd.value s with
      Val sv ->
	List.exists (fun set -> S.compare set sv = 0) (Fcl_stak.get sets) || Fcl_stak.fail "Conjunto.member"
    | Unk attr ->
	let (glb, lub) = Fcl_setDomain.min_max attr in
	let new_sets = filter glb lub (Fcl_stak.get sets) in
	match new_sets with
	  [] -> Fcl_stak.fail "Conjunto.member"
	| [set] -> SetFd.unify s set; true
	| _ -> Fcl_stak.set sets new_sets; false

  and delay ct =
    SetFd.delay [SetFd.on_min; SetFd.on_max] s ct in

  Fcl_cstr.create ~name:"Conjunto.member" update delay


(* sum_weight *)

(* Un seul binding par clé et toutes les données positives *)
let check_hash h =
  try
    Hashtbl.iter
      (fun k d ->
	match Hashtbl.find_all h k with
	  [_] -> if d < 0 then raise Exit
	| _ -> raise Exit)
      h;
    true
  with Exit -> false

let check_dom lub h =
  try
    S.iter (fun k -> ignore (Hashtbl.find h k)) lub;
    true
  with Not_found -> false

let sum_weight_cstr s weights_list sum =
  let weights = Hashtbl.create (List.length weights_list) in
  List.iter (fun (k, d) -> Hashtbl.add weights k d) weights_list;

  (* s ne doit pas adresser d'éléments en dehors de weights et tous
     les poids doivent être positifs. De plus, les bindings doivent être
     unique dans weights. *)
  assert
    (let lub = SetFd.max s in check_dom lub weights && check_hash weights);

  let f d =
    (* S.fold pas encore implanté *)
    (* S.fold (fun i r -> Hashtbl.find weights i + r) d 0 in *)
    let fd = ref 0 in
    S.iter (fun i -> fd := !fd + Hashtbl.find weights i) d;
    !fd in

  let update _ =
    let m, n = Fd.min_max sum
    and a, b = SetFd.min_max s in
    let fa = f a and fb = f b in
    (* Rule I3 *)
    let m' = max m fa and n' = min n fb in
    if n' < m' then Fcl_stak.fail "Conjunto.sum_weight";
    if m' <> m || n' <> n then refine_low_up sum m' n';
    (* Rule I4 *)
    if n' = fa then begin SetFd.unify s a; Fd.unify sum fa; true end
    (* Rule I5 *)
    else if m' = fb then begin SetFd.unify s b; Fd.unify sum fb; true end
    else false

  and delay ct =
    SetFd.delay [SetFd.on_min; SetFd.on_max] s ct;
    Fd.delay [Fd.on_min; Fd.on_max] sum ct in

  Fcl_cstr.create ~name:"Conjunto.sum_weight" update delay

let sum_weight s weights =
  let sum = Fd.create (Fcl_domain.int) in
  Fcl_cstr.post (sum_weight_cstr s weights sum);
  sum


(* [sets] must have cardinality [c] must intersect pairwise in atmost one
 element *)
let atmost1 sets c =
  let n = Array.length sets in
  let values =
    Array.fold_right
      (fun x r -> S.union (SetFd.max x) r) sets S.empty in
  let lubs = Array.map Fcl_invariant.SetFd.max sets
  and glbs = Array.map Fcl_invariant.SetFd.min sets in


  S.iter
    (fun a ->
      let cardS_a =
    	Fcl_invariant.sum (Array.map (Fcl_invariant.unary (fun glbsi -> if S.mem a glbsi then 1 else 0)) glbs)

      and bigTa =
	Array.fold_right
	  (Fcl_invariant.binary
	      (fun lubsi r ->
	    	if S.mem a lubsi
	    	then S.union lubsi r
	    	else r))
	  lubs
	  (Fcl_invariant.constant S.empty)
      in
      let maxa = Fcl_invariant.unary (fun x -> (S.cardinal x-1)/(c-1)) bigTa in
      ignore(Fcl_invariant.binary
	       (fun maxa cardS_a ->
		 Fcl_debug.call 's' (fun c -> Printf.fprintf c "atmost1 max%d=%d cardS_%d=%d\n" a maxa a cardS_a);
		 if maxa < cardS_a then Fcl_stak.fail "atmost1";
		 if maxa = cardS_a then begin
		   for i = 0 to n - 1 do
		     let glbi = Fcl_invariant.get glbs.(i) in
		     if not (S.mem a glbi) then begin
		       Fcl_debug.call 's' (fun c -> Printf.fprintf c "atmost1 remove %d from %a\n" a SetFd.fprint sets.(i));
		       outside a sets.(i)
		     end
		   done
		 end)
	       maxa cardS_a))
    values;;
