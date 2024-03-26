(*
$Id: fcl_sorting.ml,v 1.18 2004-08-12 15:22:07 barnier Exp $
*)

(*
  From
    Narrowing a 2$n$-Block of Sorting in ${O}(n\log{n})$
    Noelle Bleuzen Guernalec and Alain Colmerauer
    CP97
*)

module C = Fcl_cstr

open Fcl_var

let inverse p =
  let n = Array.length p in
  let inv = Array.make n (-1) in
  Array.iteri (fun i x -> inv.(x) <- i) p;
  inv;;

let refine_interval ai inf sup =
  match Fd.value ai with
    Val x -> if x < inf || x > sup then Fcl_stak.fail "Narrow.refine_interval"
  | Unk _ai_ -> Fd.refine_low_up ai inf sup;;

(* d = sort(a); a.(i) = d.(p.(i)) *)
let new_sort a p d =
  let pi = Array.init (Array.length a) (fun x -> x)
  and pi' = Array.init (Array.length a) (fun x -> x)
  and n = Array.length a in

  assert(Array.length a = Array.length d);

  let name = "sort"

  and delay x =
    Array.iter (fun v -> Fd.delay [Fd.on_min; Fd.on_max] v x) a;
    Array.iter (fun v -> Fd.delay [Fd.on_min; Fd.on_max] v x) d

  and update _ =
    Fcl_debug.call 'S'
      (fun c ->
	Printf.fprintf c "Unsorted: "; Array.iter (fun v -> Printf.fprintf c "%a " Fd.fprint v) a; Printf.fprintf c "\n";
	Printf.fprintf c "Sorted: "; Array.iter (fun v -> Printf.fprintf c "%a " Fd.fprint v) d; Printf.fprintf c "\n");

(* calcul de e (en place dans d) *)
    for i = 1 to n-1 do
      let min = Fd.min d.(i-1)
      and max = Fd.max d.(n-i) in
      if Fd.min d.(i) < min then begin
	match Fd.value d.(i) with
	  Val _ -> Fcl_stak.fail "sort#update d.(i)#Fd.min"
	| Unk _di_ -> Fd.refine_low d.(i) min
      end;
      if Fd.max d.(n-1-i) > max then begin
	match Fd.value d.(n-1-i) with
	  Val _ -> Fcl_stak.fail "sort#update d.(n-1-i)#Fd.max"
	| Unk _di_ -> Fd.refine_up d.(n-1-i) max
      end
    done;

    let e = Array.map (fun d -> (Fd.min d)) d
    and e' = Array.map (fun d -> (Fd.max d)) d in

(* calcul de b et c *)
    let max_a = Array.map (fun x -> (Fd.max x)) a
    and min_a = Array.map (fun x -> (Fd.min x)) a in
    Array.sort (fun x y -> compare max_a.(x) max_a.(y)) pi;
    Array.sort (fun x y -> compare min_a.(x) min_a.(y)) pi';
    let pi'_1 = inverse pi'
    and pi_1 = inverse pi in
    let c i = min_a.(pi'.(i))
    and b' i = max_a.(pi.(i)) in

(* calcul de s (infI, infI') et s' (supI, supI') *)
    let infI = Array.make n (-1) and supI = Array.make n (-1) in

    (* Calcul de s sans tri: On remarque que
        1) e, e' et b' sont déjà triés
        2) c est la version triée de b' par la permutation pi' o pi_1
        c) il n'est pas nécessaire de merger c et e' mais seulement de
           les parcourir en // *)
    (* l: index in e'
       k: index in c
       ais: last values seen in c *)
    let rec compute_infI k l ais =
      if l = n then

	Fcl_stak.fail "Sorting: one min in 'a' is scrictly greater than the max of 'd'";
      let set_infI ()=List.iter (fun i -> infI.(pi_1.(pi'.(i))) <- l) ais in
      if k < n then
	if e'.(l) < c k then begin
	  set_infI ();
	  compute_infI k (l+1) []
	end else
	  compute_infI (k+1) l (k :: ais)
      else
	set_infI () in

    compute_infI 0 0 [];


    (* l: index in e
       k: index in b'
       ais: last values seen in b' *)
    let rec compute_supI k l ais =
      if l < 0 then
	Fcl_stak.fail "Sorting: one max in 'a' is scrictly less than the min of 'd'";
      let setSupI () = List.iter (fun i -> supI.(i) <- l) ais in
      if k >= 0 then
	if e.(l) > b' k then begin
	  setSupI ();
	  compute_supI k (l-1) []
	end else
	  compute_supI (k-1) l (k :: ais)
      else
	setSupI () in

    compute_supI (n-1) (n-1) [];

    let infI' = Array.make n (-1) and supI' = Array.make n (-1) in
    for i = 0 to n-1 do
      let pi_i = pi.(i) in
      if (infI.(i) > supI.(i)) then Fcl_stak.fail "Sorting: infI > supI";

      begin match p with
	Some p -> refine_interval p.(pi_i) infI.(i) supI.(i)
      |	None -> () end;

      let i' = n-1 - pi'_1.(pi_i) in
      infI'.(i') <- n-1 - supI.(i);
      supI'.(i') <- n-1 - infI.(i)
    done;
    let s (i,j) = infI.(i) <= j && j <= supI.(i)
    and s' (i,j) = infI'.(i) <= j && j <= supI'.(i) in


(*** (* graphmin sets, version avec arbre binaire à la heap *)
    let graphmin inf sup =
      let t = Array.make (2*n-1) (-1) in
      let left k = 2*k+1 and right k = 2*k + 2 in
      let leaf k = left k >= 2*n-1 in

      let rec build_tree i k =
    (* remplir le sous-arbre k des valeurs >= i *)
    (* renvoyer la feuille la plus grande du sous-arbre *)
    	if leaf k then begin
	  t.(k) <- i;
	  i
    	end else begin
	  let j = build_tree i (left k) in
	  let j' = build_tree (j+1) (right k) in
	  t.(k) <- j';
	  j'
    	end in
      let n' = build_tree 0 0 in
      assert(n' = n - 1);

      let none = -1 in

      let pick inf sup = (* on cherche un element entre inf et sup *)
	let rec loop k =
	  assert(t.(k) <> none);
	  if leaf k then
	    let x = t.(k) in
	    if x <= sup then begin
	      t.(k) <- none;
	      (x, -1)
	    end else (Printf.printf "."; flush stdout; Fcl_stak.fail "Narrow.sort.pick")
	  else
	    let l = left k and r = right k in
	    match (t.(l), t.(r)) with
	      (-1, -1) -> Fcl_debug.internal_error "Sorting: pick none none"
	    | (-1, _) ->
		let (result, max) = loop r in
		t.(k) <- max;
		(result, max)
	    | (_, -1) ->
		let (result, max) = loop l in
	     	t.(k) <- max;
		(result, max)
	    | _ ->
	      	let x = t.(k) in
	      	let ll = t.(l) in
	      	if inf <= ll then
		  let (result, max) = loop l in
		  (result, x)
	      	else
		  let r = right k in
		  let (result, max) = loop r in
		  t.(k) <- if max = -1 then t.(l) else max;
		  (result, t.(k)) in
	loop 0 in

      Array.init n (fun i -> fst (pick inf.(i) sup.(i))) in
***)

    (* Version naive quadratique pour graphmin (30s vs 35s pour atfm) *)
    let graphmin inf sup =
      let b = Array.make n false
      and t = Array.make n (-1) in
      for i = 0 to n - 1 do
	let j = ref inf.(i) in
	while b.(!j) do incr j; if !j > sup.(i) then begin
	  Fcl_stak.fail "graphmin"
	end
	done;
	t.(i) <- !j;
	b.(!j) <- true
      done;
      t in


    let gamma = graphmin infI supI
    and gamma' = graphmin infI' supI' in
    let gamma'' = Array.init n (fun i -> n-1 - gamma'.(n-1-i))
    and gamma_1 = inverse gamma in

    (* Partitioning in stable and shiftable *)
    let decomp s gamma_1 =
      let shiftable k1 k2 = s (gamma_1.(k1), k2) in
      let rec one revx z =
      	if z >= n then
	  two revx z
      	else
	  match revx with
	    [] -> one [z] (z+1)
	  | x::_xs ->
	      if shiftable x z
	      then one (z::revx) (z+1)
	      else two revx z
      and two revx z =
      	if z >= n then
	  [List.rev revx]
      	else
	  let rec remove_y y = function
	      [] -> assert (y <> []); ([], y)
	    | x::xs ->
	      	if shiftable x z
	      	then (x::xs, y)
	      	else remove_y (x::y) xs in
	  let (rest_revx, y) = remove_y [] revx in
	  y :: one rest_revx z in
      one [] 0 in

    let decomps = decomp s gamma_1
    and decomps' = decomp s' (inverse gamma') in

      (* mapmin *)
    let classes decomp =
      let c = Array.make n [||] in
      List.iter (fun sety -> let arrayy = Array.of_list sety in List.iter (fun y -> c.(y) <- arrayy) sety) decomp;
      c in
    let classes_s = classes decomps and classes_s' = classes decomps' in
    let grandk i = ((classes_s).(gamma.(i)))
    and grandk' i = ((classes_s').(gamma'.(i))) in

    let mapmin (inf : int array) sup gk =
      Array.init n (fun i ->
	let t = gk i in

	let rec dicho jmin jmax =
	  if t.(jmin) >= inf.(i) then
	    jmin
	  else
	    if jmin + 1 = jmax then
	      jmax
	    else
	      let j = (jmin + jmax) / 2 in
	      if t.(j) >= inf.(i) then dicho jmin j else dicho j jmax in

	let j = dicho 0 (Array.length t - 1) in
	assert (inf.(i) <= t.(j) && t.(j) <= sup.(i));
	t.(j)) in

    let phi = mapmin infI supI grandk
    and phi' = mapmin infI' supI' grandk' in
    let gamma''_1 = inverse gamma'' in
    for i = 0 to n - 1 do
      refine_interval a.(i) e.(phi.(pi_1.(i))) e'.(n-1 - phi'.(n-1-pi'_1.(i)));
      refine_interval d.(i) (c gamma''_1.(i)) (b' gamma_1.(i))
    done;
    try
      Array.iter (fun ai -> match (Fd.value ai) with Unk _ -> raise Exit | _ -> ()) a;
      true
    with Exit -> false
	in
  C.create ~priority:C.later ~name update delay

open Fcl_arith

(* unification de deux variables du tableau et du tableau trié quand la
   permutation se précise *)
let new_perm p a d =
  let delay x =
    Array.iteri (fun i p_i -> Fd.delay [Fd.on_subst] p_i ~waking_id:i x) p

  and name = "Sorting.perm"

  and update i =
    match Fd.value p.(i) with
      Val p_i ->
  	Fcl_cstr.post (fd2e a.(i) =~ fd2e d.(p_i));
    	true
    | Unk _ -> false in

  C.create ~name ~nb_wakings:(Array.length p) ~priority:C.immediate
    update delay



let cstr a ?(p = None) d =
  let n = Array.length a in
  if n <> Array.length d then
    invalid_arg "Sorting.cstr: arrays have not the same length";
  if n = 0 then
    Fcl_cstr.one
  else begin
    begin
      match p with
	Some perm ->
	  if Array.length perm <> n then
	    invalid_arg "Sorting.cstr: arrays have not the same length";
	  Fcl_cstr.post (new_perm perm a d);
	  Fcl_cstr.post
	    (Fcl_gcc.cstr perm (Array.init n (fun i -> (Fd.int 1, i))))
      | None -> ()
    end;
    new_sort a p d;
  end

let sortp a =
  let n = Array.length a in
  if n = 0 then
    ([||],[||])
  else if n = 1 then
    (a, [|Fd.int 0|])
  else
    let inf, sup =
      Array.fold_left
      	(fun (inf, sup) x -> Stdlib.min (Fd.min x) inf, Stdlib.max (Fd.max x) sup) (max_int, min_int) a in
    let d = Fd.array n inf sup
    and p = Fd.array n 0 (n - 1) in
    Fcl_cstr.post (cstr a ~p:(Some p) d);
    (d, p);;

let sort a =
  let n = Array.length a in
  if n <= 1 then
    a
  else
    let inf, sup =
      Array.fold_left
      	(fun (inf, sup) x -> Stdlib.min (Fd.min x) inf, Stdlib.max (Fd.max x) sup) (max_int, min_int) a in
    let d = Fd.array n inf sup in
    Fcl_cstr.post (cstr a d);
    d;;
