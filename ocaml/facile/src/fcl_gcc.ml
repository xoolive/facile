(* $Id: fcl_gcc.ml,v 1.36 2005-12-09 12:43:21 barnier Exp $ *)

module C = Fcl_cstr

open Printf

let trace = Fcl_debug.call 'd'

let print_int_list c l =
  List.iter (fun x -> Printf.fprintf c "%d " x) l;;

let int_min (a : int) b = if a < b then a else b

let tarjan nb_vertices successors root =
  let partition = ref []
  and stack = ref []
  and dfn = Array.make nb_vertices 0
  and num = ref 0 in

  let push x = stack := x :: !stack
  and pop () =
    match !stack with
      [] -> Fcl_debug.internal_error "Gcc.tarjan.pop"
    | x::xs -> stack := xs; x in

  let rec visit vertex =
    push vertex;
    incr num;
    Fcl_debug.call 'T' (fun f -> fprintf f "visit %d: %d\n" vertex !num);
    dfn.(vertex) <- !num;
    let removed, succs = successors vertex in
    let head =
      List.fold_left
      	(fun head succ ->
	  if not (removed succ) then
	    let mi = if dfn.(succ) = 0 then visit succ else dfn.(succ) in
	    Fcl_debug.call 'T' (fun f -> fprintf f "tarjan: %d succ of %d\n" succ vertex);
	    int_min mi head
	  else head)
	dfn.(vertex)
      	succs in
    Fcl_debug.call 'T' (fun f -> fprintf f "(%d:%d) head = %d\n" vertex dfn.(vertex) head);
    if head = dfn.(vertex) then begin
      let rec component ()  =
	let element = pop () in
	dfn.(element) <- max_int;
	if element = vertex then [vertex] else element :: component () in
      partition := component () :: !partition;
    end;
    head
  in
  ignore (visit root);
  !partition;;


open Fcl_var
module D = Fcl_domain

type data_of_value =
    { card : Fd.t;
      mutable nb_pred : int;
      mutable tmp_pred : int;
      value : int }

let none = min_int;;

type level = Basic | Medium | High

let new_gcc vars distribution index_of_value level =
  let k = Array.length vars
  and n = Array.length distribution in

   (* Rep duale des domaines des vars.(i) *)
  let bool_doms = Array.make_matrix k n false
  and size_bool_doms = Array.make k (-1) in

  let maj i =
    let bdi = bool_doms.(i) in
    for k = 0 to Array.length bdi - 1 do bdi.(k) <- false done;
    D.iter (fun vj -> bdi.(index_of_value vj) <- true) (Fd.dom vars.(i));
    size_bool_doms.(i) <- Fd.size vars.(i)

  and flow = Array.make k none

  and data_values =
    Array.map (fun (c, v) -> {value=v;card=c;nb_pred=0;tmp_pred=none}) distribution

  and member_vars i v = bool_doms.(i).(index_of_value v) in

  let check_satisfied () =
    try
      for i = 0 to k - 1 do (* for all vars *)
	match Fd.value vars.(i) with
	  Unk _ -> raise Exit
	| Val vi ->
          (* May occur after Tarjan refinements in case a variable appear in vars
	     AND in cards (e.g. magic sequence): a refinement in cards
	     remove the value of the computed feasible flow *)

	    if vi <> flow.(i) then Fcl_stak.fail "Gcc.check_satisfied"
      done;
      true
    with
      Exit -> false
  in

  let name = "Gcc"
  and delay c =
    Array.iter (fun x -> Fd.delay [Fd.on_refine] x c) vars;
    Array.iter (fun (x,_) -> Fd.delay [Fd.on_min; Fd.on_max] x c) distribution
  and update _ =
(* Vérification de l'ancien flow et suppression des aretes qui ne sont plus
   valables *)

    trace (fun f -> Array.iteri (fun i v -> fprintf f "vars.(%d)=%a(%d) -> %d\n" i Fd.fprint v size_bool_doms.(i) flow.(i)) vars);
    trace (fun f -> Array.iteri (fun i d -> fprintf f "card.(%d value=%d)=%a\n" i d.value Fd.fprint d.card) data_values);

    (* Mise à jour des rep. duales *)
    for i = 0 to k - 1 do
      let s = size_bool_doms.(i) in
      if Fd.size vars.(i) <> s then begin
	maj i;
	Fcl_stak.trail (fun () -> size_bool_doms.(i) <- -1) (*Tout est à refaire *)
      end;
      assert(D.iter (fun v -> if not bool_doms.(i).(index_of_value v) then Fcl_debug.internal_error ("bool_doms "^string_of_int i)) (Fd.dom vars.(i)); true);
    done;

    let data_of_value v = data_values.(index_of_value v) in

    (* On commence par vérifier si l'ancien flow n'est pas toujours correct
       Dans le même temps, on traite les variables instanciées *)
    for j = 0 to n - 1 do data_values.(j).nb_pred <- 0 done;
    let size_flow = ref k in
    (* O(k) *)
    for i = 0 to k - 1 do (* Pour toutes les variables *)
      begin
	match Fd.value vars.(i) with
	  Val vi -> flow.(i) <- vi (* A tentative value which cannot be bad *)
	| Unk _ -> ()
      end;
      if flow.(i) <> none && member_vars i flow.(i) then
	(* Previous flow still ok for this variable *)
	let d = data_of_value flow.(i) in
	if d.nb_pred = Fd.max d.card then begin
          (* Valeur deja saturée; on ne peut pas garder cette affectation
	     pour cette variable *)
	  trace (fun f -> fprintf f "remove(sat) %d from vars.(%d)\n" flow.(i) i);
	  flow.(i) <- none;
	  decr size_flow;
	end else (* OK, on peut garder cette arete du flow *)
	  d.nb_pred <- d.nb_pred + 1
      else begin (* Affectation impossible *)
	trace (fun f -> fprintf f "remove(dom) %d from vars.(%d)\n" flow.(i) i);
	flow.(i) <- none;
	decr size_flow
      end
    done;

    trace (fun f -> fprintf f "Sizeflow=%d\n" !size_flow);

    (* On vérifie ensuite que chaque valeur possède assez de prédecesseurs;
       on accumule les valeurs qui ne respectent pas cette propriété dans
       [required_values] *)

    (* Le tableau des prédécesseurs *)
    let preds = Array.make n [] in
    let init_preds () =
      for i = 0 to k - 1 do
      	D.iter (fun v -> let j = index_of_value v in preds.(j) <- i :: preds.(j)) (Fd.dom vars.(i))
      done in
    init_preds ();

    let required_values = ref [] in (* valeurs a minimum non atteint *)
    for j = 0 to n - 1 do (* Pour toutes les valeurs *)
      let d = data_values.(j) in
      let v = d.value
      and mi = Fd.min d.card in
      if d.nb_pred < mi then begin
	(* On affecte mi predecesseurs supplementaires a cette valeur,
	   eventuellement au detriments des precedentes qui vont devenir
	   "required" pour un feasible flow *)
        (* On pourrait trier les predecesseurs pour mettre les none en tete *)
	let nb_necessaires = (mi - d.nb_pred) in
	let cpt = ref nb_necessaires in
	try
	  List.iter (* Pour tous les prédécesseurs *)
	    (fun i ->
	      if flow.(i) <> v then begin (* sinon rien à changer *)
		if flow.(i) <> none then begin
                (* On reaffecte cette variable *)
		  decr size_flow;
		  let j1 = index_of_value flow.(i) in
		  let d1 = data_values.(j1) in
		  assert(if j1 < j && d1.nb_pred < Fd.min d1.card then List.mem d1.value !required_values else true);
		  (* si j1 > j alors la valeur sera traitée plus tard *)
		  if j1 < j && d1.nb_pred = Fd.min d1.card then
		    required_values := d1.value :: !required_values;
		  d1.nb_pred <- d1.nb_pred - 1
	      	end;
	      	trace (fun f -> fprintf f "augment vars.(%d) -> %d\n" i v);
		flow.(i) <- v;
	      	decr cpt;
	      	if !cpt = 0 then raise Exit
	      end)
	    preds.(j);
	  Fcl_stak.fail "Gcc: not enough predecessors"
	with
	  Exit -> (* OK pour cette valeur, elle a suffisamment de preds *)
	    d.nb_pred <- mi;
	    size_flow := !size_flow + nb_necessaires
      end
    done;

    trace (fun f -> fprintf f "sizeflow=%d required_values=%a\n" !size_flow print_int_list !required_values);
    trace (fun f -> Array.iteri (fun i fi -> fprintf f "flow.(%d)=%d\n" i fi) flow);

    if !size_flow < k then begin
     (* Compute a new flow : Voir feasible*)

      let target_in_domain targets i =
      	let rec cherche = function
	    [] -> raise Not_found
	  | t::ts ->
	      let d = data_of_value t in
	      if t <> flow.(i) && (if d.tmp_pred = none then d.nb_pred < Fd.max d.card else true) && member_vars i t then
	      	t
	      else
	      	cherche ts in
      	cherche targets
      in (* target_in_domain *)

      let augment initial_targets only_once =
	let tmp_flow = Array.make k none in
	for j = 0 to n - 1 do data_values.(j).tmp_pred <- none done;
	let rec longer targets only_once =
	  trace (fun f -> fprintf f "targets=%a\n" print_int_list targets);
	  if targets = [] then Fcl_stak.fail "gcc: targets=[]";

   	  (* Direct edge to a target *)
	  let augmented = ref false in
	  begin
	    try
	      for i = 0 to k - 1 do (* for all variables *)
	      	try
	      	  if flow.(i) = none then begin
	      	    let target = target_in_domain targets i in(* Not_found *)

		    let rec flip_path v =
		      trace (fun f -> fprintf f "flip %d\n" v);
	      	      let d = data_of_value v in
		      if d.tmp_pred = none then begin
                      (* bon pour une fin de chemin augmentant *)
		      	d.nb_pred <- d.nb_pred + 1
		      end else begin
		      	assert(flow.(d.tmp_pred) = v);
		      	flow.(d.tmp_pred) <- tmp_flow.(d.tmp_pred);
		      	flip_path flow.(d.tmp_pred);
		      	d.tmp_pred <- none (* Supression afin qu'il ne soit pas utilisé une seconde fois *)
		      end in
		    trace (fun f -> fprintf f "flip from vars.(%d)\n" i);
		    flip_path target;
	      	    flow.(i) <- target;
	      	    augmented := true;
	      	    incr size_flow;
		    if only_once then raise Exit
	      	  end
	      	with
	      	  Not_found -> ()
	      done;
	    with Exit -> () (* On ne cherchait qu'un chemin *)
	  end;

	  if !augmented then begin
	    () (* C'est fini, on a trouve (au moins) un chemin *)
	  end else begin
 	    (* On cherche des chemins plus long *)
	    trace
	      (fun f ->
		for j = 0 to n-1 do
		  fprintf f "tmp_pred(%d)=%d\n" j data_values.(j).tmp_pred
		done
	      );
	    let new_targets = ref [] in
	    for i = 0 to k - 1 do (* for all variables *)
	      try
	      	if flow.(i) <> none then begin
		  let d = data_of_value flow.(i) in
		  (* Si on peut repartir vers un noeud/valeur libre *)
		  if d.tmp_pred = none && not (List.mem d.value initial_targets) then begin
		    let target = target_in_domain targets i in (*Not_found *)
		    trace (fun f -> fprintf f "vars.(%d)=%a %d \\V %d\n" i Fd.fprint vars.(i) flow.(i) target);
		    tmp_flow.(i) <- target;
		    d.tmp_pred <- i;
		    new_targets := flow.(i) :: !new_targets;
		  end
	      	end
	      with
		Not_found -> () (* Not reachable target *)
	    done;
	    longer !new_targets true
	  end
	in (* longer *)
	longer initial_targets only_once
      in (* augment *)
      (* Compute a feasible flow en cherchant des prédécesseurs aux valeurs
	 n'en n'ayant pas assez *)
      let rec feasible = function
	  [] -> ()
	| v :: vs ->
	    let d = data_of_value v in
	    while d.nb_pred < Fd.min d.card do
	      trace (fun f -> fprintf f "unfeasible, need %d\n" v);
	      (* On ne cherche qu'une seule augmentation a la fois pour
		 donner une chance aux autres valeurs *)
	      augment [v] true;
	      trace (fun f -> fprintf f "augmented to %d\n" !size_flow)
	    done;
	    feasible vs
      in

      if !required_values <> [] then begin
	(* Non feasible flow: on commence par défaire toutes les valeurs
	   non indispensables pour permettre l'augmentation *)
	for i = 0 to k - 1 do
	  if flow.(i) <> none then
	    let d = data_of_value flow.(i) in
	    if d.nb_pred > Fd.min d.card then begin
	      d.nb_pred <- d.nb_pred - 1;
	      flow.(i) <- none;
	      decr size_flow
	    end
	done
      end;
      feasible !required_values;

      (* Compute a maximum flow *)
      while !size_flow < k do
	(* Seules les valeurs non saturées sont candidates à être extrémité
	   de chemin *)
	let targets = ref [] in
	for j = 0 to n - 1 do
	  let d = data_values.(j) in
	  if d.nb_pred < Fd.max d.card then targets := d.value :: !targets
	done;
	augment !targets false;
    	trace (fun s -> Printf.fprintf s"flow: %a" (fun s a -> Array.iter (fun x -> Printf.fprintf s "%d " x) a) flow; Printf.fprintf s "\n"; flush s);
      done
    end;

    if level > Basic then begin

      let vertex_of_index index = index + k in
      (* Processing the predecessors *)
      let successors_of_s = ref [] in
      for j = 0 to n - 1 do
	let d = data_values.(j) in
	if d.nb_pred < Fd.max d.card then
	  successors_of_s := (vertex_of_index j) :: !successors_of_s;
      done;

      (* indices des vertex dans le graphe total
         [0,k-1] : variables
	 [k,k+n-1] : values
	 k+n : s
	 k+n+1 : t
	 *)
      let vertex_t = k + n + 1
      and vertex_s = k + n in
      let is_a_var vertex = assert(0 <= vertex); (vertex < k)
      and is_a_value vertex = (k <= vertex && vertex < k+n) in

      let funfalse = fun _ -> false in
      let succ vertex =
	if vertex = vertex_t then
          (* Only useful as a start point, successor of nobody *)
          (funfalse, Fcl_misc.goedel (fun x y -> x::y) k [])
        else
 	  if vertex = vertex_s then
            (funfalse, !successors_of_s)
          else if is_a_var vertex then
	    (funfalse, [vertex_of_index (index_of_value flow.(vertex))])
	  else (* value *)
	    let d = data_values.(vertex - k) in
	    let ps = preds.(vertex - k)
	    and removed = (* remove the pred. in the flow from the predecessors *)
	      fun i -> is_a_var i && index_of_value flow.(i) = vertex - k in
	    (removed, if d.nb_pred > Fd.min d.card then vertex_s :: ps else ps) in

      let partition = tarjan (k+n+2) succ vertex_t in

      trace (fun f -> fprintf f "partition: "; List.iter (fun c -> fprintf f "["; List.iter (fun x -> fprintf f "%d " x) c; fprintf f "] ") partition; fprintf f "\n");

      let components = Array.make (k+n) (-1) in
      let num = ref 0 in
      List.iter
	(fun compo ->
	  List.iter
	    (fun vertex ->
	      if is_a_var vertex || is_a_value vertex then
		components.(vertex) <- !num)
	    compo;
	  incr num)
      	partition;

      for i = 0 to k - 1 do (* Pour toutes les variables *)
	match Fd.value vars.(i) with
	  Val _ -> ()
	| Unk domi ->
	    let to_remove = ref [] in
	    Fcl_domain.iter
	      (fun v ->
	    	if v <> flow.(i) &&
		  components.(i) <> components.(vertex_of_index (index_of_value v)) then
		  to_remove := v :: !to_remove)
	      domi;
	    if !to_remove <> [] then
	      Fd.refine vars.(i) (D.difference domi (D.create !to_remove))
      done;

      if level > Medium then begin

      (* Mise a jour des cardinaux *)
      (* Variables affectées *)
      	let known_values = Array.make n 0 in
      	for i = 0 to k - 1 do
	  match Fd.value vars.(i) with
	    Val v ->
	      let j = index_of_value v in
	      known_values.(j) <- known_values.(j) + 1
	  | Unk _ -> ()
      	done;
      	for j = 0 to n - 1 do
	  let c = data_values.(j).card in
	  if known_values.(j) > Fd.min c then begin
	    trace (fun f -> fprintf f "value %d updated\n" j);
	    Fd.refine c (D.remove_low known_values.(j) (Fd.dom c))
	  end
      	done;

      (* Pour toutes les composantes connexes, on obtient une equation sur les
	 cardinaux *)
      	List.iter
	  (fun component ->
	    let values_vertex = List.filter is_a_value component in
	    let values_index = List.map (fun i -> i - k) values_vertex in
	    let nb_predecessors, max_sum, min_sum =
	      List.fold_left
	      	(fun (nb_preds, maxs, mins) index ->
		  let d = data_values.(index) in
		  (d.nb_pred + nb_preds,
		   Fd.max d.card + maxs,
		   Fd.min d.card + mins)) (0,0,0)
	      	values_index in

	    trace (fun f -> fprintf f "compo: %d -> %a\n" nb_predecessors print_int_list values_index);

	  (* Il faudrait boucler sur l'iteration suivante jusqu'a atteindre
	     un point fixe. Utile ??? *)
	    List.iter
	      (fun index ->
	      	let d = data_values.(index) in
                let (cardmin, cardmax) = Fd.min_max d.card in
	      	let new_min = nb_predecessors - (max_sum - cardmax)
	      	and new_max = nb_predecessors - (min_sum - cardmin) in
	      	match Fd.value d.card with
		  Val d_card ->
		    if d_card < new_min || new_max < d_card then
		      Fcl_stak.fail "Gcc.d_card"
	      	| Unk _d_card ->
		    if cardmin < new_min || new_max < cardmax then
		      Fd.refine_low_up d.card new_min new_max)
	      values_index)
	  partition
      end (* level > Basic *)
    end; (* level > Medium *)
    check_satisfied () in

  let init () =
    (* Cardinal nul pour les valeurs non présentes *)
    begin
      let union_di =
      	Array.fold_left (fun acc vi -> D.union acc (Fd.dom vi)) D.empty vars in
      Array.iter
      	(fun (c, v) -> if not (D.member union_di v) then Fd.unify c 0)
      	distribution;

      let values = Array.map snd distribution in
      let domain = D.create (Array.to_list values) in
      Array.iter
      	(fun v ->
      	  match Fd.value v with
	    Val x ->
	      if not (D.member domain x) then
                let mesg =
                  Printf.sprintf "Gcc: value %d out of possible values %s"
                    x (D.to_string domain) in
	      	Fcl_stak.fail mesg
      	  | Unk d ->
	      Fd.refine v (D.intersection d domain))
      	vars;

    (* Initialisation de la représentation duale des domaines *)
      for i = 0 to k - 1 do
      	maj i
      done
    end;
    update 0 in

  C.create ~name ~init ~priority:C.later update delay

open Fcl_arith

let cstr ?(level = High) (vars : Fd.t array) (distribution : (Fd.t * int) array) =
  (* Redundant constraint: vars are exactly counted in the distribution *)
  let sum = (sum_fd (Array.map fst distribution) =~ i2e (Array.length vars)) in

  (* Copying of the distribution array to avoid shuffling user's data *)
  let distribution = Array.copy distribution in
  (* Sort and index values *)
  Array.stable_sort (fun (_,v1) (_,v2) -> compare v1 v2) distribution;
  begin try
    for i = 1 to Array.length distribution - 1 do
      if snd distribution.(i) = snd distribution.(i-1) then raise Exit
    done;
  with
    Exit -> invalid_arg "Gcc.cstr: values must be all different"
  end;

  let index_of_value =
    try
      (* valeurs contigues, hashtbl inutile *)
      for i = 1 to Array.length distribution - 1 do
	if not (snd distribution.(i) = snd distribution.(i-1) + 1) then
	  raise Exit
      done;
      let shift = snd distribution.(0) in
      if shift = 0 then (fun v -> v) else
      (fun v -> v - shift)
    with
      Exit ->
    	let size_hashtbl = Array.length distribution in
    	let h = Hashtbl.create size_hashtbl in
    	let index = ref 0 in
    	Array.iter (fun (_c, v) -> Hashtbl.add h v !index; incr index) distribution;
    	(fun v -> Hashtbl.find h v) in

  (* La contrainte *)
  let c = new_gcc vars distribution index_of_value level in

  Fcl_cstr.conjunction [c; sum];;
