(* $Id: fcl_alldiff.ml,v 1.24 2005-12-09 12:43:21 barnier Exp $ *)

module C = Fcl_cstr

open Fcl_var


let such_that f t =
  let n = Array.length t in
  let rec st acc i =
    if i >= 0 then st (if f t.(i) then (i::acc) else acc) (i-1)
    else acc in
  st [] (n-1);;

let is_alldiff (vals : int list) =
  let rec loop = function
      [] | [_] -> true
    | v1 :: ((v2 :: _vs) as v2vs) -> v1 <> v2 && loop v2vs in
  loop (List.sort compare vals)

let new_diff_from_others (vars : Fd.t array) =
  let name = "diff_from_others" in
  let delay x =
    Array.iteri
      (fun i _vi -> Fd.delay [Fd.on_subst] vars.(i) ~waking_id:i x)
      vars

  and update i =
    match Fd.value vars.(i) with
      Val x ->
	for j = 0 to Array.length vars - 1 do
	  if i <> j
	  then match Fd.value vars.(j) with
	    Unk _ -> Fd.remove vars.(j) x
	  | Val y -> if x = y then Fcl_stak.fail name
	done;
  	true
    | Unk _ -> Fcl_debug.internal_error name

  and init () =
    let bounded =
      Array.fold_left
	(fun acc v -> match Fd.value v with Val v -> v :: acc | Unk _ -> acc)
	[] vars in
    if not (is_alldiff bounded) then
      Fcl_stak.fail (Printf.sprintf "%s: init" name)
    else false in

  C.create ~name ~nb_wakings:(Array.length vars) ~priority:C.immediate
    ~init update delay
(*
let are_disjoint_intervals t =
  let rec loop last = function
      [] -> true
    | v :: vs ->
	match Fd.value v with
	  Val v -> last < v && loop v vs
	| Unk a ->
	    let next = Fd.min a in
	    last < next && loop next vs in
  let t2sl =
    List.sort (fun x y -> compare (Fd.min x) (Fd.max y)) (Array.to_list t) in
  loop min_int t2sl
*)

module Int = struct
  type t = int
  let equal (x : int) y = x = y
  let hash (x : int) = Hashtbl.hash x
end

module IntHashtbl = Hashtbl.Make(Int)


let new_diff (vars : Fd.t array) on_event =
  let n = Array.length vars
      (* Une valeur en dehors des valeurs possibles *)
  and valout = Array.fold_left (fun acc v -> min acc (Fd.min v)) max_int vars - 1 in
  let matchingl = Array.make n valout in (* var -> value : not backtrackable *)
  let matchingr = IntHashtbl.create n in(* value -> var : not backtrackable *)

  let name = "Permut.diff"
  and delay x = Array.iter (fun v -> Fd.delay [on_event] v x) vars

  and update _ =
    let getmatchingr i = try IntHashtbl.find matchingr i with Not_found -> -1 in
      (* matching update (values have been removed from the domains) *)
    let lefts = ref [] in
    for i = n-1 downto 0 do (* downto to get lefts ordered *)
      if matchingl.(i) = valout	then
	lefts := i :: !lefts
      else
	match Fd.value vars.(i) with
	  Unk a ->
	    if not (Fcl_domain.member a matchingl.(i)) then begin
	      IntHashtbl.remove matchingr matchingl.(i);
	      matchingl.(i) <- valout;
	      lefts := i :: !lefts end
	| Val x ->
	    if x <> matchingl.(i) then begin
	      let y = getmatchingr x in
	      if y <> -1 then begin (* Value x was already used by y *)
		matchingl.(y) <- valout;
		IntHashtbl.remove matchingr x;
		lefts := y :: !lefts end;
	      if matchingl.(i) <> valout then (*=valout the first time*)
		IntHashtbl.remove matchingr matchingl.(i);
	      matchingl.(i) <- x;
	      assert(not (IntHashtbl.mem matchingr x));
	      IntHashtbl.add matchingr x i end
    done;
    if !lefts <> [] then begin
      let apath = IntHashtbl.create n in
      let getapath i = try IntHashtbl.find apath i with Not_found -> -1 in
      let reset_apath () = IntHashtbl.clear apath in

      let depth_first rights =
	let rec reverse right left =
	  let r = matchingl.(left) in
	  matchingl.(left) <- right;
	  if r <> valout
	  then begin
	    let a = IntHashtbl.find apath r in
	    assert(IntHashtbl.mem matchingr r);
	    IntHashtbl.remove matchingr r;
	    IntHashtbl.add matchingr r a;
	    IntHashtbl.remove apath r;
	    reverse r a
	  end
	and check r =
	  let ar = getapath r in
	  ar <> -1 &&
	  (matchingl.(ar) = valout || check (matchingl.(ar))) in
	List.iter
	  (fun r ->
	    if getmatchingr r = -1 && check r then begin
	      let a = IntHashtbl.find apath r in
	      assert(not (IntHashtbl.mem matchingr r));
	      IntHashtbl.add matchingr r a;
	      IntHashtbl.remove apath r;
	      reverse r a end)
	  rights in

      let rec breadth_first lefts =
	lefts <> [] && begin
	  let ending = ref false
	  and rights = ref []
	  and new_lefts = ref [] in
	  List.iter
	    (fun l ->
	      try
        	Fd.iter
		  (fun r ->
		    if getapath r = -1 then begin
		      assert(not (IntHashtbl.mem apath r));
		      IntHashtbl.add apath r l;
		      rights := r :: !rights;
		      let mr = getmatchingr r in
		      if mr = -1 then raise Exit
		      else new_lefts := mr :: !new_lefts end)
		  vars.(l)
	      with Exit -> ending := true)
	    lefts;
	  if !ending then begin
	    depth_first !rights;
	    let lefts = such_that (fun xi -> xi = valout) matchingl in
	    lefts = [] || (reset_apath (); breadth_first lefts) end
	  else breadth_first !new_lefts
	end in
      if not (breadth_first !lefts) then Fcl_stak.fail "permut"
    end;
    Array.fold_left (fun acc v -> Fd.is_bound v && acc) true vars in

  C.create ~priority:C.later ~name update delay

type algo = Lazy | Bin_matching of Fd.event

let cstr ?(algo = Lazy) vars =
  (* vars is copied to prevent modifications by the user *)
  let vars = Array.copy vars in
  let n = Array.length vars in
  if n <= 1 then Fcl_cstr.one else
  let dfo = new_diff_from_others vars in
  match algo with
    Bin_matching on_event ->
      Fcl_cstr.conjunction [new_diff vars on_event; dfo]
  | Lazy -> dfo
