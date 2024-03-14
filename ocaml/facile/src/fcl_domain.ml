(* $Id: fcl_domain.ml,v 1.40 2011-03-10 16:37:24 barnier Exp $ *)

(* Un domaine est une liste triée d'intervalles et de valeurs
   Sont attachés au domaine sa taille et son maximum (significatif si size<>0) *)


open Fcl_misc.Operators

type elt = int
type elt_list = N | C of int * int * elt_list

let rec list_iter f = function
    N -> ()
  | C(x,y,l) -> f x y; list_iter f l

type t = {domain : elt_list; size : int; max : int; min : int}

let empty = { domain=N; size=0; max=min_int; min=max_int}
let boolean = { domain=C(0,1,N); size=2; max=1; min=0}

let iter f = fun d ->
  list_iter (fun mi ma -> for i = mi to ma do f i done) d.domain

let interval_iter f d = list_iter f d.domain

let rec list_fold_right f l empty =
  match l with
    N -> empty
  | C (x,y,l) -> f x y (list_fold_right f l empty)

let interval_fold_right f d empty =
  list_fold_right f d.domain empty

let rec list_fold_left f accu l =
  match l with
    N -> accu
  | C (x,y,l) -> list_fold_left f (f accu x y) l

let interval_fold_left f accu d =
  list_fold_left f accu d.domain

let for_exists p lb ub =
  let rec for_exists_rec i =
    if i > ub then None else if p i then Some i else for_exists_rec (i + 1) in
  for_exists_rec lb

let arg_exists_opt p d =
  let rec arg_exists_opt_rec l =
    match l with
      N -> None
    | C(lb, ub, t) ->
        match for_exists p lb ub with
          None -> arg_exists_opt_rec t
        | si -> si in
  arg_exists_opt_rec d.domain

let interval_list d =
  let rec iter = function
      N -> []
    | C (x,y,l) -> (x,y) :: iter l in
  iter d.domain

let fprint_elt c x = Printf.fprintf c "%d" x
let fprint c d =
  let print_one mi ma =
    if mi <> ma then Printf.fprintf c "%d..%d" mi ma
    else Printf.fprintf c "%d" mi in
  let rec pr = function
      N  -> Printf.fprintf c "]"
    | C(x,y,xs) ->
	print_one x y;
	match xs with
	  N -> Printf.fprintf c "]"
	| _ -> Printf.fprintf c ";"; pr xs in
  Printf.fprintf c "["; pr d.domain

let to_string d =
  let print_one = fun mi ma ->
      if mi = ma then Printf.sprintf "%d" mi
      else if ma = mi + 1 then Printf.sprintf "%d;%d" mi ma
      else Printf.sprintf "%d-%d" mi ma in
  let rec pr = function
      N  -> "]"
    | C(x,y,xs) ->
	print_one x y ^
	match xs with
	  N -> "]"
	| _ -> ";" ^ pr xs in
 "[" ^ pr d.domain

let c mi ma reste = C(mi,ma,reste)

let cons mi ma reste =
  if mi <= ma then C(mi,ma,reste) else reste

let process_max l =
  let rec pm m = function
      N -> m
    | C(_, x, es) -> pm x es in
  pm min_int l

let get_min = function
    N -> Fcl_debug.internal_error "Domain.get_min"
  | C(m, _,_) -> m

let process_size l =
  let rec ps s = function
    N -> s
  | C(mi,ma,es) -> ps (ma-mi+1 + s) es in
  ps 0 l

type size = int
let size d = d.size

let min d =
  assert (d.domain <> N);
  d.min

let max d =
  assert (d.domain <> N);
  d.max

let min_max d =
  assert (d.domain <> N);
  (d.min, d.max)

let member {domain = l; max = m; _} x =
  x = m ||
  (x < m &&
   let rec member = function
       N -> false
     | C(mi,ma,es) ->
	 if x <= ma
	 then x >= mi
	 else member es in
   member l)
let mem x d = member d x

let unsafe_create d =
  match d with
    [] -> empty
  | x::xs ->
      let max,size = Fcl_misc.last_and_length d in
      let rec make mi last = function
	  [] -> C(mi,last,N)
    	| n::ns ->
	    assert(Fcl_debug.print_in_assert (n > last) "Bad usage of \"Domain.unsafe_create\"");
	    if n = last+1
	    then make mi (last+1) ns
	    else C(mi,last,make n n ns) in
      {domain=make x x xs;size=size;max=max; min = x}

let create d =
  let d = Fcl_misc.sort_unique d in
  unsafe_create d

let interval_unsafe min max =
  {domain=C(min,max,N);size=max-min+1;max=max; min = min}

let interval min max =
  if min > max then invalid_arg "Domain.interval: min > max";
  interval_unsafe min max

let elt x = {domain=C(x,x,N); size=1; max=x; min=x}

let int = interval_unsafe (min_int/3) (max_int/3)

let is_empty d = d.size = 0

let is_bound d = d.size = 1

let elt_value d =
  if is_bound d then d.min else
  let mesg = Printf.sprintf "Domain.elt_value: not singleton" in
  Fcl_debug.fatal_error mesg

let dom_changed old new_ = new_.size < old.size

let min_changed old new_ = old.min < new_.min

let max_changed old new_ = old.max > new_.max

let remove x ({domain = l;max = m; size = s; min = min_d} as d) =
  if x < min_d || x > m then d
  else begin
    let rec remo = function
	N -> raise Not_found
      | C(mi,ma,es) ->
	  if x <= ma then
	    if x >= mi then (cons mi (x-1) (cons (x+1) ma es))
	    else raise Not_found
	  else C(mi,ma,remo es) in
    try
      let newl = remo l in
      if newl = N then empty else
      let newm = if x = m then process_max newl else m in
      let result = {domain=newl;max=newm;size=s-1; min=get_min newl} in
      result
    with
      Not_found -> d
  end

(*
(* trying to optimize new max processing and get rid of exception *)
(* apparently not much better / should be profiled *)
let remove x ({domain = l;max = m; size = s; min = min_d} as d) =
  if x < min_d || x > m then d
  else
    let last = ref None in
    let rec remo l lastmax =
      match l with
	N -> None
      | C (mi, ma, es) ->
        let cxma = compare x ma in
	if cxma < 0 then
	  let cmix = compare mi x in
	  if cmix < 0 then Some (C (mi, x-1, C (x+1, ma, es)))
	  else if cmix = 0 then Some (C (x+1, ma, es))
	  else None
	else if cxma = 0 then
	  if mi = ma then begin
	    if es = N then last := Some lastmax;
	    Some es end
	  else Some (C (mi, x-1, es))
	else
	  match remo es ma with
	    None -> None
	  | Some r -> Some (C (mi, ma, r)) in

    match remo l min_d with
      None -> d
    | Some N -> empty
    | Some (C (mini, _, _) as newl) ->
      let newm = match !last with
	  None -> if x = m then m-1 else m
	| Some newm -> newm in
      {domain = newl; max = newm; size = s - 1; min = mini}
*)

(* Removes values stricly less than x *)
let remove_low x ({domain = l;max = m; size = s;min=min_d} as d) =
  if x <= min_d then d
  else if x = m then {domain=C(m,m,N);max=m;size=1;min=m}
  else if x > m then empty
  else if s = m - min_d + 1 then interval_unsafe x m
  else (* Something is removed and the max remains *)
    let rec rem_low size = function
	N -> Fcl_debug.internal_error "remove_low"
      | C (mi,ma,es) as ees ->
	  if x <= ma then
	    if x > mi then (c x ma es, size - (x-mi))
	    else (ees, size)
	  else rem_low (size-(ma-mi+1)) es in
    let (newl, new_size) = rem_low s l in
    {domain = newl; max = m; size = new_size; min=get_min newl}

(* Removes values stricly greater than x *)
let remove_up x ({domain = l;max = m; size = s;min=min_d} as d) =
  if x >= m then d
  else if x < min_d then empty
  else if s = m - min_d + 1 then interval_unsafe min_d x
  else
    let rec rem = function
	N -> Fcl_debug.internal_error "Domain.remove_up"
      | C (mi,ma,es) ->
	  if mi <= x then
	    if x < ma then c mi x N
	    else C (mi, ma, rem es)
	  else N in
    let newl = rem l in
    {domain = newl; max = process_max newl; size = process_size newl;min=min_d}

let remove_low_up low up d = remove_up up (remove_low low d)

let remove_closed_inter min max ({domain = l;max = ma; min=mi; _} as d) =
  if min > max then d else
  if min <= mi then remove_low (max+1) d
  else if max >= ma then remove_up (min-1) d else
      (* mi < min <= max < ma *)
  let rec rem = function
      N -> N
    | C(mi,ma,es) ->
	  if min <= mi && ma <= max
	  then rem es
	  else if mi <= max && max <= ma || mi <= min && min <= ma
	  then cons mi (min-1) (cons (max+1) ma (rem es))
	  else C(mi,ma,rem es) in
  let newl = rem l in
  {d with domain = newl; size = process_size newl}


let values d =
  let rec enum_and_conc mi ma tail =
    if mi > ma then tail
    else mi::(enum_and_conc (mi+1) ma tail) in
  let rec loop = function
      N -> []
    | C(mi,ma,es) -> enum_and_conc mi ma (loop es) in
  loop d.domain

let intersection ({domain=l1;size=s1; _} as dom1) ({domain=l2;size=s2; _} as dom2) =
  let rec loop l1 l2 =
    match l1, l2 with
      N, _ | _, N -> N
    | C(mi1,ma1,e1s) as c1, (C(mi2,ma2,e2s) as c2)->
	let mi = Fcl_misc.Operators.max mi1 mi2
	and ma = Fcl_misc.Operators.min ma1 ma2 in
	cons mi ma (if ma2 > ma1 then loop e1s c2 else loop c1 e2s) in
  if dom1 == dom2 then dom1 else
  match loop l1 l2 with
    N -> empty
  | l ->
      let s = process_size l in
      if s = s1 then dom1
      else if s = s2 then dom2 else
      {domain=l;size=s; max=process_max l;min=get_min l}

(* On suppose que l'un des domaines est contenu dans l'autre. *)
let difference ({domain = l1; _} as d1) {domain = l2; size = s2; _} =
  let rec loop l1 l2 =
    match l1, l2 with
      l, N -> l
    | C(mi1, ma1, e1s), C(mi2, ma2, e2s) ->
	if ma1 < mi2 then
	  C(mi1, ma1, loop e1s l2)
	else
	  cons mi1 (mi2 - 1) (loop (cons (ma2 + 1) ma1 e1s) e2s)
    | N, C(_, _, _) -> invalid_arg "Domain.difference"
  in
  if s2 = 0 then d1 else
  match loop l1 l2 with
    N -> empty
  | l -> {domain=l;size=process_size l; max=process_max l;min=get_min l}


let diff s1 s2 = difference s1 (intersection s1 s2)

let union d1 d2 =
  let rec loop l1 l2 =
    match l1, l2 with
      N, _ -> l2
    | _, N -> l1
    | C(mi1,ma1,r1), C(mi2,ma2,r2) ->
	if ma1 < mi2 - 1 then C(mi1,ma1,loop r1 l2)
	else if ma2 < mi1 - 1 then C(mi2,ma2,loop l1 r2)
	else if ma1 > ma2 then loop (C(Fcl_misc.Operators.min mi1 mi2, ma1, r1)) r2
	else loop (C(Fcl_misc.Operators.min mi1 mi2, ma2, r2)) r1 in
  match loop d1.domain d2.domain with
    N -> empty
  | l -> {domain=l;size=process_size l; max=process_max l;min=get_min l}

let add x d = union (create [x]) d

let remove_min d =
  match d.domain with
    N -> invalid_arg "Domain.remove_min : empty domain"
  | C(mi,ma,xs) when mi = ma -> begin
      match xs with
	  N -> empty
	| C(new_mi,_,_) ->
	    {domain=xs;max=d.max;size=d.size-1;min=new_mi} end
  | C(mi,ma,xs) ->
      let new_mi = mi + 1 in
      {domain=C(new_mi,ma,xs);max=d.max;size=d.size-1;min=new_mi}

let remove_max d =
  let rec loop = function
      N -> invalid_arg "Domain.remove_max : empty domain"
    | C(mi, ma, N) ->
	assert(ma = d.max);
	cons mi (ma-1) N
    | C(mi, ma, xs) ->
	C(mi, ma, loop xs) in
  match loop d.domain with
    N -> empty
  | l -> {domain=l; size = d.size - 1; max=process_max l; min = d.min}

let included d1 d2 =
  let rec loop l1 l2 =
    match l1, l2 with
      N, _ -> true
    | _, N -> false
    | C(mi1,ma1,r1), C(mi2,ma2,r2) ->
	mi1 >= mi2 && ma1 <= ma2 && loop r1 l2 || loop l1 r2 in
  d1.size <= d2.size && d1.max <= d2.max && loop d1.domain d2.domain

let minus {domain=d; size=s; max=m;min=min_dom} =
  let rec loop l = function
      N -> l
    | C(x, y, r) -> loop (C (-y,-x, l)) r in
  {domain = loop N d; size = s; max = - min_dom; min = - m}

let plus ({domain=d; size=s; max=m;min=min_dom} as dom) b =
  if b = 0 then dom else
  let rec loop = function
      N -> N
    | C(x, y, r) -> C(x+b, y+b, loop r) in
  {domain = loop d; size = s; max = m + b; min = min_dom+b}

(* not tested *)
let times ({domain=d; size=s; max=m;min=min_dom} as dom) = function
    1 -> dom
  | 0 -> {domain = C(0, 0, N); size = 1; max = 0; min = 0}
  | k when k > 0 ->
      let rec loop = function
	  N -> N
	| C(x, y, r) -> C(k*x, k*y, loop r) in
      {domain = loop d; size = k*s; max = k*m; min = k*min_dom}
  | k when k < 0 ->
      let rec loop l = function
	  N -> l
	| C(x, y, r) -> loop (C (k*y, k*x, l)) r in
      {domain = loop N d; size = k*s; max = k*min_dom; min = k*m}
  | _ -> Fcl_debug.internal_error "times"

let smallest_geq {domain=d; max=maxi; _} c =
  let rec loop = function
      N -> Fcl_debug.internal_error "first_geq_value"
    | C(x, y, r) ->
	if x >= c then x
	else if y >= c then c
	else loop r in
  if maxi < c then raise Not_found
  else if maxi = c then c
  else loop d

let greatest_leq {domain=d; max=maxi; min=mini; _} c =
  let rec loop last = function
      N -> Fcl_debug.internal_error "first_leq_value"
    | C(x, y, r) ->
	if x > c then last
	else if y < c then loop y r
	else c in
  if mini > c then raise Not_found
  else if maxi < c then maxi
  else loop mini d

let largest_hole_around {domain=d; max=maxi; min=mini; _} c =
  let rec loop last = function
      N -> Fcl_debug.internal_error "largest_hole_around"
    | C(x, y, r) ->
	if c < x then (last, x) else
	if y < c then loop y r else
	(c, c) in
  if mini <= c && c <= maxi then
    if c = maxi then (c, c) else loop mini d
  else raise Not_found

let smallest_common_elt_greater_than
    ({domain=l1; _} as dom1) ({domain=l2; _} as dom2) lb =
  let rec loop l1 l2 =
    match l1, l2 with
      N, _ | _, N -> None
    | C(mi1,ma1,e1s) as c1, (C(mi2,ma2,e2s) as c2)->
	let mi = Fcl_misc.Operators.max mi1 mi2 in
	let ma = Fcl_misc.Operators.min ma1 ma2 in
        if mi <= ma && ma > lb then
          if mi > lb then Some mi else Some (lb + 1)
        else if ma2 > ma1 then loop e1s c2 else loop c1 e2s in
  if dom1 == dom2 then
    try Some (smallest_geq dom1 (lb + 1)) with Not_found -> None
  else loop l1 l2

let nearest c x y = if c-x > y-c then y else x

let nearest {domain=d; max=maxi; min=mini; _} c =
  let rec loop last = function
      N -> Fcl_debug.internal_error "largest_hole_around"
    | C(x, y, r) ->
	if c < x then nearest c last x else
	if y < c then loop y r else
	c in
  if mini <= c then
    if c > maxi then maxi else
    if c = maxi then c else
    loop mini d
  else mini

let choose order d =
  if size d = 0 then raise Not_found else
  let best = ref (min d) in
  iter (fun x -> if order x !best then best := x) d;
  !best

let random = fun d ->
  let rec choose i = function
    N -> Fcl_debug.internal_error "Domain.random"
  | C(mi, ma, l) ->
      let n = ma - mi + 1 in
      if i < n then mi + i else choose (i-n) l in
  choose (Random.int d.size) d.domain

let strictly_inf (x:int) y = x < y

let compare_elt (x : int) y = compare x y

let compare d1 d2 =
  let rec loop = function
      N, N -> 0
    | N, _ -> -1
    | _, N -> failwith "Fcl_domain.compare"
    | C(x1, y1, l1), C(x2, y2, l2) ->
	let cx = compare x1 x2 in
	if cx = 0 then
	  let cy = compare y2 y1 in
	  if cy = 0 then loop (l1, l2) else cy
	else cx in
  let cs = compare d1.size d2.size in
  if cs = 0 then loop (d1.domain, d2.domain) else cs

let disjoint {domain=l1; max=ma1; _} {domain=l2; max=ma2; _} =
  let rec loop l1 l2 =
    match l1, l2 with
      (N, _ | _, N) -> true
    | C(mi1,ma1,e1s) as c1, (C(mi2,ma2,e2s) as c2)->
	let mi = Fcl_misc.Operators.max mi1 mi2
 	and ma = Fcl_misc.Operators.min ma1 ma2 in
	mi > ma && if ma2 > ma1 then loop e1s c2 else loop c1 e2s in
  (* if l1 and l2 are empty: max1 = max2 *)
  l1 = N || l2 = N || (ma1 <> ma2 && loop l1 l2)
