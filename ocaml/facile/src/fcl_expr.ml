open Fcl_misc.Operators
open Fcl_var

(* This signature corresponds to public functions. *)
module type S = sig
  type t
  val fprint : out_channel -> t -> unit
  val eval : t -> int
  val min_of_expr : t -> int
  val max_of_expr : t -> int
  val min_max_of_expr : t -> (int * int)
end


(* operators *)
type agg_op = Pluse | Multe
type bin_op = Dive | Mode
type un_op = Abse
(* genuine or symbolic auxilliary variable *)
type var = Var of Fd.t | Aux of int
(* compiled expressions *)
(* they are normalized by sorting unordered subexpressions according
   to compare_expr and compare_intexpr *)
(* beware: convention for sums is Sigma (coef_i * exp_i) + cst
   whereas Fcl_linear functions and constraints assume that the constant
   is at the rhs : Sigma (coef_i * exp_i) = cst *)
type t =
  (* sum : op * (coef * exp) list * cst *)
  (* prod : op * (exposant * exp) list * cst *)
    Agg of agg_op * (int * t) list * int
  | Bin of bin_op * t * t
  | Un of un_op * t
  | Inte of int
  | Fde of var

(* let rec iter_last f last = function *)
(*     [] -> () *)
(*   | [x] -> last x *)
(*   | x :: xs -> begin f x; iter_last f last xs end *)

let fprint_var c = function
    Var v -> Fd.fprint c v | Aux id -> Printf.fprintf c "vaux_%d%!" id

type priority = PrioTop | PrioPlus | PrioMul | PrioExp

let rec fprint prio c e =
  let left p = if p < prio then Printf.fprintf c "(%!"
  and right p = if p < prio then Printf.fprintf c ")%!" in
  match e with
    Agg (Pluse, es, ct) -> begin
      left PrioPlus;
      let print (coef, e) =
	match coef with
	  1 -> Printf.fprintf c "%a%!" (fprint PrioPlus) e
	| -1 -> Printf.fprintf c "-%a%!" (fprint PrioMul) e
	| _ -> Printf.fprintf c "%d*%a%!" coef (fprint PrioMul) e in
      begin
	match es with
	  [] -> assert false
      	| e::es ->
	    print e;
	    List.iter (fun e -> Printf.fprintf c " + %!"; print e) es
      end;
      if ct <> 0 then Printf.fprintf c " + %d%!" ct;
      right PrioPlus end
  | Agg (Multe, es, ct) -> begin
      left PrioMul;
      let print (coef, e) =
	if coef <> 1 then Printf.fprintf c "%a ^ %d%!" (fprint PrioExp) e coef
	else Printf.fprintf c "%a%!" (fprint PrioMul) e in
      begin
	match es with
	  [] -> assert false
      	| e::es ->
	    print e;
	    List.iter (fun e -> Printf.fprintf c " * "; print e) es
      end;
      if ct <> 1 then Printf.fprintf c " * %d%!" ct;
      right PrioMul end
  | Bin (op, e1, e2) ->
      let op = match op with Dive -> "/" | Mode -> "%" in
      left PrioMul;
      Printf.fprintf c "%a %s %a%!" (fprint PrioExp) e1 op (fprint PrioExp) e2;
      right PrioMul
  | Un (Abse, e) -> Printf.fprintf c "|%a|%!" (fprint PrioTop) e
  | Inte i -> Printf.fprintf c "%d%!" i
  | Fde v -> fprint_var c v

let fprint = fprint PrioTop

let rec min_max_of_expr = function
    Inte x -> (x, x)
  | Fde x -> begin
      match x with
	Var v -> Fd.min_max v
      | _ -> Fcl_debug.fatal_error "Expr.min_max_of_expr: symbolic variable" end
  | Bin (typ, x1, x2) -> begin
      let (a, b) = min_max_of_expr x1 and (c, d) = min_max_of_expr x2 in
      match typ with
	Dive -> Fcl_nonlinear.min_max_div_inter a b c d
      |	Mode -> Fcl_nonlinear.min_max_mod_inter a b c d end
  | Un (Abse, x) ->
      let (a, b) = min_max_of_expr x in
      Fcl_nonlinear.min_max_abs_inter a b
  | Agg (Pluse, es, c) ->
      let (a, b) =
	List.fold_left
	  (fun (acca, accb) (coef, e) ->
	    let (a, b) = min_max_of_expr e in
	    let (coefa, coefb) = Fcl_nonlinear.min_max_mult_inter a b coef coef in
	    Fcl_linear.min_max_plus_inter coefa coefb acca accb)
	  (0, 0) es in
      Fcl_linear.min_max_plus_inter c c a b
  | Agg (Multe, es, c) ->
      let (a, b) =
	List.fold_left
	  (fun (acca, accb) (n, e) ->
	    let (a, b) = min_max_of_expr e in
	    let (coefa, coefb) = Fcl_nonlinear.min_max_expn_inter a b n in
	    Fcl_nonlinear.min_max_mult_inter coefa coefb acca accb)
	  (1, 1) es in
      Fcl_nonlinear.min_max_mult_inter a b c c

let min_of_expr e = let (a, _) = min_max_of_expr e in a
let max_of_expr e = let (_, b) = min_max_of_expr e in b

let rec eval = function
    Inte x -> x
  | Fde x -> begin
      match x with
	Var v -> begin
	  match Fd.value v with
	    Unk _ ->
	      let msg = Printf.sprintf "Expr.eval: variable %s unknown" (Fd.name v) in
	      Fcl_debug.fatal_error msg
	  | Val i -> i end
      |	_ -> Fcl_debug.fatal_error "Expr.eval: symbolic variable" end
  | Bin (typ, x1, x2) -> begin
      let ex2 = eval x2 in
      if ex2 = 0 then Fcl_debug.fatal_error "Expr.eval: division by zero";
      let ex1 = eval x1 in
      match typ with Dive -> ex1 / ex2 | Mode -> ex1 mod ex2 end
  | Un (Abse, x) -> abs (eval x)
  | Agg (typ, es, c) ->
      let (op, coef_op) =
	match typ with
	  Pluse -> (( + ), ( * )) | Multe -> (( * ), Fcl_nonlinear.expn_int) in
      List.fold_left
	(fun acc (coef, se) -> op acc (coef_op (eval se) coef))
	c es

(* order on lists according to their size then to each element
   according to [cmp] *)
let compare_list cmp lx ly =
  let rec comp_iter lx ly =
    match (lx, ly) with
      ([], []) -> 0
    | (x :: xs, y :: ys) ->
	let cxy = cmp x y in if cxy <> 0 then cxy else comp_iter xs ys
    | _ -> assert false in
  let cs = compare (List.length lx) (List.length ly) in
  if cs <> 0 then cs else comp_iter lx ly

let compare_var x y =
  match (x, y) with
    (Var x, Var y) -> Fd.compare x y
  | (Aux x, Aux y) -> compare x y
  | (Aux _, Var _) -> -1 | (Var _, Aux _) -> 1

(* order on compiled expressions *)
let rec compare_expr x y =
  match (x, y) with
  (* same constructors *)
    (Inte x, Inte y) -> compare x y
  | (Fde x, Fde y) -> compare_var x y
  | (Un (opx, x), Un (opy, y)) ->
      let cop = compare opx opy in
      if cop <> 0 then cop else compare_expr x y
  | (Bin (opx, x1, x2), Bin (opy, y1, y2)) ->
      let cop = compare opx opy in
      if cop <> 0 then cop
      else
	let c1 = compare_expr x1 y1 in
	if c1 <> 0 then c1 else compare_expr x2 y2
  | (Agg (opx, lx, cx), Agg (opy, ly, cy)) ->
      let cop = compare opx opy in
      if cop <> 0 then cop
      else
	let cc = compare cx cy in
	if cc <> 0 then cc
	else
	  (* lx and ly are supposed to be already sorted *)
	  (* let lx = List.sort compare_intexpr lx
	  and ly = List.sort compare_intexpr ly in *)
	  compare_list compare_intexpr lx ly
  (* different constructors *)
  | (Inte _, _) -> -1 | (_, Inte _) -> 1
  | (Fde _, _) -> -1 | (_, Fde _) -> 1
  | (Un _, _) -> -1 | (_, Un _) -> 1
  | (Bin _, _) -> -1 | (_, Bin _) -> 1
(* order on (int * exp) couples *)
and compare_intexpr (cx, ex) (cy, ey) =
  let cc = compare cx cy in
  if cc <> 0 then cc else compare_expr ex ey

type exp = t
(* module parameter for Hashtbl functor *)
module Exp_for_H = struct
  type t = exp
  (* alternative representation of compiled expressions for hashing:
     variables domains are replaced by their id *)
  type varh = Varh of int | Auxh of int
  type th =
      Aggh of agg_op * (int * th) list * int
    | Binh of bin_op * th * th
    | Unh of un_op * th
    | Inteh of int
    | Fdeh of varh

  let rec t2th = function
      Agg (op, l, c) ->
	(* l is supposed to be already sorted *)
	(* let l = List.sort compare_intexpr l in*)
	let lh = List.map (fun (c, e) -> (c, t2th e)) l in
	Aggh (op, lh, c)
    | Bin (op, e1, e2) -> Binh (op, t2th e1, t2th e2)
    | Un (op, e) -> Unh (op, t2th e)
    | Inte c -> Inteh c
    | Fde x -> begin
	match x with
	  Var v -> begin
	    match Fd.value v with
	      Unk _ -> Fdeh (Varh (Fd.id v))
	    | Val c -> Inteh c end
	| Aux i -> Fdeh (Auxh i) end

  let equal x y = compare_expr x y = 0
  let hash e = Hashtbl.hash (t2th e)
end
(* Hashtbl on compiled expressions *)
module HE = Hashtbl.Make(Exp_for_H)

(* deprecated conversion function between former expressions
   representation and current one *)
(*
let rec user2exp = function
    Int i -> Inte i
  | Fd v -> begin match Fd.value v with Val i -> Inte i | _ -> Fde v end
  | Plus (e1, e2) ->
      let l = [(1, user2exp e1); (1, user2exp e2)] in
      let l = List.sort compare_intexpr l in
      Agg (Pluse, l, 0)
  | Mult (e1, e2) ->
      let l = [(1, user2exp e1); (1, user2exp e2)] in
      let l = List.sort compare_intexpr l in
      Agg (Multe, l, 1)
  | Minus e -> Agg (Pluse, [(-1, user2exp e)], 0)
  | Div (e1, e2) -> Bin (Dive, user2exp e1, user2exp e2)
  | Mod (e1, e2) -> Bin (Mode, user2exp e1, user2exp e2)
  | Abs e -> Un (Abse, user2exp e)
*)


let merge es =
  let coef_exps = HE.create 11 in
  let add_or_create (c, e) =
    try
      let oldc = HE.find coef_exps e in
      oldc := !oldc + c
    with Not_found -> HE.add coef_exps e (ref c) in
  List.iter add_or_create es;
  let l =
    HE.fold (fun e c acc -> if !c <> 0 then (!c, e) :: acc else acc)
      coef_exps [] in
  List.sort compare_intexpr l

(* normalize expressions *)
let rec reduce = function
    Inte _ as e -> e | Fde _ as e -> e
  | Bin (Dive, e1, e2) -> begin
      match (reduce e1, reduce e2) with
	(_re1, Inte 0) -> Fcl_debug.fatal_error "Arith.reduce: division by zero"
      | (re1, Inte 1) -> re1
      |	(Inte 0 as re1, _re2) -> re1
      | (Inte i1, Inte i2) -> Inte (i1 / i2)
      |	(re1, re2) -> Bin (Dive, re1, re2) end
  | Bin (Mode, e1, e2) -> begin
      match (reduce e1, reduce e2) with
	(_re1, Inte 0) -> Fcl_debug.fatal_error "Arith.reduce: modulo by zero"
      | (_, Inte 1) -> Inte 0
      |	(Inte 0 as re1, _re2) -> re1
      |	(Inte i1, Inte i2) -> Inte (i1 mod i2)
      |	(re1, re2) -> Bin (Mode, re1, re2) end
  | Un (Abse, e) -> begin
      match reduce e with
	Inte i -> Inte (abs i)
      |	re -> Un (Abse, re) end
  | Agg (typ, es, c) -> begin
      match agg_reduce typ es c with
	(0, _) when typ = Multe -> Inte 0
      |	(rc, []) -> Inte rc
      |	(0, [(1, e)]) when typ = Pluse -> e
      |	(1, [(1, e)]) when typ = Multe -> e
	(* type of aggregate is changed, so it is reduced once more *)
      |	(rc, [(1, e)]) when typ = Multe -> reduce (Agg (Pluse, [(rc, e)], 0))
	(* Pi res * rc -> Sum rc * (Pi res * 1) + 0 *)
      |	(rc, res) when typ = Multe && rc <> 1 ->
	  Agg (Pluse, [(rc, Agg (Multe, res, 1))], 0)
      |	(rc, res) -> Agg (typ, res, rc) end

and agg_reduce typ es c =
  let (op, coef_op) =
    match typ with
      Pluse -> (( + ), ( * )) | Multe -> (( * ), Fcl_nonlinear.expn_int) in
  let rec agg_reduce_rec new_es c = function
      [] -> (c, merge new_es)
    | (0, _e) :: es -> agg_reduce_rec new_es c es
    | (coef, e) :: es -> begin
	match reduce e with
	  (* Pi ... * 0 ^ coef * ... -> 0 *)
	  Inte 0 when typ = Multe -> (0, [])
	| Inte i -> agg_reduce_rec new_es (op c (coef_op i coef)) es
	  (* Sum ... + coef * (Sum ees + ec) + ... -> Sum ... *)
	  (* Pi ... * (ec * Pi ees) ^ coef * ... -> Pi ... *)
	| Agg (etyp, ees, ec) when etyp = typ ->
	    let new_ees = List.map (fun (eec, ee) -> (coef * eec, reduce ee)) ees in
	    agg_reduce_rec (new_ees @ new_es) (op c (coef_op ec coef)) es
	  (* Sum ... + coef * (ec * Pi ee) + ... -> Sum ... + coef * ec * ee + ... *)
	| Agg (Multe, [(1, ee)], ec) when typ = Pluse ->
	    agg_reduce_rec ((coef * ec, ee) :: new_es) c es
	  (* Sum ... + coef * (ec * Pi ees) + ... ->
	     Sum ... + (coef * ec) * (Pi ee) + ... *)
	| Agg (Multe, ees, ec) when typ = Pluse ->
	    agg_reduce_rec ((coef * ec, Agg (Multe, ees, 1)) :: new_es) c es
	  (* Pi ... * (Sum ec * ee) ^ coef * ... ->
	     Pi ... * ee ^ coef * ... * (c * ec ^ coef) *)
	| Agg (Pluse, [(ec, ee)], 0) when typ = Multe ->
	    agg_reduce_rec ((coef, ee) :: new_es) (op c (coef_op ec coef)) es
	| re ->
	    agg_reduce_rec ((coef, re) :: new_es) c es end in
  agg_reduce_rec [] c es


(* compute intermediate equation and symbolic auxilliary variables *)
let equations e =
  (* auxilliary variables are first only named (not created) to
     avoid computing useless expression bounds (allows Linear.basic_refinements
     to be used on plain sums) and variables (the last one for equality
     constraints) *)
  let gen_auxvar = Fcl_misc.gen_int_fun () in
  let eqs = HE.create 11 in
  let add e lhs =
    let vaux = gen_auxvar () in
    HE.add eqs e (vaux, lhs);
    Aux vaux in
  let rec process = function
   (* replace integers by variables inside intermediate equations *)
      Inte i -> Var (Fd.elt i)
    | Fde v -> v
    | e -> begin try let (ve, _eq) = HE.find eqs e in Aux ve with Not_found -> begin
	(* computation of intermediate variables and corresponding equations *)
	(* constraints are not posted yet *)
	match e with
	  Un (Abse, se) -> let vse = process se in add e (Un (Abse, Fde vse))
	| Bin (typ, se1, se2) ->
	    let vse1 = process se1 and vse2 = process se2 in
	    add e (Bin (typ, Fde vse1, Fde vse2))
	| Agg (_, [], _) ->
	    Fcl_debug.internal_error "Expr.equations: empty aggregate list"
	| Agg (Pluse, ses, c) -> begin
	    let vses = List.map (fun (coef, se) -> (coef, Fde (process se))) ses in
	    add e (Agg (Pluse, vses, c)) end
	| Agg (Multe, [(coef, se)], 1) -> begin
	    assert (coef > 1);
	    let vse = process se in
	    add e (Agg (Multe, [(coef, Fde vse)], 1)) end
	| Agg (Multe, ses, 1) -> begin
	    (* intermediate exponentiation *)
	    let vses =
	      List.map
		(function
		    (1, se) as coef_se -> (process se, coef_se)
		  | (coef, se) as coef_se ->
		      let vse_coef = process (Agg (Multe, [(coef, se)], 1)) in
		      (* couples with coef are kept for further hashing
			 of partial products *)
		      (vse_coef, coef_se)) ses in
	    let vses =
	      List.sort
		(fun (_, coef_se1) (_, coef_se2) -> compare_intexpr coef_se1 coef_se2)
		vses in
	    let (vaux, _) = fold_multe vses in
	    vaux end
	| Agg (Multe, _, _c) ->
	    Fcl_debug.internal_error "Expr.equations: non neutral product constant"
	| _ -> assert false end end

  and fold_multe = function
      [(vse, coef_se)] -> (vse, [coef_se])
    | (vse, coef_se) :: rest -> begin
	let (acc_vaux, acc_exp) = fold_multe rest in
	let lhs_list = List.sort compare_intexpr [(1, Fde vse); (1, Fde acc_vaux)] in
	let lhs = Agg (Multe, lhs_list, 1) in
	let vses = coef_se :: acc_exp in
	let e = Agg (Multe, vses, 1) in
	let vaux = add e lhs in
	(vaux, vses) end
    | _ -> Fcl_debug.internal_error "Expr.fold_multe: empty list" in
  let vfinal = process e in
  (vfinal, eqs)

let fprint_eqs c eqs =
  List.iter
    (fun (vaux, lhs) ->
      Printf.fprintf c "%a = %a\n" fprint_var (Aux vaux) fprint lhs) eqs

let bintype2cstr = function
    Dive -> Fcl_nonlinear.division | Mode -> Fcl_nonlinear.modulo

let bintype2aux = function
    Dive -> Fcl_nonlinear.division_aux | Mode -> Fcl_nonlinear.modulo_aux

let emptyh h =
  try HE.iter (fun _ _ -> raise Exit) h; true with Exit -> false

let interm eqs re =
  try let (_, term) = HE.find eqs re in term with Not_found -> re

let get_var vars = function
    Var v -> v | Aux i -> Hashtbl.find vars i

let intfde2intfd vars vses =
  List.map
    (function
	(coef, Fde vse) -> (coef, get_var vars vse)
      | _ -> assert false)
    vses

let post_eqs eqs =
  let vars = Hashtbl.create 11 in
  let eqs_list = HE.fold (fun _ eq acc -> eq :: acc) eqs [] in
  (* equations must be sorted to generate auxilliary variables in the right order *)
  let eqs_sorted = List.sort (fun (i1, _) (i2, _) -> compare i1 i2) eqs_list in
  Fcl_debug.call 'a' (fun s -> Printf.fprintf s "to be posted:\n%a\n" fprint_eqs eqs_sorted);
  List.iter
    (fun (i, lhs) ->
      let cstr =
	match lhs with
	  Un (Abse, Fde vse) -> begin
	    let vse = get_var vars vse in
	    let vaux = Fcl_nonlinear.absolute_aux vse in
	    Hashtbl.add vars i vaux;
	    Fcl_nonlinear.absolute vaux vse end
	| Bin (typ, Fde vse1, Fde vse2) -> begin
	    let vse1 = get_var vars vse1 and vse2 = get_var vars vse2 in
	    let vaux = (bintype2aux typ) vse1 vse2 in
	    Hashtbl.add vars i vaux;
	    (bintype2cstr typ) vaux vse1 vse2 end
	| Agg (Multe, [(coef, Fde vse)], 1) -> begin
	    assert (coef > 1);
	    let vse = get_var vars vse in
	    let vaux = Fcl_nonlinear.expn_aux vse coef in
	    Hashtbl.add vars i vaux;
	    Fcl_nonlinear.expn vaux vse coef end
	| Agg (Pluse, vses, c) -> begin
	    assert (match (vses, c) with ([(1, _)], 0) -> false | _ -> true);
	    let vses = List.map
		(function
		    (coef, Fde vse) -> (coef, get_var vars vse)
		  | _ -> assert false) vses in
	    let vaux = Fcl_linear.linear_aux vses (0 - c) in
	    Hashtbl.add vars i vaux;
	    Fcl_linear.cstr ((-1, vaux) :: vses) Fcl_linear.Equal (0 - c) end
	| Agg (Multe, [(1, Fde vse1); (1, Fde vse2)], 1) -> begin
	    let vse1 = get_var vars vse1 and vse2 = get_var vars vse2 in
	    let vaux = Fcl_nonlinear.monome_aux vse1 vse2 in
	    Hashtbl.add vars i vaux;
	    Fcl_nonlinear.monome vaux vse1 vse2 end
	| _ -> assert false in
      Fcl_cstr.post cstr)
    eqs_sorted;
  vars

(* remove vse = ct when vse = a simple nonlinear equation *)
let remove_basic_cstr1 eqs vse sre ct =
  match interm eqs sre with
    Un (Abse, Fde vsse) -> begin
      HE.remove eqs sre;
      let vars = post_eqs eqs in
      let vsse = get_var vars vsse in
      Fcl_nonlinear.absolute (Fcl_var.Fd.int ct) vsse end
  | Bin (typ, Fde vsse1, Fde vsse2) -> begin
      HE.remove eqs sre;
      let vars = post_eqs eqs in
      let vsse1 = get_var vars vsse1 and vsse2 = get_var vars vsse2 in
      (bintype2cstr typ) (Fcl_var.Fd.int ct) vsse1 vsse2 end
  | Agg (Multe, [(1, Fde vsse1); (1, Fde vsse2)], 1) -> begin
      HE.remove eqs sre;
      let vars = post_eqs eqs in
      let vsse1 = get_var vars vsse1 and vsse2 = get_var vars vsse2 in
      Fcl_nonlinear.monome (Fcl_var.Fd.int ct) vsse1 vsse2 end
  | Agg (Multe, [(coef, Fde vsse)], 1) -> begin
      HE.remove eqs sre;
      let vars = post_eqs eqs in
      let vsse = get_var vars vsse in
      Fcl_nonlinear.expn (Fcl_var.Fd.int ct) vsse coef end
  | _ ->
      let vars = post_eqs eqs in
      let vse = get_var vars vse in
      Fcl_linear.cstr [(1, vse)] Fcl_linear.Equal ct

(* remove vse1 = vse2 when vse1 or vse2 = a simple nonlinear equation *)
let remove_basic_cstr2 eqs vse1 vse2 sre1 sre2 =
  match (interm eqs sre1, interm eqs sre2) with
    (Un (Abse, Fde vsse1), _) -> begin
      HE.remove eqs sre1;
      let vars = post_eqs eqs in
      let vse2 = get_var vars vse2 and vsse1 = get_var vars vsse1 in
      Fcl_nonlinear.absolute vse2 vsse1 end
  | (_, Un (Abse, Fde vsse2)) -> begin
      HE.remove eqs sre2;
      let vars = post_eqs eqs in
      let vse1 = get_var vars vse1 and vsse2 = get_var vars vsse2 in
      Fcl_nonlinear.absolute vse1 vsse2 end
  | (Bin (typ, Fde vsse1, Fde vsse2), _) -> begin
      HE.remove eqs sre1;
      let vars = post_eqs eqs in
      let vsse1 = get_var vars vsse1 and vsse2 = get_var vars vsse2
      and vse2 = get_var vars vse2 in
      (bintype2cstr typ) vse2 vsse1 vsse2 end
  | (_, Bin (typ, Fde vsse1, Fde vsse2)) -> begin
      HE.remove eqs sre2;
      let vars = post_eqs eqs in
      let vsse1 = get_var vars vsse1 and vsse2 = get_var vars vsse2
      and vse1 = get_var vars vse1 in
      (bintype2cstr typ) vse1 vsse1 vsse2 end
  | (Agg (Multe, [(1, Fde vsse1); (1, Fde vsse2)], 1), _) -> begin
      HE.remove eqs sre1;
      let vars = post_eqs eqs in
      let vsse1 = get_var vars vsse1 and vsse2 = get_var vars vsse2
      and vse2 = get_var vars vse2 in
      Fcl_nonlinear.monome vse2 vsse1 vsse2 end
  | (_, Agg (Multe, [(1, Fde vsse1); (1, Fde vsse2)], 1)) -> begin
      HE.remove eqs sre2;
      let vars = post_eqs eqs in
      let vsse1 = get_var vars vsse1 and vsse2 = get_var vars vsse2
      and vse1 = get_var vars vse1 in
      Fcl_nonlinear.monome vse1 vsse1 vsse2 end
  | (Agg (Multe, [(coef, Fde vsse)], 1), _) -> begin
      HE.remove eqs sre1;
      let vars = post_eqs eqs in
      let vsse = get_var vars vsse and vse2 = get_var vars vse2 in
      Fcl_nonlinear.expn vse2 vsse coef end
  | (_, Agg (Multe, [(coef, Fde vsse)], 1)) -> begin
      HE.remove eqs sre2;
      let vars = post_eqs eqs in
      let vsse = get_var vars vsse and vse1 = get_var vars vse1 in
      Fcl_nonlinear.expn vse1 vsse coef end
  | _ ->
      let vars = post_eqs eqs in
      let vse1 = get_var vars vse1 and vse2 = get_var vars vse2 in
      Fcl_linear.cstr [(-1, vse1); (1, vse2)] Fcl_linear.Equal 0

let constrain e rel =
  (* e rel 0 *)
  let re = reduce e in
  (* vfinal = re, vfinal rel 0 *)
  let (vfinal, eqs) = equations re in
  (* termfinal rel 0 *)
  (* (Inte i) cannot appear in equations but may in [re] when eqs is empty *)
  (* get final equation: vfinal = termfinal *)
  let termfinal = interm eqs re in
  let cstr =
    match rel with
      Fcl_linear.Equal -> begin
      (* last equation is useless *)
	HE.remove eqs re;
	match termfinal with
	  Inte 0 -> begin assert (emptyh eqs); Fcl_cstr.one end (* 0 = 0 *)
	| Inte _i -> begin assert (emptyh eqs); Fcl_cstr.zero end (* i = 0 *)
	| Fde x ->
	    let vars = post_eqs eqs in
	    let v = get_var vars x in
	    Fcl_linear.cstr [(1, v)] Fcl_linear.Equal 0 (* v = 0 *)
	| Un (Abse, Fde vse) -> (* |v| = 0 *)
	    let vars = post_eqs eqs in
	    let vse = get_var vars vse in
	    Fcl_linear.cstr [(1, vse)] Fcl_linear.Equal 0
	| Bin (typ, Fde vse1, Fde vse2) ->
	    let vars = post_eqs eqs in
	    let vse1 = get_var vars vse1 and vse2 = get_var vars vse2 in
	    (bintype2cstr typ) (Fd.int 0) vse1 vse2
	| Agg (Multe, [(1, Fde vse1); (1, Fde vse2)], 1) ->
	    let vars = post_eqs eqs in
	    let vse1 = get_var vars vse1 and vse2 = get_var vars vse2 in
	    Fcl_nonlinear.monome (Fd.int 0) vse1 vse2
	| Agg (Multe, [(coef, Fde vse)], 1) -> begin
	    assert (coef > 1); (* only true exponentiation *)
	  (* vse ^ n = 0 <=> vse = 0 *)
	    let vars = post_eqs eqs in
	    let vse = get_var vars vse in
	    Fcl_linear.cstr [(1, vse)] Fcl_linear.Equal 0 end
	| Agg (Pluse, vses, ct) -> begin
        (* vse1 = vse2 or vse = ct
	   here we can remove some useless equations if one of
	   the two subexpressions is a simple non-linear expression *)
	    match vses with
	      [(-1, Fde vse1); (1, Fde vse2)] when ct = 0 -> begin
		match re with (* vse1 = sre1 and vse2 = sre2 *)
		  Agg (Pluse, [(-1, sre1); (1, sre2)], 0) ->
		    remove_basic_cstr2 eqs vse1 vse2 sre1 sre2
		| _ -> assert false end
	    | [(-1, Fde vse)] -> begin (* vse = ct *)
		match re with
		  Agg (Pluse, [(-1, sre)], ct) ->
		    remove_basic_cstr1 eqs vse sre ct
		| _ -> assert false end
	    | [(1, Fde vse)] -> begin (* vse = ct *)
		match re with
		  Agg (Pluse, [(1, sre)], ct) ->
		    remove_basic_cstr1 eqs vse sre (0 - ct)
		| _ -> assert false end
	    | _ -> (* no simplification *)
		let vars = post_eqs eqs in
		let vses = intfde2intfd vars vses in
		Fcl_linear.cstr vses Fcl_linear.Equal (0 - ct) end
	| _ -> assert false end

    | _ -> begin (* constraint is not =~ *)
	match termfinal with
	  Inte i -> begin
	    assert (emptyh eqs); (* no intermediate equations *)
	    match rel with
	      Fcl_linear.Diff -> if i = 0 then Fcl_cstr.zero else Fcl_cstr.one
	    | Fcl_linear.LessThan -> if i <= 0 then Fcl_cstr.one else Fcl_cstr.zero
	    | _ -> assert false end
	| Fde v -> begin
	    assert (emptyh eqs); (* no intermediate equations *)
	    let v = match v with Var v -> v | _ -> assert false in
	    let cstr = Fcl_linear.cstr [(1, v)] rel 0 in
	    cstr end
	| Agg (Pluse, vses, ct) -> begin
        (* final intermediate equation is removed because it can be
	   directly returned *)
	    HE.remove eqs re;
	    let vars = post_eqs eqs in
	    let vses = intfde2intfd vars vses in
	    Fcl_linear.cstr vses rel (0 - ct) end
	| _ ->
	(* all other cases, but we could check if [termfinal] is well formed
	   with an OR-pattern *)
	    let vars = post_eqs eqs in
	    let vfinal = get_var vars vfinal in
	    Fcl_linear.cstr [(1, vfinal)] rel 0 end in

  Fcl_debug.call 'a' (fun s -> Printf.fprintf s "final constraint: %a\n" Fcl_cstr.fprint cstr);
  cstr
