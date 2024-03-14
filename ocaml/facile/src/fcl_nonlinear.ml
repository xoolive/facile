(* $Id: fcl_nonlinear.ml,v 1.8 2004-08-12 15:22:07 barnier Exp $ *)

open Fcl_var
open Fcl_misc.Operators
open Printf
module C = Fcl_cstr
module Linear = Fcl_linear


(*** Bounds evaluation ***)

(* signs differ with x <= y *)
let diffsign x y = assert (x <= y); x < 0 && y > 0
let diffeqsign x y = assert (x <= y); x <= 0 && y >= 0
(* unsorted diffsign *)
let udiffsign x y = if x <= y then diffsign x y else diffsign y x

(* min_of_absmod for min_max_of_expr when bounds are already known
   AND positive *)
let min_of_absmod_inter a b c d =
  assert (a >= 0 && c >= 0);
  if b < c then a
  else (* 0 *)
  (* on peut faire mieux quand c=d (i.e. x2 connu) *)
    if c = d then
      if c = 0 then
	Fcl_debug.fatal_error "Arith.min_of_absmod_inter: division_by_zero"
      else
	let amodc = a mod c in
	if b - a + amodc < c then amodc else 0
    else 0

(* max_of_absmod for min_max_of_expr when bounds are already known
   AND positive *)
let max_of_absmod_inter a b c d =
  assert (a >= 0 && c >= 0);
  if b < d then b
  else (* d - 1 *)
  (* we could do more when c=d (i.e. x2 known) *)
    if c = d then
      if c = 0 then
	Fcl_debug.fatal_error "Arith.max_of_absmod_inter: division_by_zero"
      else
	let bmodc = b mod c in
	if bmodc - (b - a) > 0 then bmodc else d - 1
    else d - 1


(* Implementation of non-linear constraints (z = x*y, x^n, x/y, |x|, x%y) *)

(*** monome constraint: z = x * y ***)

let min_max_mult_inter a b c d =
  if diffeqsign a b || diffeqsign c d then
    (* one of the two domains contains 0 *)
    (min (a*d) (b*c), max (a*c) (b*d))
  else if udiffsign a c then (* domains not on the same side of 0 *)
    if a > 0 then (b*c, a*d) else (a*d, b*c)
  else (* same side *)
    if a > 0 then (a*c, b*d) else (b*d, a*c)

(* y = z[a,b] / x[c,d] *)
(* compute bounds to refine y *)
let min_max_of_div_for_mult z x =
  (* if 0 belongs to dom(x), no conclusion on y *)
  if Fd.member x 0 then (min_int, max_int) else
  let (a, b) = Fd.min_max z and (c, d) = Fd.min_max x in
  (* is this useful? only waken on min or max modif... *)
  if diffsign c d then (* c < 0 < d*)
    match Fd.value x with
      Unk xa ->
	let (c', d') = Fcl_domain.largest_hole_around xa 0 in
	(min (a /+ d') (b /+ c'), max (a /- c') (b /- d'))
      (* if x was ground, c*d >= 0 *)
    | _ -> Fcl_debug.internal_error "min_max_of_div_for_mult: x ground"
  (* 0 < c || d < 0 *)
  else if diffsign a b then (* a < 0 < b *)
    if c > 0 then (a /+ c, b /- c) else (b /+ d, a /- d)
  (* 0 does not belong to dom(z) or dom(x) *)
  else
    if a >= 0 then (* z >= 0 *)
      if c > 0 then (a /+ d, b /- c) else (b /+ d, a /- c) (* x pos or neg *)
    else (* z <= 0 *)
      if c < 0 then (b /+ c, a /- d) else (a /+ c, b /- d) (* x neg or pos *)

(* z = x*y *)
let monome z x y =
  let name = "monome" in
  let zero_removed = Fcl_stak.ref false in
  let update_val_unk a y =
    match Fd.value z with
      Val c -> (* a <> 0 *)
	if c mod a = 0 then (Fd.unify y (c / a); true)
	else Fcl_stak.fail (name ^ ": Val a, Unk _, Val c")
    | Unk _ ->
	C.post (Linear.cstr [(1, z); (-a, y)] Linear.Equal 0); true in
  let compute_bounds () = (Fd.min_max z, Fd.min_max x, Fd.min_max y) in

  let delay c =
    Fd.delay [Fd.on_min; Fd.on_max] z c;
    Fd.delay [Fd.on_min; Fd.on_max] x c;
    Fd.delay [Fd.on_min; Fd.on_max] y c

  and fprint c =
    Printf.fprintf c "%a = %a * %a" Fd.fprint z Fd.fprint x Fd.fprint y;
    flush c

  and update _ =
    Fcl_debug.call 'a' (fun s -> fprintf s "%s - before update : %a = %a * %a\n" name Fd.fprint z Fd.fprint x Fd.fprint y);

    let rec loop () =
      match Fd.value x, Fd.value y with
	Val a, Val b -> Fd.unify z (a * b); true
      | (Val 0, _  | _, Val 0) ->  Fd.unify z 0; true
      | Val a, Unk _ -> update_val_unk a y
      | Unk _, Val b -> update_val_unk b x
(* On pourrait aussi traiter le cas z = Val 1 (idem pour expn) *)
      | Unk xa, Unk ya ->
	  match Fd.value z with
	    Val 0 ->
	      let xa_with_0 = Fcl_domain.member xa 0
	      and ya_with_0 = Fcl_domain.member ya 0 in
	      if xa_with_0 then begin
		if not ya_with_0 then begin
		  Fd.unify x 0; true end
		else false end
	      else if ya_with_0 then begin
		Fd.subst y 0; true end
	      else Fcl_stak.fail (name ^ ": Unk xa, Unk ya, Val c")
	  | _z_val ->
	      (* On essaie d'enlever d'abord 0 dans x et y sinon pas de
		 propagation *)
	      if not (Fcl_stak.get zero_removed) && not (Fd.member z 0) then begin
		Fcl_stak.set zero_removed true;
		Fd.refine x (Fcl_domain.remove 0 xa);
		Fd.refine y (Fcl_domain.remove 0 ya);
	        (* x et y peuvent etre instanciées *)
		loop () end
	      else begin
		let bounds = compute_bounds () in
		let (a, b) = Fd.min_max x and (c, d) = Fd.min_max y in
		let (z_min, z_max) = min_max_mult_inter a b c d in
		Fcl_debug.call 'a' (fun s -> fprintf s "%s - Unk xa, Unk ya, z : z_min=%d z_max=%d " name z_min z_max);
		Fd.refine_low_up z z_min z_max;

		let (y_min, y_max) = min_max_of_div_for_mult z x in
		Fcl_debug.call 'a' (fun s -> fprintf s "y_min=%d y_max=%d " y_min y_max);
		Fd.refine_low_up y y_min y_max;

		let (x_min, x_max) = min_max_of_div_for_mult z y in
		Fcl_debug.call 'a' (fun s -> fprintf s "x_min=%d x_max=%d\n" x_min x_max);
		Fd.refine_low_up x x_min x_max;
	        (* On rappelle update pour atteindre le point fixe *)
		if bounds <> compute_bounds () then loop () else false end in
    let r = loop () in
    Fcl_debug.call 'a' (fun s -> fprintf s "%s - after update : %a = %a * %a\n" name Fd.fprint z Fd.fprint x Fd.fprint y);
    r in

  C.create ~name ~fprint update delay


let min_max_abs_for_abs x =
  match Fd.value x with
    Val a -> let absa = abs a in (absa, absa)
  | Unk xa ->
      let (a, b) = Fcl_domain.min_max xa in
      if a >= 0 then (a, b) else if b <= 0 then (-b, -a) else
      (* x à cheval sur 0 *)
      let a', b' = Fcl_domain.largest_hole_around xa 0 in
      (min (-a') b', max (-a) b)

let absolute z x =
  let name = "absolute" in

  let delay c = (* We could delay on_refine for x *)
    Fd.delay [Fd.on_min; Fd.on_max] z c;
    Fd.delay [Fd.on_min; Fd.on_max] x c

  and fprint c =
    Printf.fprintf c "%a = |%a|" Fd.fprint z Fd.fprint x; flush c

  and update _ =
    Fcl_debug.call 'a' (fun s -> fprintf s "%s - before update : %a=|%a|\n" name Fd.fprint z Fd.fprint x);

    let r = match Fd.value x with
      Val a -> Fd.unify z (abs a); true
    | Unk xa ->
	if Fcl_domain.min xa >= 0 then begin
	  C.post (Linear.cstr [(1, z); (-1, x)] Linear.Equal 0);
 	  true end
	else if Fcl_domain.max xa <= 0 then begin
	  C.post (Linear.cstr [(1, z); (1, x)] Linear.Equal 0);
 	  true end
	else
	  match Fd.value z with
	    Val c -> begin
              Fd.refine
                x (Fcl_domain.intersection (Fcl_domain.create [-c; c]) xa);
	      true end
	  | Unk _za -> begin
	      let (z_min, z_max) = min_max_abs_for_abs x in
	      Fd.refine_low_up z z_min z_max;
	      let z_min, z_max = Fd.min_max z in
	      assert (z_min >= 0);
	      let d =
		Fcl_domain.remove_closed_inter (-z_min+1) (z_min-1)
		  (Fcl_domain.remove_low_up (-z_max) z_max xa) in
	      Fd.refine x d;
	      false end in

    Fcl_debug.call 'a' (fun s -> fprintf s "%s - after update : %a=|%a|\n" name Fd.fprint z Fd.fprint x);
    r in

  C.create ~name ~fprint update delay


(* z = x1 / x2 *)
let min_max_div_inter a b c d =
  if c = d && c = 0 then
    Fcl_stak.fail "Arith.min_max_div_inter: division_by_zero"
  else (* Otherwise, we suppose that x2 won't be instantiated to 0 *)
    let c = if c = 0 then 1 else c and d = if d = 0 then -1 else d in
    if diffsign c d then
      (min a (0 - b), max (0 - a) b)
    else if diffsign a b then
      if c > 0 then (a/c, b/c) else (b/d, a/d)
    else
      if a >= 0 then (* x1 positive *)
	if c > 0 then (a/d, b/c) else (b/d, a/c) (* x2 positive ou negative *)
      else (* x1 negative *)
	if c < 0 then (b/c, a/d) else (a/c, b/d) (* x2 negative ou positive *)

(* y = xr[a,b] / z[c,d] *)
(* a = min (x-r) , b = max (x-r) *)
let min_max_of_div_for_div a b z =
  (* if 0 belongs to dom(x), no conclusion on y *)
  if Fd.member z 0 then (min_int, max_int)
  else
    let (c, d) = Fd.min_max z in
    (* is this useful? only waken on min or max modif... *)
    if sign c * sign d < 0 then (* c < 0 < d *)
      match Fd.value z with
	Unk domz ->
	  let (c', d') = Fcl_domain.largest_hole_around domz 0 in
	  (min (a /+ d') (b /+ c'), max (a /- c') (b /- d'))
        (* if z was ground, c*d >= 0 *)
      |	_ -> Fcl_debug.internal_error "min_max_of_div_for_mult : z ground"
    (* 0 < c || d < 0 *)
    else if sign a * sign b < 0 then (* a < 0 < b *)
      if c > 0 then (a /+ c, b /- c) else (b /+ d, a /- d)
    else
      if a >= 0 then (* xr positive *)
	if c > 0 then (a /+ d, b /- c) else (b /+ d, a /- c) (* z positive or negative *)
      else (* xr negative *)
	if c < 0 then (b /+ c, a /- d) else (a /+ c, b /- d) (* z negative or positive *)

(* z=x/y, x=y*z+r *)
(* if x >= 0 then r >= 0 else r <= 0 *)
let min_max_of_remainder x y =
  let r_abs_max =
    let min_y, max_y = Fd.min_max y in
    max (Stdlib.abs min_y) (Stdlib.abs max_y) - 1 in
  if Fd.min x >= 0 then (0, r_abs_max)
  else if Fd.max x <= 0 then ((0 - r_abs_max), 0)
  else ((0 - r_abs_max), r_abs_max)

(* z = x / y *)
let division z x y =
  let zero_removed = Fcl_stak.ref false in
  let min_max_r () = min_max_of_remainder x y in
  let compute_bounds () =
    (Fd.min_max z, Fd.min_max x, Fd.min_max y) in

  let name = "division" in

  let delay c =
    Fd.delay [Fd.on_min; Fd.on_max] z c;
    Fd.delay [Fd.on_min; Fd.on_max] x c;
    Fd.delay [Fd.on_min; Fd.on_max] y c

  and fprint c =
    Printf.fprintf c "%a = %a / %a" Fd.fprint z Fd.fprint x Fd.fprint y;
    flush c

  and update _ =
    Fcl_debug.call 'a' (fun s -> fprintf s "%s - before update : %a = %a / %a\n" name Fd.fprint z Fd.fprint x Fd.fprint y);

    if not (Fcl_stak.get zero_removed) then begin
      begin match Fd.value y with
	Unk _ya -> Fd.remove y 0
      | Val 0 -> Fcl_stak.fail (name ^ ": division by zero")
      | _ -> () end;
      Fcl_stak.set zero_removed true end;

    (* 0 does not belong to dom(y) *)
    let rec loop bounds =
      match Fd.value x, Fd.value y with
	Val a, Val b -> Fd.unify z (a / b); true
      | Val 0, _ -> Fd.unify z 0; true
      | _x_val, _y_val ->
	  let (a, b) = Fd.min_max x and (c, d) = Fd.min_max y in
	  let (z_min, z_max) = min_max_div_inter a b c d in
	  Fcl_debug.call 'a' (fun s -> fprintf s "z_min=%d z_max=%d " z_min z_max);
	  Fd.refine_low_up z z_min z_max;

	  (* x = y*z + r *)
	  let (a, b) = Fd.min_max y and (c, d) = Fd.min_max z in
	  let (yz_min, yz_max) = min_max_mult_inter a b c d
	  and (r_min, r_max) = min_max_r () in
	  let (x_min, x_max) =
	    Linear.min_max_plus_inter yz_min yz_max r_min r_max in
	  Fcl_debug.call 'a' (fun s -> fprintf s "x_min=%d x_max=%d " x_min x_max);
	  Fd.refine_low_up x x_min x_max;

	  (* y = (x-r) / z *)
	  let (r_min, r_max) = min_max_r () in
	  let xr_min = Fd.min x - r_max and xr_max = Fd.max x - r_min in
	  let (y_min, y_max) = min_max_of_div_for_div xr_min xr_max z in
	  Fcl_debug.call 'a' (fun s -> fprintf s "y_min=%d y_max=%d\n" y_min y_max);
	  Fd.refine_low_up y y_min y_max;

	  let new_bounds = compute_bounds () in
	  if bounds <> new_bounds then loop new_bounds else false in
    let r = loop (compute_bounds ()) in

    Fcl_debug.call 'a' (fun s -> fprintf s "%s - after update : %a = %a / %a\n" name Fd.fprint z Fd.fprint x Fd.fprint y);
    r in

  C.create ~name ~fprint update delay



let min_max_abs_inter a b =
  if a >= 0 then (a, b)
  else if b <= 0 then (0 - b, 0 - a)
  else (0, max (0 - a) b)

let min_max_mod_inter a b c d =
  let (c, d) = min_max_abs_inter c d in
  if a >= 0 then (* x1 >= 0 *)
    (min_of_absmod_inter a b c d, max_of_absmod_inter a b c d)
  else if b <= 0 then (* x1 <= 0 *)
    let (a, b) = min_max_abs_inter a b in
    (0 - max_of_absmod_inter a b c d, 0 - min_of_absmod_inter a b c d)
  else (0 - max_of_absmod_inter 0 (-a) c d, max_of_absmod_inter 0 b c d)

(* y = (x-z) / (x/y) *)
let min_max_of_div_for_mod a b c d =
  (* if 0 belongs to domain of x/y, no conclusion on y *)
  if c <= 0 && d >= 0 then
    (min_int, max_int)
  else (* x/y positive ou negative *)
    if sign a * sign b < 0 then (* xz à cheval sur 0 *)
      if c > 0 then (a /+ c, b /- c) else (b /+ d, a /- d)
    (* ni xz ni x/y à cheval sur 0 *)
    else if a >= 0 then (* xz positive *)
      if c > 0 then (a /+ d, b /- c) else (b /+ d, a /- c) (* x/y positive ou negative *)
    else (* xz negative *)
      if c < 0 then (b /+ c, a /- d) else (a /+ c, b /- d) (* x/y negative ou positive *)

(* z = x % y *)
let modulo z x y =
  let zero_removed = Fcl_stak.ref false in
  let min_max_of_xexp () =
    let (xa, xb) = Fd.min_max x and (yc, yd) = Fd.min_max y in
    let (xyc, xyd) = min_max_div_inter xa xb yc yd in
    let (yxya, yxyb) = min_max_mult_inter yc yd xyc xyd in
    let (zc, zd) = Fd.min_max z in
    let xmin, xmax = Linear.min_max_plus_inter yxya yxyb zc zd in
    let xmin = if zc >= 0 then max 0 xmin else xmin
    and xmax = if zd <= 0 then min 0 xmax else xmax in
    (xmin, xmax) in
  let min_max_of_yexp () =
    let (xa, xb) = Fd.min_max x and (zc, zd) = Fd.min_max z in
    let (xza, xzb) = Linear.min_max_minus_inter xa xb zc zd in
    let (yc, yd) = Fd.min_max y in
    let (xyc, xyd) = min_max_div_inter xa xb yc yd in
    min_max_of_div_for_mod xza xzb xyc xyd in

  (* when y is known and b-a < y, c actually is |c| *)
  let hole a b c =
    assert (c >= 0);
    if a >= 0 then (* x >= 0 *)
      let amodc = a mod c and bmodc = b mod c in
      if amodc <= bmodc then
	match Fd.value z with
	  Val v -> (* a/c = b/c *)
	    let xv = c * (a / c) + v in
	    Fd.subst x xv; raise Exit
	| _ -> Fd.refine_low_up z amodc bmodc
      else
	match Fd.value z with
	  Unk attrz ->
	    let newdom = Fcl_domain.union
		(Fcl_domain.interval 0 bmodc)
                (Fcl_domain.interval amodc (c-1)) in
	    Fd.refine z (Fcl_domain.intersection newdom attrz)
	| Val v ->
	    let ab = (* a/c = b/c - 1 *)
	      if v >= amodc && v < c then a
	      else if v <= bmodc && v >= 0 then b
	      else Fcl_stak.fail "Nonlinear.modulo" in
	    let xv = c * (ab / c) + v in
	    Fd.subst x xv; raise Exit
    else if b <= 0 then (* x <= 0 *)
      let amodc = a mod c and bmodc = b mod c in
      if amodc <= bmodc then
	match Fd.value z with
	  Val v -> (* a/c = b/c *)
	    let xv = c * (a / c) + v in
	    Fd.subst x xv; raise Exit
	| _ -> Fd.refine_low_up z amodc bmodc
      else
	match Fd.value z with
	  Unk attrz ->
	    let newdom = Fcl_domain.union (* c >= 0 *)
		(Fcl_domain.interval amodc 0)
                (Fcl_domain.interval (1-c) bmodc) in
	    Fd.refine z (Fcl_domain.intersection newdom attrz)
	| Val v ->
	    let ab = (* b/c = a/c - 1 *)
	      if v <= bmodc && v > -c then b
	      else if v >= amodc && v <= 0 then a
	      else Fcl_stak.fail "Nonlinear.modulo" in
	    let xv = c * (ab / c) + v in
	    Fd.subst x xv; raise Exit
    (* 0 strictly belongs to [a,b] so -|c| < a < 0 < b < |c| *)
    else Fd.refine_low_up z a b in

  let compute_bounds () = (Fd.min_max z, Fd.min_max x, Fd.min_max y) in

  let name = "modulo" in

  let delay c =
    Fd.delay [Fd.on_min; Fd.on_max] z c;
    Fd.delay [Fd.on_min; Fd.on_max] x c;
    Fd.delay [Fd.on_min; Fd.on_max] y c

  and fprint c =
    Printf.fprintf c "%a = %a %% %a" Fd.fprint z Fd.fprint x Fd.fprint y;
    flush c

  and update _ =
    Fcl_debug.call 'a' (fun s -> fprintf s "%s - before update : %a = %a %% %a\n" name Fd.fprint z Fd.fprint x Fd.fprint y);

    if not (Fcl_stak.get zero_removed) then begin
      begin match Fd.value y with
	Unk _ya -> Fd.remove y 0
      | Val 0 -> Fcl_stak.fail (name ^ ": division by zero")
      | _ -> () end;
      Fcl_stak.set zero_removed true end;
    (* 0 does not belong to dom(y) *)
    let rec loop bounds =
      match Fd.value x, Fd.value y with
	Val a, Val b -> Fd.unify z (a mod b); true
      | Val 0, _ -> Fd.unify z 0; true
      | _x_val, _y_val ->
	  let (a, b) = Fd.min_max x and (c, d) = Fd.min_max y in
	  if c = d && b - a < abs c then (* y known *)
	    hole a b (abs c)
	  else begin
	    let (z_min, z_max) = min_max_mod_inter a b c d in
	    Fcl_debug.call 'a' (fun s -> fprintf s "z_min=%d z_max=%d " z_min z_max);
	    Fd.refine_low_up z z_min z_max end;

	  (* x = y*(x/y) + z *)
	  let (x_min, x_max) = min_max_of_xexp () in
	  Fcl_debug.call 'a' (fun s -> fprintf s "x_min=%d x_max=%d " x_min x_max);
	  Fd.refine_low_up x x_min x_max;

	  (* y = (x-z) / (x/y) *)
	  let (y_min, y_max) = min_max_of_yexp () in
	  Fcl_debug.call 'a' (fun s -> fprintf s "y_min=%d y_max=%d\n" y_min y_max);
	  Fd.refine_low_up y y_min y_max;

	  let new_bounds = compute_bounds () in
	  if bounds <> new_bounds then loop new_bounds else false in
    (* function hole raises Exit when the constraint is satisfied *)
    let r = try loop (compute_bounds ()) with Exit -> true in

    Fcl_debug.call 'a' (fun s -> fprintf s "%s - after update : %a = %a %% %a\n" name Fd.fprint z Fd.fprint x Fd.fprint y);
    r in

  C.create ~name ~fprint update delay



let expn_int x n =
  if n < 0 then Fcl_debug.fatal_error "Arith.expn_int: negative exponent" else
  let rec loop = function
      0 -> 1
    | n ->
	let n2 = n / 2 in
	let xn2 = loop n2 in
	let xn = xn2 * xn2 in
	if n mod 2 = 0 then xn else xn * x in
  loop n

let min_max_expn_inter a b n =
  assert (a <= b);
  if n < 0 then
    Fcl_debug.fatal_error "Nonlinear.min_max_expn_inter: negative exponent"
  else if n = 0 then (1, 1) else
  let an = expn_int a n and bn = expn_int b n in
  if n mod 2 = 0 then
    if a >= 0 then (an, bn)
    else if b <= 0 then (bn, an)
    else if an <= bn then (0, bn) else (0, an)
  else (an , bn)

let min_max_of_expn x n =
  assert (n > 1);
  match Fd.value x with
    Unk xa ->
      let (min_xa, max_xa) = Fcl_domain.min_max xa in
      let min_xan = expn_int min_xa n and max_xan = expn_int max_xa n in
      if n mod 2 = 0 then  (* even exponent *)
	if min_xa >= 0 then (min_xan, max_xan) (* positive domain *)
	else if max_xa <= 0 then (max_xan, min_xan) (* negative domain *)
	else
	  let min_neg, min_pos = Fcl_domain.largest_hole_around xa 0 in
	  let minn =
	    if -min_neg < min_pos then expn_int min_neg n
	    else expn_int min_pos n
	  and maxn = max min_xan max_xan in
	  (minn, maxn)
  (* L'exposant est impair *)
      else (min_xan, max_xan)
  | Val c -> let cn = expn_int c n in (cn, cn)

let nth_root upper z n =
  (* Root of a negative number returns nan, so computation is done
     with the absolute value *)
  let az = abs z and sz = sign z in
  let znth = truncate (float az ** (1. /. float n)) in
  if expn_int znth n = az then sz * znth else
  (* float computation may return z^(1/n)-e, rounded by truncate to the
     preceding integer of the actual root *)
  let znmore = znth + 1 in
  if expn_int znmore n = az then sz * znmore else
  if upper then
    if z >= 0 then znmore else (0 - znth)
  else
    if z >= 0 then znth else (0 - znmore)

let ( **/+) = nth_root true
let ( **/-) = nth_root false

let min_max_of_nth_root z n =
  (Fd.min z **/+ n, Fd.max z **/- n)

let int_root z n =
  let az = abs z and sz = sign z in
  let znth = truncate (float az ** (1. /. float n)) in
  if expn_int znth n = az then sz * znth else
  let znmore = znth + 1 in
  if expn_int znmore n = az then sz * znmore else raise Not_found

(* z = x^n *)
let expn z x n =
  let even = n mod 2 = 0
  and compute_bounds () = (Fd.min_max z, Fd.min_max x) in
  let name = "expn" in

  let delay c =
    Fd.delay [Fd.on_min; Fd.on_max] z c;
    Fd.delay [Fd.on_min; Fd.on_max] x c

  and fprint c =
    Printf.fprintf c "%a = %a ^ %d" Fd.fprint z Fd.fprint x n;
    flush c

  and update _ =
    Fcl_debug.call 'a' (fun s -> fprintf s "%s - before update : %a = %a ^ %d\n" name Fd.fprint z Fd.fprint x n);

    let rec loop bounds =
      match Fd.value x, Fd.value z with
	Val a, _ -> Fd.unify z (expn_int a n); true
      | Unk xa, Val c -> begin
	  try
	    let root = int_root c n in
	    if even then
              Fd.refine
                x (Fcl_domain.intersection (Fcl_domain.create [-root; root]) xa)
	    else Fd.subst x root;
	    true
	  with Not_found -> Fcl_stak.fail name end
      | Unk xa, Unk _za ->
	  let (z_min, z_max) = min_max_of_expn x n in
	  Fcl_debug.call 'a' (fun s -> fprintf s "%s - Unk xa, Unk za : z_min=%d z_max=%d " name z_min z_max);
	  Fd.refine_low_up z z_min z_max;
	  let (x_min, x_max) = min_max_of_nth_root z n in
	  Fcl_debug.call 'a' (fun s -> if even then fprintf s "%s (even) - Unk xa, za : x_neg=[%d,%d] x_pos=[%d,%d]\n" name (-x_max) (-x_min) x_min x_max else fprintf s "%s (odd) - Unk xa, Unk za : x_min=%d x_max=%d\n" name x_min x_max);
	  if even then
	    (* [-x_max_pos, -x_min_pos] U [x_min_pos, x_max_pos] *)
	    let d =
	      Fcl_domain.remove_closed_inter (-x_min+1) (x_min-1)
		(Fcl_domain.remove_low_up (-x_max) x_max xa) in
	    Fd.refine x d
	  else
	    (* [x_min, x_max] *)
	    Fd.refine_low_up x x_min x_max;
	  let new_bounds = compute_bounds () in
	  if bounds <> new_bounds then loop new_bounds else false in
    let r = loop (compute_bounds ()) in

    Fcl_debug.call 'a' (fun s -> fprintf s "expn - after update : %a = %a ^ %d\n" Fd.fprint z Fd.fprint x n);

    r in

C.create ~name ~fprint update delay

let aux2 bounds x y =
  let (a, b) = Fd.min_max x and (c, d) = Fd.min_max y in
  let (z_min, z_max) = bounds a b c d in
  Fd.interval z_min z_max

let monome_aux = aux2 min_max_mult_inter
let division_aux = aux2 min_max_div_inter
let modulo_aux = aux2 min_max_mod_inter

let absolute_aux x =
  let (z_min, z_max) = min_max_abs_for_abs x in
  Fd.interval z_min z_max

let expn_aux x n =
  assert (n > 0);
  if n = 1 then x else
  let (mini, maxi) = min_max_of_expn x n in
  Fd.interval mini maxi
