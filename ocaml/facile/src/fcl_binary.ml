(* $Id: fcl_binary.ml,v 1.13 2004-09-08 16:57:28 barnier Exp $ *)

open Fcl_var

let binary var1 var2 nogoods =
  let min = Fd.min var1
  and nogoods1 =
    Array.init (Fd.max var1 - Fd.min var1+1)
      (fun i ->
	(List.fold_right
	   (fun (x1, x2) acc -> if x1 = i+Fd.min var1 then x2::acc else acc) nogoods [])) in

  let name = "binary" in
  let delay x =
    Fd.delay [Fd.on_refine] var1 x;
    Fd.delay [Fd.on_refine] var2 x
  and update _ =
    match Fd.value var1 with
      Unk attr ->
	Fcl_domain.iter
	  (fun x ->
	    try
	      Fd.iter
		(fun y -> if not (List.mem y nogoods1.(x-min)) then raise Exit)
		var2;
	      match Fd.value var1 with
		Unk _var1_ -> Fd.remove var1 x
	      | Val xx -> assert(x = xx); Fcl_stak.fail name
	    with Exit -> ())
	  attr;
	false
    | Val x ->
	Fd.iter
	  (fun y ->
	    if List.mem y nogoods1.(x-min) then
	      match Fd.value var2 with
		Val yy -> assert(y = yy); Fcl_stak.fail name
	      | Unk _ -> Fd.remove var2 y)
	  var2;
	true in
  Fcl_cstr.create ~name update delay

let cstr var1 var2 ng =
  let c1 = binary var1 var2 ng
  and c2 = binary var2 var1 (List.map (fun (x,y)-> (y,x)) ng) in
  Fcl_reify.(&&~~) c1 c2
