(* $Id: fcl_goals.ml,v 1.65 2010-07-22 13:01:57 barnier Exp $ *)

open Fcl_var
module C = Fcl_cstr

type t = Fcl_stak.gl = { name : string; call : unit -> t option }

exception Success

let goal_stack = ref [];;

let reset () = Fcl_stak.reset (); C.reset_queue ();;

let while_true = fun _ -> ()

let name c = c.name

let fprint c g = Printf.fprintf c "%s" g.name

let create_rec ?(name = "create_rec") f =
  let rec g = { name = name; call = fun () -> Some (f g) } in
  g;;

let create ?(name = "create") f a =
  { name = name; call = fun () -> Some (f a) }

let atomic ?(name = "atomic") f =
  { name = name; call = fun () -> f (); None }

let success = { name = "success"; call = fun () -> None }

let fail = { name = "fail"; call = fun () -> Fcl_stak.fail "Goals.fail"}

let (&&~) g1 g2 =
  { name = "&&~"; call = (fun () -> goal_stack := g2:: !goal_stack; Some g1) }

let (||~) g1 g2 =
  { name = "||~";
    call =
      (fun () -> ignore (Fcl_stak.save (g2 :: !goal_stack)); Some g1) }

let on_choice_point = C.new_event ()

let and_loop () =
  while true do
    match !goal_stack with
      [] -> raise Success
    | goal::goals ->
        goal_stack := goals;
        begin
	  match goal.call () with
	    None -> ()
	  | Some g -> goal_stack := g :: !goal_stack
	end
  done

let solve ?(control = while_true) goal =
  Fcl_debug.call 'g' (fun f ->  Printf.fprintf f "|on_choice_point|=%d\n" (List.length (C.registered on_choice_point)));
  Fcl_stak.reset ();
  let backtracks = ref 0 in
      (* A choice point is systematically added before a search in order
	 to get a correct backtrack on events (constraints, refinnments, ...)
	 done before the first user choice point.
	 This case occurs in minimize where a constraint attached to
	 "choice_point" is posted before the search *)
  goal_stack := [goal ||~ fail];
  try
    while true do (* OR loop *)
      try
        control !backtracks;
	C.schedule on_choice_point;
	C.wake_all ();
	C.assert_empty_queue ();
	and_loop ()
      with
	Fcl_stak.Fail s ->
          Fcl_debug.call 'g' (fun f ->  Printf.fprintf f "fail %s\n" s);
          goal_stack := Fcl_stak.backtrack (); (* May raise Empty_stack *)
          incr backtracks
    done;
    failwith "end of while true"
  with
    Fcl_stak.Empty_stack -> false
  | Success -> control !backtracks; true


let remove var x =
  match Fd.value var with
    Val v ->
      if v = x then Fcl_stak.fail (Printf.sprintf "Fcl_goals.remove %d" x)
  | Unk _ -> Fd.remove var x

let indomain var =
  create_rec ~name:"indomain"
    (fun self ->
      match Fd.value var with
	Val _ -> success
      | Unk var_ ->
    	  let mini = Fcl_domain.min var_ in
	  Fcl_debug.call 'g' (fun f -> Printf.fprintf f "indomain: %a=%d\n" Fd.fprint var mini);
	  (atomic (fun () -> Fd.subst var mini)
	 ||~ atomic (fun () -> remove var mini))
	    &&~ self)
 (* can t use remove_min because min may have been changed in the
    choice point (Opti.minimize Continue does that) *)

let random var =
  create_rec ~name:"random"
    (fun self ->
      match Fd.value var with
	Val _ -> success
      | Unk var_ ->
    	  let x = Fcl_domain.random var_ in
	  Fcl_debug.call 'g' (fun f -> Printf.fprintf f "Goals.random: %a=%d\n" Fd.fprint var x);
	  (atomic (fun () -> Fd.subst var x)
	 ||~ atomic (fun () -> remove var x))
	    &&~ self)
 (* can t use remove_min because min may have been changed in the
    choice point (Opti.minimize Continue does that) *)

let assign ?(choose = Fcl_domain.min) var =
  let name = "assign" in
  create ~name
    (fun v ->
      match Fd.value v with
	Val _ -> success
      | Unk var_ ->
	  let x = choose var_ in
	  Fcl_debug.call 'g' (fun f -> Printf.fprintf f "%s: %a=%d\n" name Fd.fprint v x);
	  (atomic (fun () -> Fd.subst v x)
	 ||~ atomic (fun () -> remove var x)))
    var

type order = Decr | Incr

let dichotomic ?(order = Incr) var =
  create_rec ~name:"dichotomic"
    (fun self ->
      match Fd.value var with
      	Val _ -> success
      | Unk d ->
      	  let mini = Fcl_domain.min d and maxi = Fcl_domain.max d in
      	  let middle = (maxi + mini) / 2 in
	  let lower = atomic (fun () -> Fd.refine_up var middle)
	  and upper = atomic (fun () -> Fd.refine_low var (middle+1)) in
	  match order with
	    Decr -> (upper ||~ lower) &&~ self
	  | Incr -> (lower ||~ upper) &&~ self)


let instantiate choose var =
  create_rec ~name:"instantiate"
    (fun self ->
      match Fd.value var with
	Val _ -> success
      | Unk var_ ->
    	  let x = choose var_ in
	  (atomic (fun () -> Fd.subst var x)
	 ||~ atomic (fun () -> remove var x))
	    &&~ self)

let _once goal =
  let l = ref (Fcl_stak.level ()) in
  atomic (fun () -> l := (Fcl_stak.level ())) &&~
  goal &&~
  atomic (fun () -> Fcl_stak.cut !l)

let forto min max g =
  let rec la i =
    create
      (fun j ->
	 if j <= max then g j &&~ la (j+1) else success)
      i in
  la min

let foreachto min max g =
  if min > max then success else
  let rec la i =
    create
      (fun j ->
	 if j < max then g j ||~ la (j+1) else g max)
      i in
  la min

let fordownto max min g =
  forto min max (fun i -> g (max - i + min));;

module Array = struct
  let fold_hi select lab_one a init =
    create_rec
      (fun self ->
	match try let i = select a in Some i with Not_found -> None with
	  Some i -> lab_one i a.(i) self
	| None -> init)

  let foldi lab_one a init =
    let size = Array.length a in
    let rec la i =
      create
	(fun j -> if j < size then lab_one j a.(j) (la (j+1)) else init)
	i in
    la 0

  let foralli ?select f a =
    match select with
      None -> foldi (fun i x r -> f i x &&~ r) a success
    | Some s -> fold_hi s (fun i x r -> f i x &&~ r) a success

  let forall ?select f a = foralli ?select (fun _ -> f) a

  let existsi ?select f a =
    match select with
      None -> foreachto 0 (Array.length a - 1) (fun i -> f i a.(i))
    | Some s -> fold_hi s (fun i x r -> f i x ||~ r) a fail

  let exists ?select f a = existsi ?select (fun _ -> f) a

  let choose_index order tab =
    let n = Array.length tab in
    (* seek the first unbound variable *)
    let rec first_unbound i =
      if i < n then
        let tabi = tab.(i) in
        if Fd.is_var tabi then (i, tabi) else first_unbound (i + 1)
      else raise Not_found in

    let (b, attr) = first_unbound 0 in
    let best = ref b and attr_best = ref attr in
    (* seek the best variable wrt criterion *)
    for i = b+1 to n - 1 do
      let tabi = tab.(i) in
      if Fd.is_var tabi && order tabi !attr_best then
        begin best := i; attr_best := tabi end
    done;
    !best

  let labeling (a : Fcl_var.Fd.t array) = forall indomain a

  exception Return of int
  let not_instantiated_fd fds =
    try
      Array.iteri
      (fun i fdsi -> if Fd.is_var fdsi then raise (Return i))
	fds;
      raise Not_found
    with
      Return i -> i
end

module List = struct
  let rec fold fgoal l init =
    create
      (function
	   [] -> init
	 | x::xs -> fgoal x (fold fgoal xs init))
      l

  let rec fold_h select fgoal l init =
    create
      (function
	  [] -> init
       	| _ ->
            let x,xs = select l in
            fgoal x (fold_h select fgoal xs init))
      l

  let forall ?select f l =
    match select with
      None -> fold (fun x r -> f x &&~ r) l success
    | Some s -> fold_h s (fun x r -> f x &&~ r) l success

  let exists ?select f l =
    match select with
      None -> fold (fun x r -> f x ||~ r) l fail
    | Some s -> fold_h s (fun x r -> f x ||~ r) l fail

  let member v l = exists (fun x -> atomic (fun () -> Fd.unify v x)) l

  let labeling = forall indomain
end

let unify v x = atomic (fun () -> Fd.unify v x)


let level g =
  create (fun () -> let l = Fcl_stak.level () in g l) ()

let sigma ?(domain = Fcl_domain.int) g =
  create (fun () -> g (Fd.create domain)) ()

let once g =
  level (fun l -> g &&~ atomic (fun () -> Fcl_stak.cut l))

let minimize_restart step goal cost compute_solution =
  let name = "Goals.minimize_restart" in
  (* +step because we constrain the cost before the search starts in
     the recursive goal *)
  let best_cost = ref (Fd.max cost) and once_more = ref true in
  let loop_level = ref (Fcl_stak.level ()) in

  create_rec
    (fun self ->
      if !once_more then begin (* First iteration or last one succeeded *)
	once_more := false;
        (* Stores the choice-point and constrains the cost *)
	({ name = Printf.sprintf "%s.restart_store" name;
	   call =
	   fun () ->
	     begin match Fd.value cost with
	       Val c -> if c > !best_cost then Fcl_stak.fail name
	     | Unk _a -> Fd.refine_up cost !best_cost end;
	     loop_level := Fcl_stak.level ();
	     Some goal}
 	   &&~ (* One solution found *)
	 { name = Printf.sprintf "%s: found_one" name;
	   call =
	   fun () ->
	     once_more := true;
	     let m = Fd.elt_value cost in
	     compute_solution m;
	     best_cost := m - step;
	     Fcl_stak.cut !loop_level;
	     Fcl_stak.fail name})
      ||~
	self end
      (* Last try failed *)
      else Fcl_stak.fail name)


let minimize_continue step goal cost compute_solution =
  let best_cost = ref (Fd.max cost) in

  let rec bt_until l =
  (* Backtrack until lower bound better than current cost, staying above [l] *)
    let gs = Fcl_stak.backtrack () in
    if Fd.min cost <= !best_cost then ignore (Fcl_stak.save gs)
    else if Fcl_stak.older (Fcl_stak.level ()) l then Fcl_stak.fail "continue"
    else bt_until l in

  let restore_max =
    let update _ =
      (*Printf.printf "\nupdate %d\n%!" !best_cost;*)
      match Fd.value cost with
	Val v ->
	  if v > !best_cost then Fcl_stak.fail "Goals.restore_max" else true
      | Unk _attr ->
	  Fd.refine_up cost !best_cost;
	  false
    and delay x = C.delay [on_choice_point] x in
    C.create ~name:"restore_cost" update delay in

  let found_one l =
    { name = "found_one";
      call =
      fun () ->
        let c = Fd.elt_value cost in
      	compute_solution c;
	best_cost := c - step;
	bt_until l;
      	Fcl_stak.fail "Goals.minimize_more" } in

  let init =
    { name = "continue_init";
      call = fun () -> C.post restore_max; Some goal } in

  level (fun l -> init &&~ found_one l)


let minimize_dicho_restart step goal cost compute_solution =
  let name = "Goals.minimize_dicho" in
  let lb = ref (Fd.min cost) and ub = ref (Fd.max cost)
  and left = ref true and sol = ref true in
  let loop_level = ref (Fcl_stak.level ()) in
  create_rec
    (fun self ->
      if (!left || !sol) && !lb <= !ub then
	let mid = (!ub + !lb) / 2 in
	({name = Printf.sprintf "%s: restart_store" name;
	  call =
	  fun () ->
	    (*Printf.printf "\nupdate sol:%b left:%b lb:%d ub:%d\n\n%!" !sol !left !lb !ub;*)
	    if !sol then begin
	      sol := false; left := true;
	      Fd.refine_low_up cost !lb mid end
	    else begin
	      sol := false; left := false; lb := mid+1;
	      Fd.refine_low_up cost (mid+1) !ub end;
	    loop_level := Fcl_stak.level ();
	    Some goal}
 	   &&~
	 { name = Printf.sprintf "%s: found_one" name;
	   call =
	   fun () ->
	     let m = Fd.elt_value cost in
	     compute_solution m;
	     sol := true;
	     ub := m-step;
	     Fcl_stak.cut !loop_level;
	     Fcl_stak.fail name})
      ||~
	self
      else Fcl_stak.fail name)

(********* C'EST PAS ENCORE ÇA... **********)
(*
let minimize_dicho_continue step goal cost compute_solution =
  let name = "Goals.minimize_dicho_continue" in
  let lb = ref (Fd.min cost) and ub = ref (Fd.max cost)
  and left = ref true and sol = ref true in

  let rec bt_until l =
  (* Backtrack until lower bound better than current cost, staying below [l] *)
    let gs = Fcl_stak.backtrack () in
    let mid = (!ub + !lb) / 2 in
    if Fd.min cost <= mid then ignore (Fcl_stak.save gs)
    else if Fcl_stak.older (Fcl_stak.level ()) l then Fcl_stak.fail name
    else bt_until l in

  let restore_max =
    let name = "Goals.restore_max" in
    let update _ =
      Printf.printf "\nupdate sol:%b left:%b lb:%d ub:%d\n\n%!"
	!sol !left !lb !ub;
      let mid = (!ub + !lb) / 2 in
      if !sol then begin
	sol := false; left := true;
	Fd.refine_low_up cost !lb mid end
      else begin
	sol := false; left := false; lb := mid+1;
	Fd.refine_low_up cost (mid+1) !ub end;
      false
    and delay x = C.delay [on_choice_point] x in
    C.create ~name update delay in

  let found_one l =
    { name = "found_one";
      call =
      fun () ->
	let mid = (!ub + !lb) / 2 in
        let c = Fd.elt_value cost in
      	compute_solution c;
	sol := true;
	ub := c - step;
	bt_until l;
      	Fcl_stak.fail name } in

  let init =
    { name = "continue_init";
      call = fun () -> C.post restore_max; Some goal } in

  create_rec
    (fun self ->
      if (!left || !sol) && !lb <= !ub then
	let l = Fcl_stak.level () in
	(init &&~ found_one l) ||~ self
      else Fcl_stak.fail name)
*)

type bb_mode = Restart | Continue | Dicho
let minimize ?(step=1) ?(mode = Continue) g c cs =
  if step <= 0 then invalid_arg "Goals.minimize: step must be non negative";
  match mode with
    Restart -> minimize_restart step g c cs
  | Continue -> minimize_continue step g c cs
  | Dicho -> minimize_dicho_restart step g c cs

(*
type bb_strategy = Descent | Dicho

let minimize ?(step=1) ?(mode = Continue) ?(strategy = Descent) g c cs =
  if step <= 0 then invalid_arg "Goals.minimize: step must be non negative";
  match (mode, strategy) with
    (Restart, Descent) -> minimize_restart step g c cs
  | (Continue, Descent) -> minimize_continue step g c cs
  | (Restart, Dicho) -> minimize_dicho_restart step g c cs
  | (Continue, Dicho) ->  minimize_dicho_continue step g c cs
*)

let lds ?(step = 1) goal =
  let lds_max = ref 0 and more = ref true and lds = Fcl_stak.ref 0 in

  let lds_check =
    let update _ =
      Fcl_stak.set lds (Fcl_stak.get lds + 1);
      if Fcl_stak.get lds > !lds_max then begin
	more := true; Fcl_stak.fail "Goals.lds_check" end
      else false
    and delay x = C.delay [on_choice_point] x
    and init _ = false in
    C.create ~name:"lds_check" ~init update delay in

  { name = "lds_init";
    call = (fun () ->
      (* lds must be less than lds_max for lds_check not to fail when
	 backtracking just before executing "self" *)
      lds_max := -step; more := true; Fcl_stak.set lds (!lds_max - 1);
      C.post lds_check; None) }
    &&~
  (create_rec ~name:"lds_iterate"
     (fun self ->
       if not !more then Fcl_stak.fail "Goals.lds" else begin
	 lds_max := !lds_max + step;
	 more := false;
	 Fcl_debug.call 'l' (fun f -> Printf.fprintf f "lds_max=%d\n" !lds_max);
	 (atomic (fun () -> Fcl_stak.set lds 0) &&~ goal) ||~ self end))


module Conjunto = struct
  let indomain d =
    create_rec ~name:"Goals.Conjunto.indomain"
      (fun self ->
	match SetFd.value d with
	  Val _ -> success
	| Unk a ->
	    let (glb, lub) = Fcl_setDomain.min_max a in
	    let diff = Fcl_setDomain.S.diff lub glb in
	    let x = Fcl_setDomain.S.choose diff in
	    ( ( atomic (fun () -> Fcl_conjunto.outside x d)
	     ||~ atomic (fun () -> Fcl_conjunto.inside x d)) &&~ self) )
end
