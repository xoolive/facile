module C = Fcl_cstr

module type DOMAIN = sig
  type elt
  type t
  val is_empty : t -> bool
  val is_bound : t -> bool
  val elt_value : t -> elt
  val dom_changed : t -> t -> bool
  val min_changed : t -> t -> bool
  val max_changed : t -> t -> bool
  val compare_elt : elt -> elt -> int
  val fprint_elt : out_channel -> elt -> unit
  val fprint : out_channel -> t -> unit
  val to_string : t -> string
  val min : t -> elt
  val max : t -> elt
  val min_max : t -> elt * elt
  val mem : elt -> t -> bool
  val interval : elt -> elt -> t
  val elt : elt -> t
  val remove_low : elt -> t -> t
  val remove_up : elt -> t -> t
  val remove_low_up : elt -> elt -> t -> t
  val included : t -> t -> bool
  type size
  val size : t -> size
  val strictly_inf : elt -> elt -> bool
      (* [strictly_inf x1 x2] checks if [x1 < x2] when it is already known that [x1 <= x2] *)
end

let gen_int = Fcl_misc.gen_int_fun ()

type ('a, 'b) concrete = Unk of 'a | Val of 'b

module type BASICVAR = sig
  type t
  type elt
  type domain
  type size
  type event
  val create : ?name:string -> domain -> t
  val interval : ?name:string -> elt -> elt -> t
  val array : ?name:string -> int -> elt -> elt -> t array
  val elt : elt -> t
  val is_var : t -> bool
  val is_bound : t -> bool
  val value : t -> (domain, elt) concrete
  val dom : t -> domain
  val min : t -> elt
  val max : t -> elt
  val min_max : t -> elt * elt
  val elt_value : t -> elt
  val size : t -> size
  val member : t -> elt -> bool
  val id : t -> int
  val name : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val active_store : unit -> t list
  val fprint : out_channel -> t -> unit
  val fprint_array : out_channel -> t array -> unit
  val to_string : t -> string
  val constraints_number : t -> int
  val wdeg : t -> int
  val twdeg : t -> int
  val tightness : t -> float
  val unify : t -> elt -> unit
  val refine : t -> domain -> unit
  val refine_low : t -> elt -> unit
  val refine_up : t -> elt -> unit
  val refine_low_up : t -> elt -> elt -> unit
  val on_refine : event
  val on_subst : event
  val on_min : event
  val on_max : event
  val delay : event list -> t -> ?waking_id:int -> Fcl_cstr.t -> unit
  val int : elt -> t
  val subst : t -> elt -> unit
  val unify_cstr : t -> elt -> Fcl_cstr.t
end

module MakeVar(Domain : DOMAIN) = struct
  type domain = Domain.t
  type elt = Domain.elt
  type size = Domain.size
  type t = {
      dom : domain Fcl_stak.ref;
      on_refine : C.event;
      on_min : C.event;
      on_max : C.event;
      on_subst : C.event;
      id : int;
      name : string
    }
  type event = t -> C.event

  let dom var = Fcl_stak.get var.dom
  let id var = var.id
  let name var = var.name

  let is_var var = not (Domain.is_bound (Fcl_stak.get var.dom))

  let is_bound var = Domain.is_bound (Fcl_stak.get var.dom)

  type var = t
  module Store_var = struct
    type t = var
    let withdraw = is_bound
    (* When backtracking on (Store.add v), the variable
       may have been withdrawn by the GC, and then a bound
       variable might instead be tested (id'ed). -1 is
       returned in such a case. *)
    let id v = if is_var v then id v else -1
  end
  module Store = Fcl_weakstore.MakeStore(Store_var)
  let active_store = Store.active_store

  let create ?(name = "") domain =
    if Domain.is_empty domain then
      let msg =
	Printf.sprintf "Var.XxxFd.create: empty initial domain (%s)" name in
      Fcl_stak.fail msg
    else
      let id = gen_int () in
      let name = if name = "" then Printf.sprintf "_%d" id else name in
      let var =
        {dom = Fcl_stak.ref domain;
         on_refine = C.new_event ();
         on_min = C.new_event ();
         on_max = C.new_event ();
         on_subst = C.new_event ();
         id;
         name} in
      Store.add var;
      var

  let elt i = create (Domain.interval i i)
  let int = elt

  let interval ?name min max = create ?name (Domain.interval min max)

  let array ?name n min max =
    let dom = Domain.interval min max in
    let name_elt =
      match name with
	None -> fun _i -> None
      |	Some n -> fun i -> Some (Printf.sprintf "%s_%d" n i) in
    Array.init n (fun i -> create ?name:(name_elt i) dom)

  let member var x =
    if is_bound var then Domain.elt_value (Fcl_stak.get var.dom) = x
    else  Domain.mem x (Fcl_stak.get var.dom)

  let subst var x =
    if is_var var then
      	if member var x then begin
	  Fcl_stak.set var.dom (Domain.elt x);
	  C.schedule var.on_subst;
	  C.schedule var.on_refine;
	  C.schedule var.on_min;
	  C.schedule var.on_max;
	  C.wake_all ()	end
      	else Fcl_stak.fail "Var.XxxFd.subst"
    else
      let msg = "XxxFd.subst: bound variable (use XxxFd.unify on possibly \
          bound variable)" in
      Fcl_debug.fatal_error msg

  let fprint c var =
    Printf.fprintf c "%s%a" var.name Domain.fprint (Fcl_stak.get var.dom)

  let to_string var =
    Printf.sprintf "%s%s" var.name (Domain.to_string (Fcl_stak.get var.dom))

  let elt_value var =
    let d = dom var in
    if Domain.is_bound d then Domain.min d
    else
      let mesg =
        Printf.sprintf "XxxFd.elt_value: unbound variable %s"
          (to_string var) in
      Fcl_debug.fatal_error mesg

  let value var =
    if is_var var then Unk (Fcl_stak.get var.dom) else Val (elt_value var)

  let refine var newdom =
    if is_var var then begin
      Fcl_debug.call 'v' (fun s -> Printf.fprintf s "refine %a with %a\n" fprint var Domain.fprint newdom);
      assert (Domain.included newdom (dom var));
      if Domain.is_empty newdom then Fcl_stak.fail "Var.XxxFd.refine"
      else if Domain.is_bound newdom then subst var (Domain.elt_value newdom)
      else
        let dom = Fcl_stak.get var.dom in
        if Domain.dom_changed dom newdom then begin
      	  Fcl_stak.set var.dom newdom;
	  C.schedule var.on_refine;
	  if Domain.min_changed dom newdom then C.schedule var.on_min;
      	  if Domain.max_changed dom newdom then C.schedule var.on_max;
	  C.wake_all () end end
    else if not (Domain.mem (Domain.elt_value (Fcl_stak.get var.dom)) newdom) then
      Fcl_stak.fail "Var.XxxFd.refine"

  let unify var x =
    if is_bound var then begin
      if elt_value var <> x then Fcl_stak.fail "Var.XxxFd.unify" end
    else subst var x

  let unify_cstr var value =
    let update _ = unify var value; true
    and delay _ = () in (* Solved when posted *)
    C.create ~name:"unify_cstr" ~priority:C.immediate update delay

  (* refinements shortcuts to avoid explicit Fd.value matchings *)
  let refine_up var ub =
    if is_bound var then begin
      if Domain.compare_elt (elt_value var) ub > 0 then
        Fcl_stak.fail "Var.XxxFd.refine_up" end
    else refine var (Domain.remove_up ub (dom var))

  let refine_low var lb =
    if is_bound var then begin
      if Domain.compare_elt (elt_value var) lb < 0 then
        Fcl_stak.fail "Var.XxxFd.refine_low" end
    else refine var (Domain.remove_low lb (dom var))

  let refine_low_up var lb ub =
    Fcl_debug.call 'v'
      (fun s -> Printf.fprintf s "Var.XxxFd.refine_min_max: %a %a\n"
	  Domain.fprint_elt lb Domain.fprint_elt ub);
    if is_bound var then begin
      let x = elt_value var in
      if Domain.compare_elt x lb < 0 || Domain.compare_elt x ub > 0 then
	Fcl_stak.fail "Var.XxxFd.refine_low_up" end
    else refine var (Domain.remove_low_up lb ub (dom var))

  let fprint_array c vs =
    let n = Array.length vs in
    Printf.fprintf c "[|";
    for i = 0 to n - 2 do Printf.fprintf c "%a; " fprint vs.(i) done;
    if n = 0 then Printf.fprintf c "|]" else
    Printf.fprintf c "%a|]" fprint vs.(n-1)

  let min var = Domain.min (dom var)

  let max var = Domain.max (dom var)

  let min_max var = Domain.min_max (dom var)

  let compare v1 v2 =
    match is_bound v1, is_bound v2 with
      true, true -> Domain.compare_elt (elt_value v1) (elt_value v2)
    | true, false -> -1
    | false, true -> 1
    | false, false -> compare v1.id v2.id

  let equal v1 v2 = compare v1 v2 = 0

  let cstrs_hash var =
(* Not all constraints are suspended on on_subst (e.g. <~), so
   all the constraints lists have to be taken into account *)
    let h = Hashtbl.create 17 in
    let mem_add c =
      let id = C.id c in
      if not (Hashtbl.mem h id) then Hashtbl.add h id c in
    let add_list l =
      List.iter (fun (c, _) -> if not (C.is_solved c) then mem_add c) l in
    add_list (C.registered var.on_subst);
    add_list (C.registered var.on_min);
    add_list (C.registered var.on_max);
    add_list (C.registered var.on_refine);
    h

  let constraints_number a =
    let h = cstrs_hash a in
    Hashtbl.length h

  let wdeg a =
    let h = cstrs_hash a in
    Hashtbl.fold (fun _id c r -> C.weight c + r) h 0

  let twdeg a =
    let h = cstrs_hash a in
    Hashtbl.fold
      (fun _id c r ->
	if Fcl_domain.size (C.freevars c) > 1 then C.weight c + r else r) h 0

  let tightness a =
    let h = cstrs_hash a in
    Hashtbl.fold
      (fun _id c r ->
	if Fcl_domain.size (C.freevars c) > 1 then C.tightness c +. r else r)
      h 0.

  let on_refine var = var.on_refine
  let on_subst var = var.on_subst
  let on_min var = var.on_min
  let on_max var = var.on_max

  let size var = Domain.size (Fcl_stak.get var.dom)

  let delay events var ?waking_id c =
    if is_var var then
      C.delay (List.map (fun ev -> ev var) events) ?waking_id c
end

module type FD = sig
  include BASICVAR
  val remove : t -> elt -> unit
  val values : t -> elt list
  val iter : (elt -> unit) -> t -> unit
  val vars2ids : t list -> Fcl_domain.t
end

module BasicFd = MakeVar(Fcl_domain)
module Fd = struct
  include BasicFd

  let values var = Fcl_domain.values (dom var)

  let iter f var = Fcl_domain.iter f (dom var)

  let remove var x =
    if is_bound var then begin
      if Fcl_domain.compare_elt (elt_value var) x = 0 then
        Fcl_stak.fail "Var.XxxFd.remove" end
    else refine var (Fcl_domain.remove x (dom var))

  let vars2ids vars =
    List.fold_left
      (fun acc v -> if is_var v then Fcl_domain.add (id v) acc else acc)
      Fcl_domain.empty vars
end

module SetFd = MakeVar(Fcl_setDomain)
