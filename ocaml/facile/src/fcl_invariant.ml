(* $Id: fcl_invariant.ml,v 1.6 2007-08-01 15:59:17 barnier Exp $ *)

type ('a, 'b) t = {
    x : 'a Fcl_stak.ref; 
    event : Fcl_cstr.event;
    id : int;
    name : string
  }
type setable
type unsetable
type 'a setable_t = ('a, setable) t
type 'a unsetable_t = ('a, unsetable) t

let gen_int = Fcl_misc.gen_int_fun ()
let create ?(name = "") v =
  let id = gen_int () in
  let name = if name = "" then Printf.sprintf "_%d" id else name in
  {x = Fcl_stak.ref v;
   event = Fcl_cstr.new_event ();
   id;
   name}
let constant = create
let get r = Fcl_stak.get r.x
let event r = r.event
let name r = r.name
let id r = r.id
let fprint c ?(printer = (fun _ _ -> ())) r =
  Printf.fprintf c "%s%a" r.name printer (get r)
let set r v =
  let old = Fcl_stak.get r.x in
  if old <> v then begin
    Fcl_stak.set r.x v;
    Fcl_cstr.schedule r.event;
    Fcl_cstr.wake_all ()
  end 

let unary ?(name = "Invariant.unary") f x =
  let yname = Printf.sprintf "%s(%s)" name x.name in
  let y = create ~name:yname (f (get x)) in
  let update _ =
    set y (f (get x));
    false
  and delay c = Fcl_cstr.register x.event c in
  Fcl_cstr.post (Fcl_cstr.create ~name update delay);
  y

let sum_gen add minus zero array =
  let name = "Invariant.sum" in
  let n = Array.length array in
  if n = 0 then raise (Invalid_argument name);

  let rname = Printf.sprintf "sum(%s...%s)" array.(0).name array.(n-1).name in
  let r =
    create ~name:rname (Array.fold_left (fun r x -> add (get x) r) zero array)
  and lasts = Array.map get array in
  let update i =
    let new_ai = get array.(i) in
    set r (add (minus (get r) lasts.(i)) new_ai);
    Fcl_data.Array.set lasts i new_ai;
    false

  and delay c =
    Array.iteri
      (fun i ai -> Fcl_cstr.register (event ai) ~waking_id:i c)
      array in

  let c = Fcl_cstr.create ~name ~nb_wakings:n update delay in
  Fcl_cstr.post c;
  r

let sum t = sum_gen (+) (-) 0 t
let sum_float t = sum_gen (+.) (-.) 0. t

let prod_gen prod div one zero array =
  let name = "Invariant.prod" in
  let n = Array.length array in
  if n = 0 then raise (Invalid_argument name);

  let rname = Printf.sprintf "prod(%s...%s)" array.(0).name array.(n-1).name in
  let prod_array t = Array.fold_left (fun r x -> prod (get x) r) one t in
  let r = create ~name:rname (prod_array array)
  and lasts = Array.map get array in
  let update i =
    let new_ai = get array.(i) in
    if lasts.(i) <> zero then
      set r (prod (div (get r) lasts.(i)) new_ai)
    else begin
      assert(new_ai <> zero);
      set r (prod_array array) end;
    Fcl_data.Array.set lasts i new_ai;
    false

  and delay c =
    Array.iteri
      (fun i ai -> Fcl_cstr.register (event ai) ~waking_id:i c)
      array in

  let c = Fcl_cstr.create ~name ~nb_wakings:n update delay in
  Fcl_cstr.post c;
  r

let prod t = prod_gen ( * ) (/) 1 0 t
let prod_float t = prod_gen ( *. ) (/.) 1. 0. t


let binary ?(name = "Invariant.binary") f x y =
  let zname = Printf.sprintf "%s_%s" x.name y.name in
  let z = create ~name:zname (f (get x) (get y)) in
  let update _ =
    set z (f (get x) (get y));
    false
  and delay c =
    Fcl_cstr.register (event x) c; Fcl_cstr.register (event y) c; in
  Fcl_cstr.post (Fcl_cstr.create ~name update delay);
  z

let ternary ?(name = "Invariant.ternary") f x y t =
  let zname = Printf.sprintf "%s_%s_%s" x.name y.name t.name in
  let z = create ~name:zname(f (get x) (get y) (get t)) in
  let update _ =
    set z (f (get x) (get y) (get t));
    false
  and delay c =
    Fcl_cstr.register (event x) c; Fcl_cstr.register (event y) c;
    Fcl_cstr.register (event t) c in
  Fcl_cstr.post (Fcl_cstr.create ~name update delay);
  z


module Array = struct
  let argmin array f =
    let name = "Invariant.Array.argmin" in
    let n = Array.length array in
    if n = 0 then raise (Invalid_argument name);
    let idxname =
      Printf.sprintf "argmin(%s...%s)" array.(0).name array.(n-1).name in
    if n = 1 then constant ~name:idxname 0 else

    let values = Array.map (fun ai -> f (get ai)) array in
  
    let module Ord =
      struct
	type t = int
	let compare i j =	compare (values.(i), i) (values.(j), j)
      end in
    let module S = Set.Make(Ord) in

    let s = Fcl_stak.ref (Fcl_misc.goedel S.add n S.empty) in
    let idx_min = create (S.min_elt (Fcl_stak.get s)) in

    let update i =
      let last_idx_min = get idx_min in
      let last_min = values.(last_idx_min) in
      let s' = S.remove i (Fcl_stak.get s) in
      Fcl_data.Array.set values i (f (get array.(i)));
      let s'' = S.add i s' in
      Fcl_stak.set s s'';
      if (values.(i), i) < (last_min, last_idx_min) then
	set idx_min i
      else if i = last_idx_min then
	set idx_min (S.min_elt s'');
      false
    and delay c =
      Array.iteri
	(fun i ai -> Fcl_cstr.register (event ai) ~waking_id:i c)
	array in

    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.Array.argmin" ~nb_wakings:n update delay);
  
    idx_min


  let unary_get a idx =
    let name = "Invariant.Array.unary_get" in
    let n = Array.length a in
    if n = 0 then raise (Invalid_argument name);

    let rname =
      Printf.sprintf "unary_get(%s...%s).(%s)"
	a.(0).name a.(n-1).name idx.name in
    let r = create ~name:rname (get a.(get idx)) in
    let update _ = set r (get a.(get idx)); false
    and delay c = Fcl_cstr.register (event idx) c in
    Fcl_cstr.post (Fcl_cstr.create ~name update delay);
    r

  let min a f = unary_get a (argmin a f)

  let get array idx =
    let name = "Invariant.Array.get" in
    let n = Array.length array in
    if n = 0 then raise (Invalid_argument name);

    let rname =
      Printf.sprintf "get(%s...%s).(%s)"
	array.(0).name array.(n-1).name idx.name in
    let r = create ~name:rname (get array.(get idx)) in
    let update i =
      let nidx = get idx in
      assert(0 <= nidx && nidx < n);
      if i = n then set r (get array.(nidx))
      else if i = nidx then set r (get array.(nidx));
      false

    and delay c =
      Array.iteri
	(fun i ai -> Fcl_cstr.register (event ai) ~waking_id:i c)
      array;
      Fcl_cstr.register (event idx) ~waking_id:n c in

    let c = Fcl_cstr.create ~name ~nb_wakings:(n + 1) update delay in
    Fcl_cstr.post c;
    r
end

module type FD = sig
  type fd
  type elt
  val min : fd -> elt unsetable_t
  val max : fd -> elt unsetable_t
  val size : fd -> int unsetable_t
  val is_var : fd -> bool unsetable_t
  val unary : ?name:string -> (fd -> 'a) -> fd -> 'a unsetable_t
end

module MakeVar(Var : Fcl_var.BASICVAR) = struct
  type fd = Var.t
  type elt = Var.elt
  (* to avoid exceptions when v is bound *)
  let var_name v = if Var.is_var v then Var.name v else ""
  let min v =
    let name = Printf.sprintf "fd_min(%s)" (var_name v) in
    let inv = create ~name (Var.min v) in
    let update _ = set inv (Var.min v); Var.is_bound v
    and delay c = Var.delay [Var.on_min] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.XxxVar.min" update delay);
    inv
  let max v =
    let name = Printf.sprintf "fd_max(%s)" (var_name v) in
    let inv = create ~name (Var.max v) in
    let update _ = set inv (Var.max v); Var.is_bound v
    and delay c = Var.delay [Var.on_max] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.XxxVar.max" update delay);
    inv
  let size v =
    let name = Printf.sprintf "fd_size(%s)" (var_name v) in
    let inv = create ~name (Var.size v) in
    let update _ = set inv (Var.size v); Var.is_bound v
    and delay c = Var.delay [Var.on_refine] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.XxxVar.size" update delay);
    inv
  let is_var v =
    let name = Printf.sprintf "fd_is_var(%s)" (var_name v) in
    let inv = create ~name (Var.is_var v) in
    let update _ = set inv (Var.is_var v); Var.is_bound v
    and delay c = Var.delay [Var.on_subst] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.XxxVar.is_var" update delay);
    inv
  let unary ?(name = "Invariant.XxxVar.unary") h v =
    let invname = Printf.sprintf "%s(%s)" name (var_name v) in
    let inv = create ~name:invname (h v) in
    let update _ = set inv (h v); Var.is_bound v
    and delay c = Var.delay [Var.on_refine] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name update delay);
    inv
end

module Fd = MakeVar(Fcl_var.Fd)
module SetFd = MakeVar(Fcl_var.SetFd)
