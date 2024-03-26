module Array = struct
  let set t i v =
    let old = t.(i) in
    t.(i) <- v; Fcl_stak.trail (fun () -> t.(i) <- old)
end

module Hashtbl = struct
  type ('a, 'b) t = ('a, 'b Fcl_stak.ref) Hashtbl.t

  let create n = Hashtbl.create n

  let _get h = h

  let add h k d =
    Hashtbl.add h k (Fcl_stak.ref d);
    Fcl_stak.trail (fun () -> Hashtbl.remove h k)

  let persistent_add h k d = Hashtbl.add h k (Fcl_stak.ref d)

  let remove h k =
    let d = Hashtbl.find h k in
    Hashtbl.remove h k;
    Fcl_stak.trail (fun () -> Hashtbl.add h k d)

  let find h k = Fcl_stak.get (Hashtbl.find h k)

  let replace h k d =
    Fcl_stak.set (Hashtbl.find h k) d

  let mem = Hashtbl.mem

  let iter f h = Hashtbl.iter (fun k d -> f k (Fcl_stak.get d)) h

  let fold f h init = Hashtbl.fold (fun k d r -> f k (Fcl_stak.get d) r) h init
end

module type CONTAINER = sig
  type t
  val empty : t
  val add : int -> t -> t
  val remove : int -> t -> t
end

module type MEMOIZE = sig
  type 'a t
  type set
  val create : int -> 'a t
  val add : 'a t -> 'a -> int -> unit
  val bt_add : 'a t -> 'a -> int -> unit
  val mem : 'a t -> 'a -> bool
  val find : 'a t -> 'a -> set
  val find_opt : 'a t -> 'a -> set option
  val remove : 'a t -> 'a -> int -> unit
  val remove_isempty : 'a t -> 'a -> int -> bool
  val bt_remove : 'a t -> 'a -> int -> unit
  val bt_remove_isempty : 'a t -> 'a -> int -> bool
  val remove_all : 'a t -> 'a -> unit
  val bt_remove_all : 'a t -> 'a -> unit
  val iter : ('a -> set -> unit) -> 'a t -> unit
  val fold : ('a -> set -> 'c -> 'c) -> 'a t -> 'c -> 'c
end

module MakeMemoize(C : CONTAINER) = struct
  type 'a t = ('a, C.t ref) Hashtbl.t

  type set = C.t

  let create n = Hashtbl.create n

  let add h k d =
    try
      let r = Hashtbl.find h k in
      r := C.add d !r
    with Not_found -> Hashtbl.add h k (ref (C.add d C.empty))

  let bt_add h k d =
    try
      let r = Hashtbl.find h k in
      r := C.add d !r;
      Fcl_stak.trail (fun () -> r := C.remove d !r)
    with Not_found ->
      Hashtbl.add h k (ref (C.add d C.empty));
      Fcl_stak.trail (fun () -> Hashtbl.remove h k)

  let mem = Hashtbl.mem

  let find h k = !(Hashtbl.find h k)

  let find_opt h k =
    try Some (find h k) with Not_found -> None

  let remove h k d =
    let r = Hashtbl.find h k in
    r := C.remove d !r

  let remove_isempty h k d =
    let r = Hashtbl.find h k in
    let set = C.remove d !r in
    r := set;
    set = C.empty

  let bt_remove h k d =
    let r = Hashtbl.find h k in
    r := C.remove d !r;
    Fcl_stak.trail (fun () -> r := C.add d !r)

  let bt_remove_isempty h k d =
    let r = Hashtbl.find h k in
    let set = C.remove d !r in
    if set = C.empty then begin
      Hashtbl.remove h k;
      Fcl_stak.trail (fun () -> Hashtbl.add h k r);
      true end
    else begin
      r := set;
      Fcl_stak.trail (fun () -> r := C.add d !r);
      false end

  let remove_all = Hashtbl.remove

  let bt_remove_all h k =
    let d = !(Hashtbl.find h k) in
    Hashtbl.remove h k;
    Fcl_stak.trail (fun () -> Hashtbl.add h k (ref d))

  let iter f = Hashtbl.iter (fun k d -> f k !d)

  let fold f = Hashtbl.fold (fun k d -> f k !d)
end
