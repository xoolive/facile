module type STORABLE = sig
  type t
  val withdraw : t -> bool
  val id : t -> int
end

module MakeStore(Storable : STORABLE) = struct
  let size_store = 1024
  let store = ref (Weak.create size_store)
  let next_free = ref 0

  type t = Storable.t
    
  let compress_or_extend () =
    let size = Weak.length !store in
    let rec look_for_free from to_ =
      if to_ < size then
      	match Weak.get !store to_ with
      	  None -> copy (max (to_ + 1) from) to_
      	| Some _ -> look_for_free from (to_ + 1)
      else size (* Full *)
    and copy from to_ =
      assert(from > to_);
      if from < size then
      	match Weak.get !store from with
      	  None -> copy (from + 1) to_
      	| some_c ->
	    Weak.set !store to_ some_c;
	    Weak.set !store from None;
	    look_for_free (from + 1) (to_ + 1)
      else to_ in
    next_free := look_for_free 0 0;
    if !next_free > size / 2 then begin
      let old_store = !store in
      store := Weak.create (size * 2);
      Weak.blit old_store 0 (!store) 0 size;
    end
      
  let add = fun c ->
    if not (Storable.withdraw c) then begin
      let size = Weak.length !store in
      if !next_free >= size then begin
      	assert(!next_free = size);
      	compress_or_extend () (* Set next_free *)
      end;
      Weak.set !store !next_free (Some c);
      let c_idx = !next_free in
      incr next_free;
      let id = Storable.id c in
      Fcl_stak.trail
      	(fun () ->
        (* le weak pointer de c a ete eventuellement supprime par le GC et
	   la compression *)
	  match Weak.get !store c_idx with
	    Some c' when Storable.id c' <> id -> ()
	  | _ -> decr next_free)
    end
	
  let active_store () =
    let rec loop active i = 
      if i < !next_free then
      	loop
	  (match Weak.get !store i with
	    None -> active
      	  | Some c ->
	      if Storable.withdraw c then active else (c::active))
	  (i+1)
      else
      	active in
    loop [] 0
end

module type STORE = sig
  type t
  val add : t -> unit
  val active_store : unit -> t list
end
