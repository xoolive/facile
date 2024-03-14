open Printf

exception DontKnow

type priority = int
let nb_priorities = 4

let immediate = 0
let normal = 1
let later = 2
let even_later = 3

type t = {
    id : int;
    name: string;
    priority : priority;
    solved : bool array;
    woken : bool array;
    nb_solved : int Fcl_stak.ref;
    fprint : out_channel -> unit;
    update : int -> unit;
    init : unit -> unit;
    check : unit -> bool;
    delay : t -> unit;
    not : unit -> t;
    weight : int ref;
    freevars : unit -> Fcl_domain.t;
    tightness : unit -> float
  }

let id c = c.id

let gen_int = Fcl_misc.gen_int_fun ()

let array_set_true t i =
  t.(i) <- true;
  Fcl_stak.trail (fun () -> t.(i) <- false)

let create ?(name = "anonymous") ?(nb_wakings = 1) ?fprint ?(priority = normal) ?init ?check ?not ?freevars ?tightness update delay =
  if nb_wakings < 1 then begin
    let msg = "Cstr.create: nb_wakings must be greater or equal to 1" in
    Fcl_debug.fatal_error msg end;
  let solved = Array.make nb_wakings false in
  (*let solved = Fcl_data.Hashtbl.create nb_wakings in*)


  let nb_solved = Fcl_stak.ref 0 in
  let weight = ref 1 in

  let init =
    match init with
      Some init -> init
   (* For not breaking constraints that don't use waking ids and
      rely on being woken at post time. If update must not be called
      at post time (e.g. because it is suspended to on_subst and the
      code rely on the fact that the variable really is instantiated)
      and waking ids are not used, init must be defined. *)
    | None when nb_wakings = 1 -> fun () -> update 0
   (* otherwise we do nothing *)
    | _ -> fun () -> false in
  let init = fun () -> if init () then Fcl_stak.set nb_solved nb_wakings in

  let update i =
    if Stdlib.not solved.(i) then
      let is_solved =
	try update i with (Fcl_stak.Fail _) as e -> incr weight; raise e in
      if is_solved then begin
       	Fcl_stak.incr nb_solved;
       	array_set_true solved i
      end in
  let check =
    match check with
      Some c -> c
    | None ->
	fun () ->
	  Fcl_debug.fatal_error (name ^ ": check callback undefined") in
  let not =
    match not with
      Some n -> n
    | None ->
	fun () -> Fcl_debug.fatal_error (name ^ ": not callback undefined") in
  let fprint =
    match fprint with
      Some f -> f
    | None -> fun c -> fprintf c "%s" name in
  let freevars =
    match freevars with
      None ->
	let mesg = Printf.sprintf "%s: free vars not implemented" name in
	fun () -> Fcl_debug.fatal_error mesg
    | Some f -> f in
  let tightness =
    match tightness with
      None ->
	(* (fun () -> 0.) *)
        let mesg = Printf.sprintf "%s: tightness not implemented" name in
	fun () -> Fcl_debug.fatal_error mesg
    | Some f -> f in
  {id = gen_int ();
   name = name;
   priority = priority;
   update = update;
   delay = delay;
   solved = solved;
   woken = Array.make nb_wakings false;
   nb_solved = nb_solved;
   fprint = fprint;
   init = init;
   check = check;
   not = not;
   weight = weight;
   freevars = freevars;
   tightness = tightness}

let fprint chan ct = Printf.fprintf chan "%d: " ct.id; ct.fprint chan

let self_delay c = c.delay

let check c = c.check

let self_init c = c.init

let _solved c = c.solved

let is_solved ct = Fcl_stak.get ct.nb_solved = Array.length ct.woken

let nb_wakings ct = Array.length ct.woken

let weight c = !(c.weight)

let freevars c = c.freevars ()

let tightness c = c.tightness ()

let queue = Array.make nb_priorities []
and already_in_wake = ref false
and next_priority = ref nb_priorities

let reset_queue () =
  for i = 0 to nb_priorities -1 do
    queue.(i) <- []
  done;
  next_priority := nb_priorities;
  already_in_wake := false;;

let assert_empty_queue () =
  assert(
    try
      for i = 0 to nb_priorities -1 do
	if queue.(i) <> [] then raise Exit
      done;
      !next_priority = nb_priorities && not !already_in_wake
    with
      Exit -> false);;

let wake_all () =
  if not !already_in_wake
  then begin
    already_in_wake := true;
    try
      while !next_priority < nb_priorities do
	match queue.(!next_priority) with
      	    [] -> incr next_priority
	  | (c, i) :: cs ->
	      queue.(!next_priority) <- cs;
	      Fcl_debug.call 'c' (fun s -> fprintf s "%s(%d)#update(%d)\n" c.name c.id i);
	      if not c.solved.(i) then c.update i;
	      Fcl_debug.call 'c' (fun s -> fprintf s "%s(%d)#updated(%d)%s\n" c.name c.id i (if is_solved c then "*" else ""));
	      c.woken.(i) <- false (* not trailed *)
      done;
      already_in_wake := false
   (* To avoid being in a state where already_in_wake = true after an
      uncaught exception during the while loop. *)
    with e ->
      reset_queue ();
      raise e
  end

let schedule_one_cstr ((cstr, i) as c) =
  Fcl_debug.call 'c' (fun s -> fprintf s "%s(%d)#scheduled(%d) - (woken:%b, solved:%b)\n" cstr.name cstr.id i cstr.woken.(i) cstr.solved.(i));
  if not (cstr.woken.(i) || cstr.solved.(i)) then begin
    Fcl_debug.call 'c' (fun s -> fprintf s "wake %d(%d): " cstr.id i; cstr.fprint s; fprintf s "\n");
    let p = cstr.priority in
    queue.(p) <- c :: queue.(p);
    next_priority := Fcl_misc.Operators.min !next_priority p;
    array_set_true cstr.woken i end


type cstr = t

module Store_cstr = struct
  type t = cstr
  let withdraw = is_solved
  let id = id
end

module Store = Fcl_weakstore.MakeStore(Store_cstr)

let active_store = Store.active_store


let post c =
  Fcl_debug.call 'c' (fun s -> fprintf s "post: "; c.fprint s; fprintf s "\n%!");
  let current_status = !already_in_wake in
  already_in_wake := true; (* Because #init may wake constraints and we want
			      to schedule them correctly *)
  begin
    try (* Because #init may fail or raise any other exception *)
      c.init ();
    with
      e ->
  	reset_queue ();
	raise e
  end;
  if not (is_solved c) then begin
    c.delay c;
    Store.add c;
  end;
  already_in_wake := current_status;
  wake_all ()

  (* post pour les démons *)
let init c =
  c.init ();
  c.delay c;;


let rec one () =
  let delay _ = ()
  and check () = true
  and update _ = true
  and not = zero in
  create ~priority:immediate ~name:"one" ~check ~not update delay
and zero () =
  let delay _ = ()
  and check () = false
  and update _ = Fcl_stak.fail "zero"
  and not = one in
  create ~priority:immediate ~name:"zero" ~check ~not update delay

let one = one ()
let zero = one.not ()

let name c = c.name
let priority c = c.priority

(* Un objet avec des contraintes qui lui sont attachées *)
type event = (t * int) list Fcl_stak.ref
let new_event () = Fcl_stak.ref []
let schedule (event : event) = List.iter schedule_one_cstr (Fcl_stak.get event)
let register event ?(waking_id=0) cstr =
  let nb_wakings = Array.length cstr.woken in
  if waking_id >= nb_wakings then begin
    let msg =
      Printf.sprintf
	"nb_wakings less one (%d) must be equal to maximum waking_id (here %d) in constraint %s" (nb_wakings - 1) waking_id cstr.name in
    Fcl_debug.fatal_error msg end;
  if not (is_solved cstr)
  then Fcl_stak.set event ((cstr, waking_id) :: Fcl_stak.get event)
let registered = Fcl_stak.get

let delay events ?waking_id c =
  List.iter
    (fun event -> register event ?waking_id c)
    events

let conjunction = function
    [] -> one
  | [cstr] -> cstr
  | cstrs ->
      let update _ = true
      and delay _ = ()
      and init () =
	List.iter (fun c -> post c) cstrs;
	true
      and fprint chan =
	List.iter (fun c -> Printf.fprintf chan "%a\n" fprint c) cstrs in
      create ~fprint ~init ~name:"conjunction" update delay

let not ct = ct.not ()
