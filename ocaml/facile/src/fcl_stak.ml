(* $Id: fcl_stak.ml,v 1.25 2007-07-26 13:14:24 barnier Exp $ *)

type gl = {
    name : string;
    call : unit -> gl option
  }

type level = int
let older = ((<=) : level -> level -> bool)

exception Empty_stack
exception Level_not_found of int

type cont = Alive of gl list | Cut
type lev = {
    level: level;
    mutable success: cont;
    mutable failure: stack; (* mutability only used by cut_bottom *)
    last_level : stack
  }
and  stack = Level of lev
           | Empty
           | Trail of (unit -> unit) * stack

let gen_int = Fcl_misc.gen_int_fun ()

let stack = ref Empty
let top_level = ref !stack
let nb_levels = ref 0
let nb_choice_points = ref 0
let reset () =
  stack := Empty;
  top_level := Empty;
  nb_levels := 0
let bottom_level = gen_int ()
let save x =
  let l = gen_int () in
  stack := Level {level=l; success=Alive x; failure = !stack; last_level = !top_level};
  top_level := !stack;
  incr nb_levels;
  incr nb_choice_points;
  l

let level () =
  let rec c = function
      Level {success=Cut; last_level=st; _} -> c st
    | Level {level=l; _} -> l
    | Empty -> bottom_level
    | Trail (_, _) -> Fcl_debug.internal_error "Stak.level" in
  c !top_level

let levels () =
  let rec c = function
      Level {success=Cut; last_level=st; _} -> c st
    | Level {last_level=st; level=l; _} -> l :: c st
    | Empty -> [bottom_level]
    | Trail (_, _) -> Fcl_debug.internal_error "Stak.level" in
  c !top_level

let backtrack () =
  let rec bt = function
      Level {success=Cut; failure=s; _} -> bt s (* level was cut *)
    | Level {success=Alive x; failure=s; last_level=l; _} ->
 	stack := s; top_level := l;
 	decr nb_levels;
	x
    | Empty -> reset (); raise Empty_stack
    | Trail (undo, s) -> undo (); bt s in
  bt !stack

let backtrack_all () =
  let rec bt = function
      Level {failure=s; _} -> bt s
    | Empty -> reset ()
    | Trail (undo, s) -> undo (); bt s in
  bt !stack

let size () =
  let rec count n = function
      Level {failure=s; _} -> count n s
    | Empty -> n
    | Trail (_undo, s) -> count (n+1) s in
  count 0 !stack

let depth () = !nb_levels

let trail undo = if !stack <> Empty then stack := Trail (undo, !stack)

let cut level =
  if level = bottom_level then reset () else
  let rec c to_cut = function
      Level {level=l; _} when l = level ->
	List.iter (fun ll -> ll.success <- Cut) to_cut;
	nb_levels := !nb_levels - List.length to_cut
    | Level {success=Cut; last_level=last; _} -> c to_cut last
    | Level ({last_level=last; _} as ll) ->
	Fcl_debug.call 'S' (fun f -> Printf.fprintf f "cut %d-1\n" !nb_levels);
	c (ll :: to_cut) last
    | Empty -> raise (Level_not_found level)
    | Trail _ -> Fcl_debug.internal_error "cut" in
  c [] !top_level

let cut_bottom level =
  if level <> bottom_level then
    let rec c = function
	Level ({level=l; _} as ll) when l = level ->
	  ll.failure <- Empty;
	  ll.success <- Cut
      |	Level {last_level=last; _} ->
	  incr nb_levels;
	  c last
      | Empty -> raise (Level_not_found level)
      | Trail _ -> Fcl_debug.internal_error "cut_bottom" in
    nb_levels := 0;
    c !top_level


type 'a ref = {mutable contents : 'a; mutable timestamp : int}
let get x = x.contents
let unsafe_set r x = r.contents <- x
let ref x =
  {contents = x; timestamp = level ()}
let set refb value =
  let {contents = old; timestamp = os } = refb in
  refb.contents <- value;
  match !top_level with
    Level {level=l; _} when os <> l ->
      refb.timestamp <- l;
      assert(!stack <> Empty);
      stack := Trail ((fun () -> refb.contents <- old), !stack)
  | _ -> ()
let incr refb = set refb (get refb + 1)
let decr refb = set refb (get refb - 1)

exception Fail of string
let fail x = raise (Fail x)

let nb_choice_points () = !nb_choice_points

let print_current_level c =
  Printf.fprintf c "%d" (level ())
