(* $Id: fcl_misc.ml,v 1.17 2005-12-09 12:36:25 barnier Exp $ *)

let last_and_length l =
  let rec ll n = function
    [] -> Fcl_debug.internal_error "Fcl_misc.last_and_length: empty list"
  | [x] -> (x, n)
  | _::xs -> ll (n+1) xs
  in ll 1 l

let extremum_array comp f xs =
  let extrem = ref 0 and extrem_v = ref (f xs.(0)) in
  for i = 1 to Array.length xs - 1 do
    let fxi = f xs.(i) in
    if comp fxi !extrem_v then begin extrem := i; extrem_v := fxi end
  done;
  (!extrem, !extrem_v)

let arg_min_array t = extremum_array (<) t
let arg_max_array t = extremum_array (>) t

let sort_unique l =
  let rec remove_sorted_duplicates = function
      [] -> []
    | x :: (y::_xs as tail) when x = y -> remove_sorted_duplicates tail
    | x :: xs -> x :: remove_sorted_duplicates xs in
  remove_sorted_duplicates (List.sort compare l)

let gen_int_fun () =
  let count = ref (-1) in
  fun () -> incr count; !count

let int_overflow x =
  Fcl_debug.print_in_assert
    (float max_int > x && float min_int < x) "integer overflow\n"

module Operators = struct
  (* let ( * ) x y = *)
  (*   assert (int_overflow (float x *. float y)); *)
  (*   x * y *)

  (* let (+) x y = *)
  (*   assert (int_overflow (float x +. float y)); *)
  (*   x + y *)

  (* let (-) x y = *)
  (*   assert (int_overflow (float x -. float y)); *)
  (*   x - y *)

  let (+) = Stdlib.(+)
  let (-) = Stdlib.(-)
  let ( * ) = Stdlib.( * )

  let (=+) x y = x := !x + y
  let (=+.) x y = x := !x +. y

  let min (a : int) b = if a <= b then a else b
  let max (a : int) b = if a <= b then b else a

  let sign x = if x < 0 then (-1) else if x = 0 then 0 else 1

  let (/+) x y =
    let xy = x / y in
    if x mod y = 0 then xy else
    if sign x * sign y >= 0 then xy + 1 else xy

  let (/-) x y =
    let xy = x / y in
    if x mod y = 0 then xy else
    if sign x * sign y >= 0 then xy else xy - 1
end

let rec iter f n z =
  if n = 0 then z else f (iter f (n-1) z)

let rec goedel f n z =
  if n = 0
  then z
  else f (n-1) (goedel f (n-1) z)


let flags = (ref [] : (string * bool ref) list ref)

let assoc_or_add n =
  try
    List.assoc n !flags
  with
    Not_found ->
      let f = ref false in
      flags := (n, f) :: !flags;
      f

let protect name f =
  let already_in = assoc_or_add name in
  if !already_in then Fcl_debug.fatal_error (Printf.sprintf "%s not reentrant" name);
  already_in := true;
  try
    let x = f () in
    already_in := false;
    x
  with
    exc ->
      already_in := false;
      raise exc
