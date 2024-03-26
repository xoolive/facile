(* $Id: fcl_setDomain.ml,v 1.3 2005-10-25 12:45:35 barnier Exp $ *)

(* Renaming of Fcl_domain *)
module S = struct
  include Fcl_domain
  let subset = included
  let cardinal = size
  let choose = min
  let max_elt = max
  let min_elt = min
  let elements = values
  let equal x y = x = y
  let inter = intersection
  let singleton x = create [x]
end

type elt = S.t
type t = { glb : elt; lub : elt}

let empty = {glb = S.empty; lub = S.empty}

let unsafe_interval glb lub =
  assert(S.subset glb lub);
  { glb = glb; lub = lub }

let interval glb lub =
  if not (S.subset glb lub) then invalid_arg "SetDomain.interval: min > max";
  { glb = glb; lub = lub }

let elt x = {glb = x; lub = x}

let elt_of_list l =
  List.fold_right S.add l S.empty

type size = int

let size d =
  if d = empty then 0 else
  S.cardinal d.lub - S.cardinal d.glb + 1

let is_empty d = size d = 0
let is_bound d = size d = 1

let elt_value d = assert (is_bound d); d.glb

let dom_changed old new_ = size new_ < size old

let min_changed old new_ = S.cardinal old.glb < S.cardinal new_.glb

let max_changed old new_ = S.cardinal new_.lub  < S.cardinal old.lub


let min s = s.glb
let max s = s.lub
let min_max d = (min d, max d)
let mem s d = S.subset d.glb s && S.subset s d.lub
let included d1 d2 = S.subset d2.glb d1.glb && S.subset d1.lub d2.lub

(* EXPONENTIAL *)
let iter f d =
  let diff = S.diff d.lub d.glb in
  let rec loop current possibles =
    if S.is_empty possibles then
      f current
    else
      let x = S.choose possibles in
      let rest = S.remove x possibles in
      loop (S.add x current) rest;
      loop current rest in
  loop d.glb diff

(* EXPONENTIAL *)
let values d =
  let l = ref [] in
  iter (fun x -> l := x :: !l) d;
  !l

let fprint_elt = S.fprint

let fprint c d =
  if is_bound d then
    Printf.fprintf c "{%a}" S.fprint d.glb
  else Printf.fprintf c "{%a..%a}" S.fprint d.glb S.fprint d.lub

let to_string d =
  if is_bound d then
    Printf.sprintf "{%s}" (S.to_string d.glb)
  else Printf.sprintf "{%s..%s}" (S.to_string d.glb) (S.to_string d.lub)

let intersection = S.inter

let strictly_inf a b = S.cardinal a < S.cardinal b

let compare_elt = S.compare

let remove_low x d =
  if S.subset x d.glb then d else
  if S.subset d.lub x then empty else
  if S.subset x d.lub then {glb = x; lub = d.lub}
  else Fcl_debug.fatal_error "Setdomain.remove_low"

let remove_up x d =
  if S.subset d.lub x then d else
  if S.subset x d.glb then empty else
  if S.subset d.glb x then {glb = d.glb; lub = x}
  else Fcl_debug.fatal_error "Setdomain.remove_up"

(* to optimize? *)
let remove_low_up x y d =
  remove_low x (remove_up y d)
