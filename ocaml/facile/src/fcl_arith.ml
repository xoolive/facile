open Fcl_misc.Operators
open Fcl_var
open Fcl_expr

type t = Fcl_expr.t
let fprint = Fcl_expr.fprint
let eval = Fcl_expr.eval
let min_of_expr = Fcl_expr.min_of_expr
let max_of_expr = Fcl_expr.max_of_expr
let min_max_of_expr = Fcl_expr.min_max_of_expr

let i2e x = Inte x
let fd2e x = match Fd.value x with Val i -> Inte i | _ -> Fde (Var x)
let ( +~ ) x y =
  let l = List.sort compare_intexpr [(1, x); (1, y)] in
  Agg (Pluse, l, 0)
(*let sum tx =
  let l =
    List.sort compare_intexpr (Array.to_list (Array.map (fun x -> (1, x)) tx)) in
  Agg (Pluse, l, 0)*)
let sume t1x =
  Array.sort compare_intexpr t1x;
  let l1x = Array.to_list t1x in
  match l1x with
    [] -> Inte 0
  | _ -> Agg (Pluse, l1x, 0)
let sum tx =
  let t1x = Array.map (fun x -> (1, x)) tx in
  sume t1x
let sum_fd tx =
  let t1x = Array.map (fun x -> (1, fd2e x)) tx in
  sume t1x
let ( *~ ) x y =
  let l = List.sort compare_intexpr [(1, x); (1, y)] in
  Agg (Multe, l, 1)
let ( -~ ) x y = Agg (Pluse, [(-1, y); (1, x)], 0)
let ( /~ ) x y = Bin (Dive, x, y)
let ( %~ ) x y = Bin (Mode, x, y)
let abs x = Un (Abse, x)

let ( **~ ) x n =
  if n < 0 then Fcl_debug.fatal_error "**~ : negative exponent"
  else if n = 0 then Inte 1
  else Agg (Multe, [(n, x)], 1)

let prod es =
  let l = List.map (fun x -> (1, x)) (Array.to_list es) in
  let l = List.sort compare_intexpr l in
  match l with
    [] -> Inte 1
  | _ -> Agg (Multe, l, 1)

let prod_fd vs = prod (Array.map fd2e vs)

let scalprod scals exps =
  if Array.length scals <> Array.length exps then
    Fcl_debug.fatal_error "Arith.scalprod : arrays have not the same length";
  sum (Array.mapi (fun i ei -> ei *~ i2e scals.(i)) exps)

let scalprod_fd ints vars = scalprod ints (Array.map fd2e vars)

let (=~) e1 e2 =  constrain (e1 -~ e2) Fcl_linear.Equal
let (<>~) e1 e2 = constrain (e1 -~ e2) Fcl_linear.Diff
let (<=~) e1 e2 =  constrain (e1 -~ e2) Fcl_linear.LessThan
let (>=~) e1 e2 = e2 <=~ e1
let (<~) e1 e2 =  constrain (e1 -~ e2 +~ i2e 1) Fcl_linear.LessThan
let (>~) e1 e2 = e2 <~ e1

let e2fd (*e*) = function
  (*match reduce e with*)
    Inte x -> Fd.int x
  | Fde (Var v) -> v
  | Fde (Aux _) -> assert false
  | re -> begin
      let (a, b) = min_max_of_expr re in
      let v = Fd.interval a b in
      Fcl_cstr.post (fd2e v =~ re);
      v end

let reify_bin op e1 e2 = fd2e (Fcl_reify.boolean (op e1 e2))

let (=~~) = reify_bin (=~)
let (>=~~) = reify_bin (>=~)
let (<=~~) = reify_bin (<=~)
let (<~~) = reify_bin (<~)
let (>~~) = reify_bin (>~)
let (<>~~) = reify_bin (<>~)

let shift x d =
  let (a, b) = Fd.min_max x in
  let y = Fd.interval (a + d) (b + d) in
  Fcl_cstr.post (Fcl_linear.shift_cstr y x d);
  y

let get_boolsum_threshold = Fcl_linear.get_boolsum_threshold
let set_boolsum_threshold = Fcl_linear.set_boolsum_threshold
