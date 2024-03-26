(* $Id: fcl_reify.ml,v 1.22 2007-07-26 13:08:19 barnier Exp $ *)

open Fcl_var
module C = Fcl_cstr

let reification c b on_not =
  let negation = if on_not then Some (C.not c) else None in
  let name = Printf.sprintf "reified %s" (C.name c)
  and fprint s = Printf.fprintf s "reification: "; C.fprint s c
  and delay x =
    C.self_delay c x;
    begin if on_not then
      match negation with
        None -> failwith "Reify.reification.delay: unreachable"
      | Some negc -> (C.self_delay negc) x end;
    Fd.delay [Fd.on_subst] b x

  and update _ =
    match Fd.value b with
      Val vb ->
	Fcl_cstr.post
          (if vb = 0 then
            match negation with None -> C.not c | Some negc -> negc
          else c);
	true
    | Unk _ ->
	try
	  if C.is_solved c || C.check c () then begin
	    Fd.unify b 1
	  end else begin
	    Fd.unify b 0
	  end;
	  true
	with
	  Fcl_cstr.DontKnow -> false in

  let freevars () =
    let c_ids = C.freevars c in
    let b_id = Fd.vars2ids [b] in
    if Fcl_domain.is_empty b_id then c_ids else Fcl_domain.union c_ids b_id in
(*
  let init () =
    C.self_init c ();
    begin if on_not then
      match negation with
        None -> failwith "Reify.reification.init: unreachable"
      | Some negc -> C.self_init negc () end;
    false in
*)
  C.create ~name ~fprint (*~init*) ~freevars update delay

let cstr ?(delay_on_negation = true) c b =
  reification c b delay_on_negation;;

let boolean ?delay_on_negation ?name c  =
  let b = Fd.create ?name Fcl_domain.boolean in
  let r = cstr ?delay_on_negation c b in
  Fcl_cstr.post r;
  b;;

exception MyDontKnow;;

let rec (||~~) c1 c2 =
  let update _ =
    C.is_solved c1 || C.is_solved c2 || 
    try
      if not (C.check c1 ()) then begin (* if c1 is false, c2 must be true *)
	Fcl_cstr.post c2
      end;
      true
    with
      Fcl_cstr.DontKnow -> 
	try
	  if not (C.check c2 ()) then (* if c2 is false, c1 must be true *)
	    Fcl_cstr.post c1;
	  true
	with
	  Fcl_cstr.DontKnow -> false

  and fprint s = Printf.fprintf s "("; C.fprint s c1; Printf.fprintf s ") ||~~ ("; C.fprint s c2; Printf.fprintf s ")"

  and delay c =
    C.self_delay c1 c;
    C.self_delay c2 c;
    C.self_delay (C.not c1) c;
    C.self_delay (C.not c2) c

  and check () =
    C.is_solved c1 || C.is_solved c2 ||
    try 
      (try C.check c1 () with Fcl_cstr.DontKnow -> raise MyDontKnow)
      || C.check c2 ()
    with
      MyDontKnow ->
	C.check c2 () || raise Fcl_cstr.DontKnow

  and not () = (&&~~) (C.not c1) (C.not c2)

  and freevars () =
    Fcl_domain.union (C.freevars c1) (C.freevars c2) in

  Fcl_cstr.create ~name:"||~~" ~fprint ~not ~check ~freevars update delay

and (&&~~) c1 c2 =
  let update _ =
    Fcl_cstr.post c1;
    Fcl_cstr.post c2;
    true

  and fprint s = Printf.fprintf s "("; C.fprint s c1; Printf.fprintf s ") &&~~ ("; C.fprint s c2; Printf.fprintf s ")"

  and delay c =
    C.self_delay c1 c;
    C.self_delay c2 c;
    C.self_delay (C.not c1) c;
    C.self_delay (C.not c2) c

  and check () =
    (C.is_solved c1 ||
      try C.check c1 () with
	Fcl_cstr.DontKnow ->
	  if C.check c2 () then raise Fcl_cstr.DontKnow else false )
      &&
    (C.is_solved c2 || C.check c2 ())

  and not () = (||~~) (C.not c1) (C.not c2)

  and freevars () =
    Fcl_domain.union (C.freevars c1) (C.freevars c2) in

  Fcl_cstr.create ~name:"&&~~" ~fprint ~not ~check ~freevars update delay;;

let (=>~~) c1 c2 = C.not c1 ||~~ c2

let rec eq_or_xor c1 c2 equiv = (* if [equiv] then (<=>~~) else (xor~~) *)
  let update _ =
    try
      Fcl_cstr.post (if C.check c1 () = equiv then c2 else C.not c2);
      true
    with
      Fcl_cstr.DontKnow -> (* c1 unknown *)
	try
	  Fcl_cstr.post (if C.check c2 () = equiv then c1 else C.not c1);
	  true
	with
	  Fcl_cstr.DontKnow -> (* c1 && c2 unknown *)
	    false

  and delay c =
    C.self_delay c1 c;
    C.self_delay c2 c;
    C.self_delay (C.not c1) c;
    C.self_delay (C.not c2) c
  and check i =
    (C.check c1 i = C.check c2 i) = equiv

  and not () =
    eq_or_xor c1 c2 (not equiv)

  in

  Fcl_cstr.create ~name:(if equiv then "<=>~~" else "xor~~") ~not ~check update delay;;


let (<=>~~) c1 c2 = eq_or_xor c1 c2 true

let xor c1 c2 = eq_or_xor c1 c2 false

let not c = C.not c
