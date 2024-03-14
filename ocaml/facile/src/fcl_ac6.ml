open Fcl_var

let _fprint_int_list c l =
  Printf.fprintf c "[";
  List.iter (fun x -> Printf.fprintf c "%d " x) l;
  Printf.fprintf c "]"

module HUnion = struct
  let create n = Hashtbl.create n
  let add h k =
    if not (Hashtbl.mem h k) then Hashtbl.add h k ()
  let to_list h =
    Hashtbl.fold (fun k () r -> k :: r) h []
end

let gen_union sets =
  let h = HUnion.create 17 in
  List.iter (fun set -> List.iter (HUnion.add h) set) sets;
  HUnion.to_list h

module Memo = struct
  let create n = Hashtbl.create n
  let add h k d =
    try
      let l = Hashtbl.find h k in
      l := d :: !l
    with Not_found -> Hashtbl.add h k (ref [d])
  let _find h k = !(Hashtbl.find h k)
  let fold f h init = Hashtbl.fold (fun k d -> f k !d) h init
end

let rec lswap l =
  match l with
    [] -> []
  | (x, y) :: xys -> (y, x) :: lswap xys

(* merge several (no)goods defined on the same variables *)
let merge_pairs pairs =
  let gather = Memo.create 17 in
  List.iter
    (fun (i, j, ng) ->
      if i = j then failwith "Ac6.merge_pairs: constraint on the same variable";
      if i < j then Memo.add gather (i, j) ng
      else (* store constraints with i < j only *)
	let sng = lswap ng in
	Memo.add gather (j, i) sng)
    pairs;
  let rpairs =
    Memo.fold
      (fun (i, j) ngs r ->
        match ngs with
          [] -> Fcl_debug.internal_error "Ac6.check_pairs"
        | [ng] -> (i, j, ng) :: r
        | _ ->
            (*Printf.printf "several pairs on the same variables (%d,%d)\n" i j;
            List.iter
              (fun couples ->
                List.iter (fun (k, l) -> Printf.printf "%d,%d|" k l) couples;
                Printf.printf "\n")
              ngs;*)
            let union_ngs = gen_union ngs in
            let sunion_ngs = List.sort compare union_ngs in
            (i, j, sunion_ngs) :: r)
      gather [] in
  let lrpairs = List.length rpairs in
  let lpairs = List.length pairs in
  if lrpairs < lpairs then begin
    (*Printf.printf "%d pairs reduced to %d\n" lpairs lrpairs;*)
    rpairs end
  else pairs

(* filters pairs where one side doesn't belong to the domain *)
let filter_pairs vars pairs =
  List.fold_right
    (fun (i, j, pairs) r ->
      let fpairs =
        List.fold_right
          (fun ((k, l) as kl) r ->
            if Fcl_domain.member vars.(i) k && Fcl_domain.member vars.(j) l
            then kl :: r
            else r)
          pairs [] in
      match fpairs with
        [] -> r
      | _ -> (i, j, fpairs) :: r)
    pairs []

let process_pairs vars pairs =
  let mpairs = merge_pairs pairs in
  filter_pairs vars mpairs

let one_dom_analysis v =
  let h = Memo.create 17 in
  List.iter (fun (vx, ng) -> Memo.add h ng vx) v;
  let interch = Memo.fold (fun _ idxs r -> List.rev idxs :: r) h [] in
  List.sort compare interch

let interchangeable doms pairs =
  let hdoms =
    Array.map
      (fun domi ->
        let hdom = Hashtbl.create 17 in
        Fcl_domain.iter (fun vx -> Hashtbl.add hdom vx (Memo.create 17)) domi;
        hdom)
      doms in
  let nb_ngs = ref 0
  and nb_couples = ref 0 in
  List.iter
    (fun (i, j, kls) ->
      let size_ngs = List.length kls in
      if size_ngs = 0 then failwith "Ac6.interchangeable: empty list" else
      nb_couples := !nb_couples + size_ngs;
      incr nb_ngs;
      List.iter
        (fun (k, l) ->
          let sigik = Hashtbl.find hdoms.(i) k in
          Memo.add sigik j l;
          let sigjl = Hashtbl.find hdoms.(j) l in
          Memo.add sigjl i k)
        kls)
    pairs;
 (* let size =
    List.fold_left
      (fun acc (i, j, _) ->
        Fcl_domain.size doms.(i) * Fcl_domain.size doms.(j) + acc)
      0 pairs in
  let mean_ratio = float !nb_couples /. float size in
  Printf.printf "mean tightness: %g\n" mean_ratio;*)
  let ldoms =
    Array.map
      (fun v ->
        let sigv =
          Hashtbl.fold
            (fun vx h r ->
              let ngs =
                Memo.fold (fun j ng r -> (j, ng) :: r) h [] in
              let sorted_ngs = List.sort compare ngs in
              (vx, sorted_ngs) :: r)
            v [] in
        List.sort compare sigv)
      hdoms in
  let interch = Array.map one_dom_analysis ldoms in
  (*Printf.printf "\ninterchangeable:\n";
  Array.iteri
    (fun i ki ->
      Printf.printf "%d:\n" i;
      List.iter
        (fun idxs ->
          if List.length idxs > 1 then begin
            Printf.printf "\t";
            fprint_int_list stdout idxs;
            Printf.printf "\n" end)
        ki)
    interch;
  Printf.printf "\n";*)
  interch

let reduce_doms interchange =
  Array.map
    (fun interchangei ->
      let domi =
        List.fold_right
          (fun interch r ->
            match interch with
              [] -> failwith "Ac6.reduce_doms: unreachable"
            | v :: _ -> v :: r)
          interchangei [] in
      Fcl_domain.create domi)
    interchange

type node = {
    mutable marked: bool;
    mutable adj: Fcl_domain.t
  }

let start_cc idx g =
  let rec start_cc_rec pending cc =
    if Fcl_domain.is_empty pending then cc
    else
      let idx = Fcl_domain.min pending in
      let pending = Fcl_domain.remove idx pending in
      if not g.(idx).marked then begin
        g.(idx).marked <- true;
        let pending = Fcl_domain.union g.(idx).adj pending in
        let cc = Fcl_domain.add idx cc in
        start_cc_rec pending cc end
      else start_cc_rec pending cc in
  start_cc_rec (Fcl_domain.create [idx]) Fcl_domain.empty

let find_cc g =
  let ccs = ref [] in
  for i = 0 to Array.length g - 1 do
    if not g.(i).marked then
      let cc = start_cc i g in
      ccs := cc :: !ccs done;
  !ccs

let cc n pairs =
  let g = Array.init n (fun _ -> {marked=false; adj=Fcl_domain.empty}) in
  List.iter
    (fun (i, j, _) ->
      g.(i).adj <- Fcl_domain.add j g.(i).adj;
      g.(j).adj <- Fcl_domain.add i g.(j).adj)
    pairs;
  find_cc g

let reduce doms pairs =
  let ppairs = process_pairs doms pairs in
  let interchange = interchangeable doms ppairs in
  let rdoms = reduce_doms interchange in
  let fppairs = filter_pairs rdoms ppairs in
  let n = Array.length doms in
  let ccs = cc n fppairs in
  (rdoms, fppairs, ccs)


let ac6 v1 v2 nogoods =
  let minv1 = Fd.min v1 in
  let intervalv1 = Fd.max v1 - minv1 + 1 in
  let val_of_index i = i + minv1
  and index_of_val v = v - minv1 in
  let nogoodsv1 =
    Array.init
      intervalv1
      (fun i ->
	List.fold_right
	  (fun (i1,j) acc -> if i1 = val_of_index i then j::acc else acc)
	  nogoods
	  []) in
  let nextsupport j i vi = (* search a new support i'>i in vi for value j *)
    let rec loop i1 =
      if i1 > Fd.max vi
      then raise Not_found
      else if Fd.member vi i1 && not (List.mem j nogoodsv1.(index_of_val(i1)))
      then i1
      else loop (i1+1) in
    loop (i+1) in
  let call_nextsupport j vi s =
    if Fd.member v2 j then
      try
      	let i = index_of_val (nextsupport j vi v1) in
	Fcl_data.Array.set s i (j::s.(i))
      with
      	Not_found ->
	  match Fd.value v2 with
	    Unk _v2_ -> Fd.remove v2 j
	  | Val vj -> assert (vj = j); Fcl_stak.fail "ac6" in
  let s =
    let s = Array.make intervalv1 [] in
    Fd.iter (fun j -> call_nextsupport j (minv1-1) s) v2;
    s in
  let last_domain = Fcl_stak.ref (Fd.dom v1) in

  let name = "ac6" in
  let delay x =
    Fd.delay [Fd.on_refine] v1 x;
    Fd.delay [Fd.on_refine] v2 x

  and update _ =
    let new_domain = Fd.dom v1 in
    let removed_values =
      Fcl_domain.difference (Fcl_stak.get last_domain) new_domain in
    Fcl_stak.set last_domain new_domain;
    Fcl_domain.iter
      (fun i ->
	let si = s.(index_of_val i) in
	List.iter (fun j -> call_nextsupport j i s) si
          (***; s.(index_of_val i) n'est plus utile. Faut-t-il le virer ???
	     Fcl_stak.trail (fun () -> s.(index_of_val i) <- si);
	     s.(index_of_val i) <- []***))
      removed_values;
    Fd.is_bound v1

  and freevars () = Fd.vars2ids [v1; v2] in

  Fcl_cstr.create ~name ~freevars update delay

let cstr var1 var2 ng =
  let c1 = ac6 var1 var2 ng
  and c2 = ac6 var2 var1 (List.map (fun (x,y)-> (y,x)) ng) in
  Fcl_reify.(&&~~) c1 c2

let _fprint_intlist_array c t =
  Array.iteri
    (fun i l ->
      Printf.fprintf c "\t%d: " i;
      List.iter (fun j -> Printf.fprintf c "%d " j) l;
      Printf.fprintf c "\n")
    t

module HSet = struct
  type 'a t = ('a, unit) Hashtbl.t
  let create n = Hashtbl.create n
  let add h k = Hashtbl.add h k ()
  let _addmem h k = if not (Hashtbl.mem h k) then add h k
  let mem = Hashtbl.mem
  let _iter f = Hashtbl.iter (fun k _ -> f k)
  let _fold f = Hashtbl.fold (fun k _ -> f k)
  let _length = Hashtbl.length
end

let id_or_val v =
  if Fd.is_var v then Printf.sprintf "var%d" (Fd.id v)
  else Printf.sprintf "val%d" (Fd.elt_value v)

(* Hashtbl instead of List.mem to search for new supports *)
let ac6_3 v1 v2 nogoods =
  let name = Printf.sprintf "ac6_3(%s,%s)" (id_or_val v1) (id_or_val v2) in
  let minv1 = Fd.min v1 in
  let intervalv1 = Fd.max v1 - minv1 + 1 in
  let val2idx v = v - minv1 in
  let nogoodsv1 = Array.init intervalv1 (fun _ -> HSet.create 7) in
  let init_nogoodsv1 () =
    List.iter
      (fun (i1, j) ->
        let idx = val2idx i1 in
        if 0 <= idx && idx < intervalv1 then HSet.add nogoodsv1.(idx) j)
      nogoods in

  let s = Array.make intervalv1 [] in
  let nextsupport j i =
    let upper = Fcl_domain.remove_low (i + 1) (Fd.dom v1) in
    Fcl_domain.arg_exists_opt
      (fun i1 -> not (HSet.mem nogoodsv1.(val2idx i1) j))
      upper in
  let call_nextsupport j vi =
    if Fd.member v2 j then
      match nextsupport j vi with
        None -> Fd.remove v2 j
      | Some newvi ->
      	  let i = val2idx newvi in
	  Fcl_data.Array.set s i (j :: s.(i)) in

  let init_s () =
    Fd.iter (fun j -> call_nextsupport j (minv1 - 1)) v2 in

  let last_domain = Fcl_stak.ref (Fd.dom v1) in

  let delay x =
    Fd.delay [Fd.on_refine] v1 x;
    Fd.delay [Fd.on_refine] v2 x

  and init () =
    init_nogoodsv1 ();
    init_s ();
    Fd.is_bound v1 in

  (* not better with this specialization *)
  (*
  let update_subst i1 =
    let ngval1 =
      HSet.fold (fun i2 r -> i2 :: r) nogoodsv1.(val2idx i1) [] in
    Fd.refine v2 (Fcl_domain.diff (Fd.dom v2) (Fcl_domain.create ngval1)) in
   *)
  let update _ =
    (*match Fd.value v1 with
      Val i1 -> update_subst i1; true
    | Unk attr ->
        let new_domain = Fd.dom attr in*)
        let new_domain = Fd.dom v1 in
        let removed_values =
          Fcl_domain.difference (Fcl_stak.get last_domain) new_domain in
        Fcl_stak.set last_domain new_domain;
        Fcl_domain.iter
          (fun i ->
	    let si = s.(val2idx i) in
	    List.iter (fun j -> call_nextsupport j i) si)
          removed_values;
        (*false*)
        Fd.is_bound v1 in

  let freevars () = Fd.vars2ids [v1; v2] in

  Fcl_cstr.create ~name ~freevars ~init update delay

let cstr3 var1 var2 ng =
  let c1 = ac6_3 var1 var2 ng
  and c2 = ac6_3 var2 var1 (List.map (fun (x,y)-> (y,x)) ng) in
  Fcl_reify.(&&~~) c1 c2

(* xor *)
let (+||) a b = if a then not b else b

(* works with goods and nogoods so the version is dynamically
   chosen (module Binary) to use the smallest set *)
(* if forbidden then nogoods else goods *)
let ac6_4 ?(forbidden = true) v1 v2 pairs =
  let name = Printf.sprintf "ac6_4(%s,%s-%b)"
      (id_or_val v1) (id_or_val v2) forbidden in
  let minv1 = Fd.min v1 in
  let intervalv1 = Fd.max v1 - minv1 + 1 in
  let val2idx v = v - minv1 in
  let pairsv1 = Array.init intervalv1 (fun _ -> HSet.create 7) in
  let init_pairsv1 () =
    List.iter
      (fun (i1, j) ->
        let idx = val2idx i1 in
        if 0 <= idx && idx < intervalv1 then HSet.add pairsv1.(idx) j)
      pairs in

  let s = Array.make intervalv1 [] in
  let nextsupport j i =
    let upper = Fcl_domain.remove_low (i + 1) (Fd.dom v1) in
    Fcl_domain.arg_exists_opt
      (fun i1 -> forbidden +|| HSet.mem pairsv1.(val2idx i1) j)
      upper in
  let call_nextsupport j vi =
    if Fd.member v2 j then
      match nextsupport j vi with
        None -> Fd.remove v2 j
      | Some newvi ->
      	  let i = val2idx newvi in
	  Fcl_data.Array.set s i (j :: s.(i)) in

  let init_s () =
    Fd.iter (fun j -> call_nextsupport j (minv1 - 1)) v2 in

  let last_domain = Fcl_stak.ref (Fd.dom v1) in

  let delay x =
    Fd.delay [Fd.on_refine] v1 x;
    Fd.delay [Fd.on_refine] v2 x

  and init () =
    init_pairsv1 ();
    init_s ();
    Fd.is_bound v1 in

  let update _ =
    let new_domain = Fd.dom v1 in
    let removed_values =
      Fcl_domain.difference (Fcl_stak.get last_domain) new_domain in
    Fcl_stak.set last_domain new_domain;
    Fcl_domain.iter
      (fun i ->
	let si = s.(val2idx i) in
	List.iter (fun j -> call_nextsupport j i) si)
      removed_values;
    Fd.is_bound v1 in

  let freevars () = Fd.vars2ids [v1; v2] in

  Fcl_cstr.create ~name ~freevars ~init update delay

let cstr4 ?(forbidden=true) v1 v2 pairs =
  let c1 = ac6_4 ~forbidden v1 v2 pairs
  and c2 = ac6_4 ~forbidden v2 v1 (List.map (fun (x,y)-> (y,x)) pairs) in
  Fcl_reify.(&&~~) c1 c2

type ac = {
    v: Fd.t;
    last: Fcl_domain.t Fcl_stak.ref;
    adj: int HSet.t array;
    sup: int list array;
    val2idx: int -> int
  }

(* only one constraint *)
let cstr5 v1 v2 goods =
  let name = Printf.sprintf "ac6_5(%s,%s)" (id_or_val v1) (id_or_val v2) in
  let create_ac v =
    let minv = Fd.min v in
    let range = Fd.max v - minv + 1 in
    {v;
     last = Fcl_stak.ref Fcl_domain.empty;
     adj = Array.init range (fun _ -> HSet.create 7);
     sup = Array.make range [];
     val2idx = fun a -> a - minv} in
  let ac1 = create_ac v1
  and ac2 = create_ac v2 in

  let call_nextsupport acl acr j vi =
    if Fd.member acr.v j then
      let upper = Fcl_domain.remove_low (vi + 1) (Fd.dom acl.v) in
      let next =
        Fcl_domain.arg_exists_opt
          (fun i1 -> HSet.mem acl.adj.(acl.val2idx i1) j)
          upper in
      match next with
        None -> Fd.remove acr.v j
      | Some newvi ->
      	  let i = acl.val2idx newvi in
	  Fcl_data.Array.set acl.sup i (j :: acl.sup.(i)) in

  let update_ac acl acr =
    let new_domain = Fd.dom acl.v in
    let removed_values =
      Fcl_domain.difference (Fcl_stak.get acl.last) new_domain in
    Fcl_stak.set acl.last new_domain;
    Fcl_domain.iter
      (fun i ->
	let si = acl.sup.(acl.val2idx i) in
	List.iter (fun j -> call_nextsupport acl acr j i) si)
      removed_values;
    Fd.is_bound acl.v in

  let update id =
    if id = 0 then update_ac ac1 ac2 else update_ac ac2 ac1 in

  let init_adj () =
    List.iter
      (fun (i, j) ->
        if Fd.member v1 i && Fd.member v2 j then begin
          let idx1 = ac1.val2idx i in
          HSet.add ac1.adj.(idx1) j;
          let idx2 = ac2.val2idx j in
          HSet.add ac2.adj.(idx2) i end)
      goods in

  let init_supp acl acr =
    Fd.iter (fun j -> call_nextsupport acl acr j (Fd.min acl.v - 1)) acr.v in

  let init () =
    init_adj ();
    init_supp ac1 ac2;
    init_supp ac2 ac1;
    Fcl_stak.set ac1.last (Fd.dom v1);
    Fcl_stak.set ac2.last (Fd.dom v2);
    (* OR seems enough (AND?) *)
    Fd.is_bound v1 || Fd.is_bound v2 in

  let delay ctr =
    Fd.delay [Fd.on_refine] v1 ~waking_id:0 ctr;
    Fd.delay [Fd.on_refine] v2 ~waking_id:1 ctr in

  let freevars () = Fd.vars2ids [v1; v2] in

  Fcl_cstr.create ~name ~freevars ~init ~nb_wakings:2 update delay

type ac66 = {
    v6: Fd.t;
    last6: Fcl_domain.t Fcl_stak.ref;
    adj6: Fcl_domain.t array;
    sup6: int list array;
    val2idx6: int -> int
  }

let _fprint_ac6 c ac6 =
  Printf.fprintf c "\tv: %a\n\tlast: %a\n\tadj: "
    Fd.fprint ac6.v6 Fcl_domain.fprint (Fcl_stak.get ac6.last6);
  Array.iteri
    (fun i adj6i ->
      Printf.fprintf c "%d:%a|" i Fcl_domain.fprint adj6i)
    ac6.adj6;
  Printf.fprintf c "\n\tsup: ";
  Array.iteri
    (fun i sup6i ->
      Printf.fprintf c "%d:" i;
      List.iter (fun x -> Printf.fprintf c "%d " x) sup6i;
      Printf.fprintf c "-")
    ac6.sup6

(* only one constraint with improved nextsupport using
   lazy intersection check of values and goods domains
   for nextsupport instead of hashtables (possible because
   we have only one constraint and have therefore access to
   goods sets of both variables *)
let cstr6 v1 v2 goods =
  let name = Printf.sprintf "ac6_6(%s,%s)" (id_or_val v1) (id_or_val v2) in
  let create_ac v =
    let minv = Fd.min v in
    let range = Fd.max v - minv + 1 in
    {v6 = v;
     last6 = Fcl_stak.ref Fcl_domain.empty;
     adj6 = Array.make range Fcl_domain.empty;
     sup6 = Array.make range [];
     val2idx6 = fun a -> a - minv} in

  (* initialize later with init to avoid uselessly low minv? *)
  let ac1 = create_ac v1
  and ac2 = create_ac v2 in

  let call_nextsupport acl acr j vi =
    if Fd.member acr.v6 j then
      let next =
        Fcl_domain.smallest_common_elt_greater_than
          (Fd.dom acl.v6) acr.adj6.(acr.val2idx6 j) vi in
      match next with
        None -> Fd.remove acr.v6 j
      | Some newvi ->
      	  let i = acl.val2idx6 newvi in
	  Fcl_data.Array.set acl.sup6 i (j :: acl.sup6.(i)) in

  let update_ac acl acr =
    let new_domain = Fd.dom acl.v6 in
    let removed_values =
      Fcl_domain.difference (Fcl_stak.get acl.last6) new_domain in
    Fcl_stak.set acl.last6 new_domain;
    Fcl_domain.iter
      (fun i ->
	let si = acl.sup6.(acl.val2idx6 i) in
	List.iter (fun j -> call_nextsupport acl acr j i) si)
      removed_values;
    Fd.is_bound acl.v6 in

  let update id =
    if id = 0 then update_ac ac1 ac2 else update_ac ac2 ac1 in

  let init_adj () =
    List.iter
      (fun (i, j) ->
        if Fd.member v1 i && Fd.member v2 j then begin
          let idx1 = ac1.val2idx6 i in
          ac1.adj6.(idx1) <- Fcl_domain.add j ac1.adj6.(idx1);
          let idx2 = ac2.val2idx6 j in
          ac2.adj6.(idx2) <- Fcl_domain.add i ac2.adj6.(idx2) end)
      goods in

  let init_supp acl acr =
    Fd.iter (fun j -> call_nextsupport acl acr j (Fd.min acl.v6 - 1)) acr.v6 in

  let init () =
    init_adj ();
    init_supp ac1 ac2;
    init_supp ac2 ac1;
    Fcl_stak.set ac1.last6 (Fd.dom v1);
    Fcl_stak.set ac2.last6 (Fd.dom v2);
    (* OR seems enough (AND?) *)
    Fd.is_bound v1 || Fd.is_bound v2 in

  let delay ctr =
    Fd.delay [Fd.on_refine] v1 ~waking_id:0 ctr;
    Fd.delay [Fd.on_refine] v2 ~waking_id:1 ctr in

  let freevars () = Fd.vars2ids [v1; v2] in

  Fcl_cstr.create ~name ~freevars ~init ~nb_wakings:2 update delay

let compl var1 var2 sub =
  let h = Hashtbl.create 17 in
  let d1 = Fd.values var1 and d2 = Fd.values var2 in
  List.iter
    (fun e1 ->
      List.iter
	(fun e2 ->
	  Hashtbl.add h (e1, e2) ())
        d2)
    d1;
  List.iter (fun e1e2 -> Hashtbl.remove h e1e2) sub;
  let csub = Hashtbl.fold (fun k _ r -> k :: r) h [] in
  List.sort compare csub

type acs = {
    vs: Fd.t;
    lasts: Fcl_domain.t Fcl_stak.ref;
    adjs: Fcl_domain.t array;
    sups: int list array;
    val2idxs: int -> int;
    nbgoods: int Fcl_stak.ref array;
    ndg: int Fcl_stak.ref
  }

let _fprint_acs c acs =
  Printf.fprintf c "\tvs: %a\n\tlasts: %a\n\tadjs: "
    Fd.fprint acs.vs Fcl_domain.fprint (Fcl_stak.get acs.lasts);
  Array.iteri
    (fun i adjsi ->
      Printf.fprintf c "%d:%a|" i Fcl_domain.fprint adjsi)
    acs.adjs;
  Printf.fprintf c "\n\tsups: ";
  Array.iteri
    (fun i supsi ->
      Printf.fprintf c "%d:" i;
      List.iter (fun x -> Printf.fprintf c "%d " x) supsi;
      Printf.fprintf c "-")
    acs.sups;
  Printf.fprintf c "\n\tnbgoods: ";
  Array.iteri
    (fun i nbgi -> Printf.fprintf c "%d:%d " i (Fcl_stak.get nbgi))
    acs.nbgoods;
  Printf.fprintf c "\n\tndg:%d\n" (Fcl_stak.get acs.ndg)

(* binary constraint is necessary violated iff the number of
   dynamic goods ndg = 0 and necessary satisfied iff ndg = nd1 * nd2 *)
let rec cstr_soft v1 v2 goods =
  let name = Printf.sprintf "ac6_s(%s,%s)" (id_or_val v1) (id_or_val v2) in
  let create_ac v =
    let minv = Fd.min v in
    let range = Fd.max v - minv + 1 in
    let val2idxs a = a - minv in
    let nbgoods = Array.init range (fun _ -> Fcl_stak.ref  0) in
    {vs = v;
     lasts = Fcl_stak.ref Fcl_domain.empty;
     adjs = Array.make range Fcl_domain.empty;
     sups = Array.make range [];
     val2idxs;
     nbgoods;
     ndg = Fcl_stak.ref 0} in

  (* initialize later with init to avoid uselessly low minv? *)
  let ac1 = create_ac v1
  and ac2 = create_ac v2 in

  (* initialiazation for reification and check *)
  let check_init () =
    let nbg = ref 0 in
    List.iter
      (fun (i, j) ->
        if Fd.member v1 i && Fd.member v2 j then begin
          incr nbg;
          let idxi = ac1.val2idxs i in
          Fcl_stak.incr ac1.nbgoods.(idxi);
          let idxj = ac2.val2idxs j in
          Fcl_stak.incr ac2.nbgoods.(idxj) end)
      goods;
    Fcl_stak.set ac1.ndg !nbg;
    Fcl_stak.set ac2.ndg !nbg;
    Fcl_stak.set ac1.lasts (Fd.dom v1);
    Fcl_stak.set ac2.lasts (Fd.dom v2) in
  check_init ();

  let call_nextsupport acl acr j vi =
    if Fd.member acr.vs j then
      let next =
        Fcl_domain.smallest_common_elt_greater_than
          (Fd.dom acl.vs) acr.adjs.(acr.val2idxs j) vi in
      match next with
        None -> Fd.remove acr.vs j
      | Some newvi ->
      	  let i = acl.val2idxs newvi in
	  Fcl_data.Array.set acl.sups i (j :: acl.sups.(i)) in

  let update_ac acl acr =
    let new_domain = Fd.dom acl.vs in
    let removed_values =
      Fcl_domain.difference (Fcl_stak.get acl.lasts) new_domain in
    Fcl_stak.set acl.lasts new_domain;
    Fcl_domain.iter
      (fun i ->
	let si = acl.sups.(acl.val2idxs i) in
	List.iter (fun j -> call_nextsupport acl acr j i) si)
      removed_values;
    Fd.is_bound acl.vs in

  let update id =
    if id = 0 then update_ac ac1 ac2 else update_ac ac2 ac1 in

  let init_adj () =
    List.iter
      (fun (i, j) ->
        if Fd.member v1 i && Fd.member v2 j then begin
          let idx1 = ac1.val2idxs i in
          ac1.adjs.(idx1) <- Fcl_domain.add j ac1.adjs.(idx1);
          let idx2 = ac2.val2idxs j in
          ac2.adjs.(idx2) <- Fcl_domain.add i ac2.adjs.(idx2) end)
      goods in

  let init_supp acl acr =
    Fd.iter (fun j -> call_nextsupport acl acr j (Fd.min acl.vs - 1)) acr.vs in

  let init () =
    init_adj ();
    init_supp ac1 ac2;
    init_supp ac2 ac1;
    Fcl_stak.set ac1.lasts (Fd.dom v1);
    Fcl_stak.set ac2.lasts (Fd.dom v2);
    (* OR seems enough (AND?) *)
    Fd.is_bound v1 || Fd.is_bound v2 in

  let check_ac ac removed_values =
    let nb_removed_arcs = ref 0 in
    Fcl_domain.iter
      (fun i ->
        nb_removed_arcs :=
          !nb_removed_arcs + Fcl_stak.get ac.nbgoods.(ac.val2idxs i))
      removed_values;
    let new_ndg = Fcl_stak.get ac.ndg - !nb_removed_arcs in
    Printf.printf "new_ndg:%d" new_ndg;
    if new_ndg = 0 then false
    else begin
      let sizeprod = Fd.size v1 * Fd.size v2 in
      (*Printf.printf " sizeprod:%d" sizeprod;*)
      if new_ndg = sizeprod then true
      else begin
        Fcl_stak.set ac.ndg new_ndg;
        raise Fcl_cstr.DontKnow end end in

  let check () =
    let new_domain1 = Fd.dom ac1.vs in
    let removed_values1 =
      Fcl_domain.difference (Fcl_stak.get ac1.lasts) new_domain1 in
    let new_domain2 = Fd.dom ac2.vs in
    let removed_values2 =
      Fcl_domain.difference (Fcl_stak.get ac2.lasts) new_domain2 in
    match Fcl_domain.is_empty removed_values1, Fcl_domain.is_empty removed_values2 with
      true, true -> (* initial update of reification *)
        let ndg1 = Fcl_stak.get ac1.ndg in
        assert (ndg1 = Fcl_stak.get ac2.ndg);
        if ndg1 = 0 then false else
        let sizeprod = Fd.size v1 * Fd.size v2 in
        if ndg1 = sizeprod  then true else raise Fcl_cstr.DontKnow
    | false, true ->
        Fcl_stak.set ac1.lasts new_domain1;
        check_ac ac1 removed_values1
    | true, false ->
        Fcl_stak.set ac2.lasts new_domain2;
        check_ac ac2 removed_values2
    | false, false ->
        Fcl_stak.set ac1.lasts new_domain1;
        try
          check_ac ac1 removed_values1
        with Fcl_cstr.DontKnow ->
          Fcl_stak.set ac2.lasts new_domain2;
          check_ac ac2 removed_values2 in

  let not () =
    let nogoods = compl v1 v2 goods in
    cstr_soft v1 v2 nogoods in

  let delay ctr =
    Fd.delay [Fd.on_refine] v1 ~waking_id:0 ctr;
    Fd.delay [Fd.on_refine] v2 ~waking_id:1 ctr in

  let freevars () = Fd.vars2ids [v1; v2] in

  Fcl_cstr.create ~name ~freevars ~init ~nb_wakings:2 ~check ~not update delay

module GoodSet = struct
  let create n = Hashtbl.create n
  let add h k = Hashtbl.add h k ()
  let remove h k =
    Hashtbl.remove h k;
    Fcl_stak.trail (fun () -> Hashtbl.add h k ())
  let iter f h = Hashtbl.iter (fun k () -> f k) h
  let size h = Hashtbl.length h
  let tolist h = Hashtbl.fold (fun k () r -> k :: r) h []
end

(* binary constraint is necessary violated iff the number of
   dynamic goods ndg = 0 and necessary satisfied iff ndg = nd1 * nd2 *)
(* version that doesn't use waking ids *)
let rec cstr_soft_noid v1 v2 goods =
  let name = Printf.sprintf "ac6_snoid(%s,%s)" (id_or_val v1) (id_or_val v2) in
  let goods =
    let set = GoodSet.create 17 in
    List.iter (fun ij -> GoodSet.add set ij) goods;
    set in

  (*let fprint_goods c =
    Printf.fprintf c "goods(%d):" (GoodSet.size goods);
    GoodSet.iter (fun (i, j) -> Printf.fprintf c "%d,%d|" i j) goods in*)

  let create_ac v =
    let minv = Fd.min v in
    let range = Fd.max v - minv + 1 in
    let val2idx6 a = a - minv in
    {v6 = v;
     last6 = Fcl_stak.ref Fcl_domain.empty;
     adj6 = Array.make range Fcl_domain.empty;
     sup6 = Array.make range [];
     val2idx6} in

  (* initialize later with init to avoid uselessly low minv? *)
  let ac1 = create_ac v1
  and ac2 = create_ac v2 in

  (* initialiazation for reification and check *)
  let check_init () =
    Fcl_stak.set ac1.last6 (Fd.dom v1);
    Fcl_stak.set ac2.last6 (Fd.dom v2) in
  check_init ();

  let call_nextsupport acl acr j vi =
    if Fd.member acr.v6 j then
      let next =
        Fcl_domain.smallest_common_elt_greater_than
          (Fd.dom acl.v6) acr.adj6.(acr.val2idx6 j) vi in
      match next with
        None -> Fd.remove acr.v6 j
      | Some newvi ->
      	  let i = acl.val2idx6 newvi in
	  Fcl_data.Array.set acl.sup6 i (j :: acl.sup6.(i)) in

  let update_ac acl acr =
    let new_domain = Fd.dom acl.v6 in
    let removed_values =
      Fcl_domain.difference (Fcl_stak.get acl.last6) new_domain in
    Fcl_stak.set acl.last6 new_domain;
    Fcl_domain.iter
      (fun i ->
	let si = acl.sup6.(acl.val2idx6 i) in
	List.iter (fun j -> call_nextsupport acl acr j i) si)
      removed_values;
    Fd.is_bound acl.v6 in

  let update _ =
    let old_dom_ac1 = Fcl_stak.get ac1.last6 in
    let new_dom_ac1 = Fd.dom ac1.v6 in
    let res12 = ref false in
    if Fcl_domain.size new_dom_ac1 < Fcl_domain.size old_dom_ac1 then
      res12 := update_ac ac1 ac2;
    let old_dom_ac2 = Fcl_stak.get ac2.last6 in
    let new_dom_ac2 = Fd.dom ac2.v6 in
    let res21 = ref false in
    if Fcl_domain.size new_dom_ac2 < Fcl_domain.size old_dom_ac2 then
      res21 := update_ac ac2 ac1;
    !res12 || !res21 in

  let init_adj () =
    GoodSet.iter
      (fun (i, j) ->
        if Fd.member v1 i && Fd.member v2 j then begin
          let idx1 = ac1.val2idx6 i in
          ac1.adj6.(idx1) <- Fcl_domain.add j ac1.adj6.(idx1);
          let idx2 = ac2.val2idx6 j in
          ac2.adj6.(idx2) <- Fcl_domain.add i ac2.adj6.(idx2) end)
      goods in
  init_adj ();

  let init_supp acl acr =
    Fd.iter (fun j -> call_nextsupport acl acr j (Fd.min acl.v6 - 1)) acr.v6 in

  let init () =
    (*Printf.printf "init %s\n" name;*)
    init_supp ac1 ac2;
    init_supp ac2 ac1;
    (*Printf.printf "init:\nac1:%a\nac2:%a\n" fprint_ac6 ac1 fprint_ac6 ac2;*)
    (* OR seems enough (AND?) *)
    Fd.is_bound v1 || Fd.is_bound v2 in

  let check_ac left removed_values =
    (*Printf.printf "check_ac(left:%b)\n" left;*)
    let acl = if left then ac1 else ac2 in
    Fcl_domain.iter
      (fun i ->
        Fcl_domain.iter
          (fun j ->
            let ij = if left then (i, j) else (j, i) in
            GoodSet.remove goods ij)
          acl.adj6.(acl.val2idx6 i))
      removed_values;
    let ndg = GoodSet.size goods in
    (*Printf.printf "size goods:%d" ndg;*)
    if ndg = 0 then begin (*Printf.printf "\n\tCHECK FALSE!\n";*) false end
    else begin
      let sizeprod = Fd.size v1 * Fd.size v2 in
      (*Printf.printf " sizeprod:%d" sizeprod;*)
      if ndg = sizeprod then begin (*Printf.printf "\n\tCHECK TRUE!\n";*) true end
      else raise Fcl_cstr.DontKnow end in

  let check () =
    (*Printf.printf "check:\nac1:%a\nac2:%a\n" fprint_ac6 ac1 fprint_ac6 ac2;*)
    (*fprint_goods stdout; Printf.printf "\n";*)
    let old_dom_ac1 = Fcl_stak.get ac1.last6 in
    let new_dom_ac1 = Fd.dom ac1.v6 in
    let old_dom_ac2 = Fcl_stak.get ac2.last6 in
    let new_dom_ac2 = Fd.dom ac2.v6 in
    let test_size1 =
      Fcl_domain.size new_dom_ac1 < Fcl_domain.size old_dom_ac1 in
    let test_size2 =
      Fcl_domain.size new_dom_ac2 < Fcl_domain.size old_dom_ac2 in
    match test_size1, test_size2 with
      false, false -> (* initial update of reification *)
        let ndg = GoodSet.size goods in
        if ndg = 0 then false else
        let sizeprod = Fd.size v1 * Fd.size v2 in
        if ndg = sizeprod  then true else raise Fcl_cstr.DontKnow
    | true, false ->
        let removed_values1 = Fcl_domain.difference old_dom_ac1 new_dom_ac1 in
        Fcl_stak.set ac1.last6 new_dom_ac1;
        check_ac true removed_values1
    | false, true ->
        let removed_values2 = Fcl_domain.difference old_dom_ac2 new_dom_ac2 in
        Fcl_stak.set ac2.last6 new_dom_ac2;
        check_ac false removed_values2
    | true, true ->
        let removed_values1 = Fcl_domain.difference old_dom_ac1 new_dom_ac1 in
        Fcl_stak.set ac1.last6 new_dom_ac1;
        try
          check_ac true removed_values1
        with Fcl_cstr.DontKnow ->
          let removed_values2 =
            Fcl_domain.difference old_dom_ac2 new_dom_ac2 in
          Fcl_stak.set ac2.last6 new_dom_ac2;
          check_ac false removed_values2 in

  let not () =
    if GoodSet.size goods = 0 then Fcl_cstr.one else
    let nogoods = compl v1 v2 (GoodSet.tolist goods) in
    cstr_soft_noid v1 v2 nogoods in

  let delay ctr =
    Fd.delay [Fd.on_refine] v1 ctr;
    Fd.delay [Fd.on_refine] v2 ctr in

  let freevars () = Fd.vars2ids [v1; v2] in

  let tightness () =
    1. -. float (GoodSet.size goods) /. float (Fd.size v1 * Fd.size v2) in

  Fcl_cstr.create ~name ~freevars ~init ~check ~tightness ~not update delay

(* maintaining tightness *)
let cstr_tight v1 v2 lgoods =
  let name = Printf.sprintf "ac6_6(%s,%s)" (id_or_val v1) (id_or_val v2) in
  let tightness = Fcl_stak.ref 0. in
  let goods = GoodSet.create 17 in
  let init_goods () =
    List.iter
      (fun ((i, j) as ij) ->
	if Fd.member v1 i && Fd.member v2 j then GoodSet.add goods ij)
      lgoods in

  let create_ac v =
    let minv = Fd.min v in
    let range = Fd.max v - minv + 1 in
    {v6 = v;
     last6 = Fcl_stak.ref Fcl_domain.empty;
     adj6 = Array.make range Fcl_domain.empty;
     sup6 = Array.make range [];
     val2idx6 = fun a -> a - minv} in

  (* initialize later with init to avoid uselessly low minv? *)
  let ac1 = create_ac v1
  and ac2 = create_ac v2 in

  let update_tightness () =
    let tight =
      1. -. float (GoodSet.size goods) /. float (Fd.size v1 * Fd.size v2) in
    Fcl_stak.set tightness tight in

  let update_goods left removed_values =
    let acl = if left then ac1 else ac2 in
    Fcl_domain.iter
      (fun i ->
        Fcl_domain.iter
          (fun j ->
            let ij = if left then (i, j) else (j, i) in
            GoodSet.remove goods ij)
          acl.adj6.(acl.val2idx6 i))
      removed_values in

  let call_nextsupport acl acr j vi =
    if Fd.member acr.v6 j then
      let next =
        Fcl_domain.smallest_common_elt_greater_than
          (Fd.dom acl.v6) acr.adj6.(acr.val2idx6 j) vi in
      match next with
        None -> Fd.remove acr.v6 j
      | Some newvi ->
      	  let i = acl.val2idx6 newvi in
	  Fcl_data.Array.set acl.sup6 i (j :: acl.sup6.(i)) in

  let update_ac acl acr left =
    let new_domain = Fd.dom acl.v6 in
    let removed_values =
      Fcl_domain.difference (Fcl_stak.get acl.last6) new_domain in
    Fcl_stak.set acl.last6 new_domain;
    Fcl_domain.iter
      (fun i ->
	let si = acl.sup6.(acl.val2idx6 i) in
	List.iter (fun j -> call_nextsupport acl acr j i) si)
      removed_values;
    update_goods left removed_values;
    update_tightness ();
    Fd.is_bound acl.v6 in

  let update id =
    if id = 0 then update_ac ac1 ac2 true else update_ac ac2 ac1 false in

  let init_adj () =
    List.iter
      (fun (i, j) ->
	if Fd.member v1 i && Fd.member v2 j then begin
          let idx1 = ac1.val2idx6 i in
          ac1.adj6.(idx1) <- Fcl_domain.add j ac1.adj6.(idx1);
          let idx2 = ac2.val2idx6 j in
          ac2.adj6.(idx2) <- Fcl_domain.add i ac2.adj6.(idx2) end)
      lgoods in

  let init_supp acl acr =
    Fd.iter (fun j -> call_nextsupport acl acr j (Fd.min acl.v6 - 1)) acr.v6 in

  let init () =
    init_adj ();
    init_supp ac1 ac2;
    init_supp ac2 ac1;
    Fcl_stak.set ac1.last6 (Fd.dom v1);
    Fcl_stak.set ac2.last6 (Fd.dom v2);
    init_goods ();
    update_tightness ();
    (* OR seems enough (AND?) *)
    Fd.is_bound v1 || Fd.is_bound v2 in

  let delay ctr =
    Fd.delay [Fd.on_refine] v1 ~waking_id:0 ctr;
    Fd.delay [Fd.on_refine] v2 ~waking_id:1 ctr in

  let freevars () = Fd.vars2ids [v1; v2] in

  let tightness () = Fcl_stak.get tightness in

  Fcl_cstr.create ~name ~freevars ~init ~tightness ~nb_wakings:2 update delay
