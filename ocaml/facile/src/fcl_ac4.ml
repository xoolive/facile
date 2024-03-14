open Fcl_var
module D = Fcl_data

module Memo = D.MakeMemoize(Fcl_domain)

(*
module BTHInt = struct
  let create n = Hashtbl.create n

  let incr h k =
    try
      incr (Hashtbl.find h k)
    with Not_found -> Hashtbl.add h k (ref 1)

  let bt_incr h k =
    try
      let r = Hashtbl.find h k in
      incr r;
      Fcl_stak.trail (fun () -> decr r)
    with Not_found ->
      Hashtbl.add h k (ref 1);
      Fcl_stak.trail (fun () -> Hashtbl.remove h k)

  let decr h k = decr (Hashtbl.find h k)

  let bt_decr h k =
    let r = Hashtbl.find h k in
    decr r;
    Fcl_stak.trail (fun () -> incr r)
end
 *)

let hdom z =
  let remove = Hashtbl.create 17 in
  List.iter (fun vz -> Hashtbl.add remove vz false) (Fd.values z);
  remove

let hrefine x h =
  let dom =
    Hashtbl.fold
      (fun vx keep acc -> if keep then Fcl_domain.add vx acc else acc)
      h Fcl_domain.empty in
  Fd.refine x dom

let dom x =
  match Fd.value x with
    Val vx -> Fcl_domain.create [vx]
  | Unk attr -> Attr.dom attr

let fprint_memo c h =
  let l = Memo.fold (fun k set acc -> (k, set)::acc) h [] in
  List.iter
    (fun (k, set) ->
      Printf.fprintf c "(%d:%a)" k Fcl_domain.fprint set)
    (List.sort compare l)

let var_id x =
  if Fd.is_bound x then Printf.sprintf "Val(%d)" (Fd.elt_value x)
  else Printf.sprintf "Unk(%d)" (Fd.id x)

let ac4 x y goods =
  let name = Printf.sprintf "AC4(%s,%s)" (var_id x) (var_id y) in
  let xgoods = Memo.create 17 and ygoods = Memo.create 17 in
  let xlast = Fcl_stak.ref Fcl_domain.empty
  and ylast = Fcl_stak.ref Fcl_domain.empty in

  let nb_goods = Fcl_stak.ref 0 in
  let tightness = Fcl_stak.ref 0. in

  let check_goods () =
    let nbxg = Memo.fold (fun _ g acc -> Fcl_domain.size g + acc) xgoods 0
    and nbyg = Memo.fold (fun _ g acc -> Fcl_domain.size g + acc) ygoods 0 in
    if nbxg <> nbyg then failwith "nbxg & nbyg differs" else
      if nbxg <> Fcl_stak.get nb_goods then failwith "nbxg & nb_goods differs" in

  let print_debug id =
    Printf.printf "%s\nx:%a\ny:%a\nid:%d tightness:%g nb_goods:%d\n\txgoods:%a\n\tygoods:%a\n\txlast:%a\n\tylast:%a\n%!"
      name Fd.fprint x Fd.fprint y id
      (Fcl_stak.get tightness) (Fcl_stak.get nb_goods)
      fprint_memo xgoods fprint_memo ygoods
      Fcl_domain.fprint (Fcl_stak.get xlast)
      Fcl_domain.fprint (Fcl_stak.get ylast) in

  let init_tightness () =
    let nb =
      Memo.fold (fun _ gs acc -> Fcl_domain.size gs + acc) xgoods 0 in
    Fcl_stak.set nb_goods nb;
    let size_xy = Fd.size x * Fd.size y in
    Fcl_stak.set tightness (1. -. float nb /. float size_xy) in

  let update_tightness nb_less =
    let new_nb = Fcl_stak.get nb_goods - nb_less in
    Fcl_stak.set nb_goods new_nb;
    (*check_goods ();*)
    let size_xy = Fd.size x * Fd.size y in
    let tight = 1. -. float new_nb /. float size_xy in
    if tight < 0. then (print_debug (-1); failwith "debug")
                         Fcl_stak.set tightness tight in

  let update_tightness_zero () =
    Fcl_stak.set tightness 0.;
  (*Fcl_stak.set nb_goods (Fd.size x * Fd.size y)*) in

  let update_last () =
    Fcl_stak.set xlast (dom x);
    Fcl_stak.set ylast (dom y) in

  let diff_update var last =
    let dvar = dom var in
    let diff = Fcl_domain.difference (Fcl_stak.get last) dvar in
    Fcl_stak.set last dvar;
    diff in

  (* optimized processing when instantiated? seems slower... *)
  let update_var x xgoods xlast y ygoods ylast =
    if Fd.is_bound x then begin
        (* no need to update tightness when one variable is bound
         because tightness = 0. and the useful state resides in the
         size of the domain of the other variable only *)
        update_tightness_zero ();
        let vx_supp = Memo.find xgoods (Fd.elt_value x) in
        match Fd.value y with
          Val vy -> Fcl_domain.member vy vx_supp || Fcl_stak.fail name
        | Unk attry ->
           let ndomy = Fcl_domain.intersection (Attr.dom attry) vx_supp in
           Fd.refine y ndomy;
           true end
    else begin
        let diff = diff_update x xlast in
        (*Printf.printf "diff:%a\n" Fcl_domain.fprint diff;*)
        let nb_less = ref 0 in
        Fcl_domain.iter
          (fun vx ->
            match Memo.find_opt xgoods vx with
              None -> ()
            | Some vx_supp ->
               nb_less := !nb_less + Fcl_domain.size vx_supp;
               Fcl_domain.iter
                 (fun vy ->
                   try
                     if Memo.bt_remove_isempty ygoods vy vx then Fd.remove y vy
                   with Not_found -> ())
                 vx_supp;
               Memo.bt_remove_all xgoods vx)
          diff;
        if Fd.is_var y then update_tightness !nb_less;
        false end in

  let update id =
    (*Printf.printf "before update:\n";
    print_debug id;*)
    let result = if id = 0 then update_var x xgoods xlast y ygoods ylast
                 else update_var y ygoods ylast x xgoods xlast in
    (*Printf.printf "after update:\n";
    print_debug id;*)
    result in

  let init () =
    (*print_debug (-2);*)
    let xkeep = hdom x and ykeep = hdom y in
    List.iter
      (fun (vx, vy) ->
        Memo.add xgoods vx vy;
        Memo.add ygoods vy vx;
        Hashtbl.replace xkeep vx true;
        Hashtbl.replace ykeep vy true)
      goods;
    hrefine x xkeep;
    hrefine y ykeep;
    update_last ();
    init_tightness ();
    (*print_debug (-1);*)
    Fd.is_bound x || Fd.is_bound y in

  let delay c =
    delay [Fd.on_refine] x ~waking_id:0 c;
    delay [Fd.on_refine] y ~waking_id:1 c in

  let freevars () = Fd.vars2ids [x; y] in

  let tightness () = Fcl_stak.get tightness in

  Fcl_cstr.create ~name ~nb_wakings:2 ~init ~freevars ~tightness update delay

let cstr = ac4
