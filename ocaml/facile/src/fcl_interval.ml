(* $Id: fcl_interval.ml,v 1.15 2005-12-09 12:43:21 barnier Exp $ *)

open Fcl_var
open Fcl_arith




let cstr v inf sup b =
  let init () =
    Fcl_cstr.post (fd2e b <=~ i2e 1);
    Fcl_cstr.post (fd2e b>=~ i2e 0);
    false in
  let delay x =
    Fd.delay [Fd.on_subst] b x;
    Fd.delay [Fd.on_refine] v x in
  let update _ =
    match Fd.value b with
      Val 0 -> begin
	match (Fd.value v) with
	  Unk attr -> Fd.refine v (Fcl_domain.remove_closed_inter inf sup attr)
	| Val x ->
	    if x >= inf && x <= sup then Fcl_stak.fail "Interval.cstr" end;
	true
    | Val 1 ->
	begin match (Fd.value v) with
	  Unk _attr -> Fd.refine_low_up v inf sup
	  | Val x ->
              if x < inf || x > sup then Fcl_stak.fail "Interval.cstr" end;
	true
    | Unk _attr ->
	begin
	  match (Fd.value v) with
	    Val x ->
	      Fd.subst b (if x < inf || x > sup then 0 else 1);
	      true
	  | Unk v_attr ->
              let (minv, maxv) = Fcl_domain.min_max v_attr in
	      if minv > sup || maxv < inf then begin Fd.subst b 0; true end
(* on n'en fait pas plus pasque c'est trop couteux : on pourrait
   calculer l'intersection et si elle est vide b=0 *)
	      else if minv >= inf && maxv <= sup then begin
                Fd.subst b 1; true end
	      else false end
    | Val _ -> Fcl_debug.internal_error "Interval.cstr#update" in (* update *)

  Fcl_cstr.create ~init ~name:"Interval.cstr" update delay

let is_member v inf sup =
  let b = Fd.create Fcl_domain.boolean in
  Fcl_cstr.post (cstr v inf sup b);
  b;;
