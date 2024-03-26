let urbcsp var dom cstr nogood =
  let max_cstr = var * (var - 1) / 2
  and max_ng = dom * dom in
  let round x = truncate (x +. 0.5) in
  let cstr = round (float (cstr * max_cstr) /. 100.)
  and nogood = round (float (nogood * (max_ng-1)) /. 100.) in

  if var < 2 then
    Fcl_debug.fatal_error "Wrong number of variables"
  else if dom < 2 then
    Fcl_debug.fatal_error "Wrong size of domains"
  else if cstr < 0 || cstr > max_cstr then
    Fcl_debug.fatal_error "Wrong number of constraints"
  else if  nogood < 0 || nogood > max_ng - 1 then
    Fcl_debug.fatal_error "Wrong number of nogoods"
  else
    (* All unique couples of variables *)
    let cstr_array = Array.make max_cstr (0, 0)
    and i = ref 0 in
    for var1 = 0 to var - 2 do
      for var2 = var1 + 1 to var - 1 do
	cstr_array.(!i) <- (var1, var2);
	incr i
      done 
    done;
    
    let ll = ref [] in
    for c = 0 to cstr - 1 do
      let r = c + Random.int (max_cstr - c) in
      let selected_cstr = cstr_array.(r) in
      cstr_array.(r) <- cstr_array.(c);
      cstr_array.(c) <- selected_cstr;
      let (var1, var2) = selected_cstr in

      (* All couples from [0..dom-1] X [0..dom-1] *)
      let ng_array = Array.init max_ng (fun i -> i / dom, i mod dom) in
      let l = ref [] in
      for t = 0 to nogood - 1 do
	let rng = t + Random.int (max_ng - t) in
	let selected_ng = ng_array.(rng) in
	ng_array.(rng) <- ng_array.(t);
	ng_array.(t) <- selected_ng;
	l := selected_ng :: !l
      done;
      ll := (var1, var2, !l) :: !ll;
    done;
    !ll
