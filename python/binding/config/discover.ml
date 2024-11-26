module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun c ->
      let system =
        match C.ocaml_config_var c "system" with
        | Some s -> s
        | None -> "unknown"
      in
      let flags =
        if system = "mingw" then [ "-cclib"; "-lasmrun" ]
        else [ "-cclib"; "-lasmrun_pic" ]
      in
      C.Flags.(write_sexp "flags.sexp" flags))
