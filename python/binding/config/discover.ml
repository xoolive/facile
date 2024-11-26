module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun c ->
      let system =
        C.ocaml_config_var c "system" |> Option.value ~default:"unknown"
      in
      print_endline system;
      let flags =
        if system = "win64" then [ "-cclib"; "-lasmrun" ]
        else [ "-cclib"; "-lasmrun_pic" ]
      in
      C.Flags.(write_sexp "flags.sexp" flags))
