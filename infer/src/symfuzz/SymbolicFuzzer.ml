open! IStd
module F = Format
(* module L = Logging *)

let checker () =
  let _ = print_endline "Start Logging SourceFiles.." in
  let _ =
    SourceFiles.pp_all
      ~filter:(fun _ -> true)
      ~type_environment:true ~procedure_names:true ~freshly_captured:true (F.get_std_formatter ())
      ()
  in
  let _ = print_endline "Finish Logging SourceFiles.." in
  let _ = print_endline "Start Logging Nodes.." in
  let source_files = SourceFiles.get_all ~filter:(fun _ -> true) () in
  let procnames =
    List.concat_map source_files ~f:(fun src -> SourceFiles.proc_names_of_source src)
  in
  let _ =
    List.map procnames ~f:(fun procname ->
        match Procdesc.load procname with
        | Some pdesc ->
            let nodes = Procdesc.get_nodes pdesc in
            let _ =
              List.fold_left nodes ~init:() ~f:(fun () node ->
                  Procdesc.Node.pp_with_instrs ~print_types:true (F.get_std_formatter ()) node )
            in
            ()
            (* Procdesc.pp_with_instrs (F.get_std_formatter ()) pdesc *)
        | None ->
            () )
  in
  let _ = print_endline "Finish Logging Nodes.." in
  ()
