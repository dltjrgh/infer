open! IStd
module F = Format
(* module L = Logging *)

module Env = Map.Make (String)

type env = value Env.t

and value = VInt of int | VVar of string

let rec eval (_env : env) (e : Exp.t) : value = match e with Var _ident -> VInt 0 | _ -> VInt 0

(* let rec exec_instr (astate: state) (inst: instr) : state = _ *)

let checker () =
  let _ = print_endline "Start Logging SourceFiles.." in
  let _ =
    SourceFiles.pp_all
      ~filter:(fun _ -> true)
      ~type_environment:true ~procedure_names:true ~freshly_captured:true (F.get_str_formatter ())
      ()
  in
  let sourcefile_log = F.flush_str_formatter () in
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
                  Procdesc.Node.pp_with_instrs ~print_types:true (F.get_str_formatter ()) node )
            in
            ()
            (* Procdesc.pp_with_instrs (F.get_std_formatter ()) pdesc *)
        | None ->
            () )
  in
  let nodes_log = F.flush_str_formatter () in
  let _ = print_endline "Finish Logging Nodes.." in
  let channel = Out_channel.create "symfuzz.log" in
  let _ = Out_channel.output_string channel sourcefile_log in
  let _ = Out_channel.output_string channel nodes_log in
  Out_channel.close channel ;
  ()
