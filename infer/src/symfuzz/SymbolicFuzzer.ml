open! IStd
module F = Format
(* module L = Logging *)

let checker () =
  let _ = print_endline "Before Logging" in
  let _ =
    SourceFiles.pp_all
      ~filter:(fun _ -> true)
      ~type_environment:true ~procedure_names:true ~freshly_captured:true (F.get_std_formatter ())
      ()
  in
  let _ = print_endline "After Logging" in
  ()
