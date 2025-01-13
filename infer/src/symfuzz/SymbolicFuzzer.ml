open! IStd
module F = Format
(* module L = Logging *)

module Env = Stdlib.Map.Make (String)

type mem_err = NPD of string | UAF of string | DFree of string | MLeak of string

type env = value Env.t

and value =
  | Vvoid
  | Vint of IntLit.t
  | Vfloat of float
  | Vstr of string
  | Vfun of Procname.t
  | Vptr of int
  | Vclass of Ident.name

let lookup (env : env) (id : string) : value =
  match Env.find_opt id env with Some v -> v | None -> raise (Failure "Undeclared identifier")


let eval_const (c : Const.t) : value =
  match c with
  | Cint ci ->
      Vint ci
  | Cfun cf ->
      Vfun cf
  | Cstr cs ->
      Vstr cs
  | Cfloat cf ->
      Vfloat cf
  | Cclass cc ->
      Vclass cc


let eval_unop (op : Unop.t) (v : value) : value =
  match op with
  | Unop.Neg -> (
    match v with
    | Vint lit ->
        Vint (IntLit.neg lit) (* Check Later : There may be more possiblities *)
    | Vfloat flt ->
        Vfloat (Stdlib.Float.neg flt)
    | Vvoid | Vstr _ | Vfun _ | Vptr _ | Vclass _ ->
        raise (Failure "Undefined Yet") (* Check Later : Undefined Yet *) )
  | Unop.BNot -> (
    match v with
    | Vint lit ->
        Vint (IntLit.logxor lit (IntLit.lognot IntLit.zero))
    | Vvoid | Vfloat _ | Vstr _ | Vfun _ | Vptr _ | Vclass _ ->
        raise (Failure "Undefined Yet") (* Check Later : Undefined Yet *) )
  | Unop.LNot -> (
    match v with
    | Vint lit ->
        if IntLit.eq lit IntLit.zero then Vint IntLit.one else Vint IntLit.zero
    | Vfloat _ | Vvoid | Vstr _ | Vfun _ | Vptr _ | Vclass _ ->
        raise (Failure "Undefined Yet") (* Check Later : Undefined Yet *) )


let eval_binop (op : Binop.t) (v1 : value) (v2 : value) : value =
  match op with
  | Binop.PlusA _ -> (
    (* Check Later : How to utilize Type Option Information? *)
    match (v1, v2) with
    | Vvoid, Vvoid ->
        Vvoid (* Check Later : Is it possible to add void to void? *)
    | Vint i1, Vint i2 ->
        Vint (IntLit.add i1 i2)
    | Vint i, Vfloat f | Vfloat f, Vint i ->
        (* Support float + int *)
        Vfloat (Stdlib.Float.add f (IntLit.to_float i))
    | Vfloat f1, Vfloat f2 ->
        Vfloat (Stdlib.Float.add f1 f2)
    | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid PlusA") )
  | Binop.PlusPI -> (
    match (v1, v2) with
    | Vptr pi, Vint i ->
        Vptr (pi + (IntLit.to_int i |> Stdlib.Option.get))
    (* Check Later : How is the PI performed? *)
    | Vvoid, _ | Vint _, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid PlusPI") )
  | Binop.MinusA _ -> (
    (* Check Later : How to utilize Type Option Information? *)
    match (v1, v2) with
    | Vvoid, Vvoid ->
        Vvoid (* Check Later : Is it possible to subtract void from void? *)
    | Vint i1, Vint i2 ->
        Vint (IntLit.add i1 (IntLit.neg i2))
    | Vint i, Vfloat f ->
        (* Support int - float *)
        Vfloat (Stdlib.Float.add (IntLit.to_float i) (Stdlib.Float.neg f))
    | Vfloat f, Vint i ->
        (* Support float - int *)
        Vfloat (Stdlib.Float.add f (Stdlib.Float.neg (IntLit.to_float i)))
    | Vfloat f1, Vfloat f2 ->
        Vfloat (Stdlib.Float.add f1 (Stdlib.Float.neg f2))
    | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid MinusA") )
  | Binop.MinusPI -> (
    match (v1, v2) with
    | Vptr pi, Vint i ->
        Vptr (pi - (IntLit.to_int i |> Stdlib.Option.get))
    (* Check Later : How is the PI performed? *)
    | Vvoid, _ | Vint _, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid MinusPI") )
  | Binop.MinusPP -> (
    match (v1, v2) with
    | Vptr pi1, Vptr pi2 ->
        Vptr (pi1 - pi2)
    | Vvoid, _ | Vint _, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid MinusPP") )
  | Binop.Mult _ -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        Vint (IntLit.mul i1 i2)
    | Vint i, Vfloat f | Vfloat f, Vint i ->
        Vfloat (Stdlib.Float.mul f (IntLit.to_float i))
    | Vfloat f1, Vfloat f2 ->
        Vfloat (Stdlib.Float.mul f1 f2)
    | Vvoid, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid Mult") )
  | Binop.DivI -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        if IntLit.iszero i2 then raise (Failure "Division by Zero") else Vint (IntLit.div i1 i2)
    | Vvoid, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid DivI") )
  | Binop.DivF -> (
    match (v1, v2) with
    | Vfloat f1, Vfloat f2 ->
        if Stdlib.Float.equal f2 Stdlib.Float.zero then raise (Failure "Division by Zero")
        else Vfloat (Stdlib.Float.div f1 f2)
    | Vint i, Vfloat f ->
        if Stdlib.Float.equal f Stdlib.Float.zero then raise (Failure "Division by Zero")
        else Vfloat (Stdlib.Float.div (IntLit.to_float i) f)
    | Vfloat f, Vint i ->
        if IntLit.iszero i then raise (Failure "Division by Zero")
        else Vfloat (Stdlib.Float.div f (IntLit.to_float i))
    | Vvoid, _ | Vint _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid DivF") )
  | Binop.Mod -> (
      let rec aux (l1 : IntLit.t) (l2 : IntLit.t) : IntLit.t =
        IntLit.add l1 (IntLit.neg (IntLit.mul l2 (IntLit.div l1 l2)))
        (* Check Later : Does this modulo actually work? *)
      in
      match (v1, v2) with
      | Vint i1, Vint i2 ->
          Vint (aux i1 i2)
      | Vvoid, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
          raise (Failure "Invalid Mod") )
  | Binop.Shiftlt -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        Vint (IntLit.shift_left i1 i2)
    | Vvoid, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid Shiftlt") )
  | Binop.Shiftrt -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        Vint (IntLit.shift_right i1 i2)
    | Vvoid, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid Shiftrt") )
  | Binop.Lt -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        if IntLit.lt i1 i2 then Vint (IntLit.of_int 1) else Vint (IntLit.of_int 0)
    | Vfloat f, Vint i ->
        if Stdlib.Float.sign_bit (Stdlib.Float.add f (Stdlib.Float.neg (IntLit.to_float i))) then
          Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vint i, Vfloat f ->
        if Stdlib.Float.sign_bit (Stdlib.Float.add (IntLit.to_float i) (Stdlib.Float.neg f)) then
          Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vfloat f1, Vfloat f2 ->
        if Stdlib.Float.sign_bit (Stdlib.Float.add f1 (Stdlib.Float.neg f2)) then
          Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vvoid, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid Lt")
        (* Check Later : Whether to add str comparison undecided yet. *) )
  | Binop.Gt -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        if IntLit.gt i1 i2 then Vint (IntLit.of_int 1) else Vint (IntLit.of_int 0)
    | Vfloat f, Vint i ->
        if Stdlib.Float.sign_bit (Stdlib.Float.add (IntLit.to_float i) (Stdlib.Float.neg f)) then
          Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vint i, Vfloat f ->
        if Stdlib.Float.sign_bit (Stdlib.Float.add f (Stdlib.Float.neg (IntLit.to_float i))) then
          Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vfloat f1, Vfloat f2 ->
        if Stdlib.Float.sign_bit (Stdlib.Float.add f2 (Stdlib.Float.neg f1)) then
          Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vvoid, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid Gt")
        (* Check Later : Whether to add str comparison undecided yet. *) )
  | Binop.Le -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        if IntLit.leq i1 i2 then Vint (IntLit.of_int 1) else Vint (IntLit.of_int 0)
    | Vfloat f, Vint i ->
        if not (Stdlib.Float.sign_bit (Stdlib.Float.add (IntLit.to_float i) (Stdlib.Float.neg f)))
        then Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vint i, Vfloat f ->
        if not (Stdlib.Float.sign_bit (Stdlib.Float.add f (Stdlib.Float.neg (IntLit.to_float i))))
        then Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vfloat f1, Vfloat f2 ->
        if not (Stdlib.Float.sign_bit (Stdlib.Float.add f2 (Stdlib.Float.neg f1))) then
          Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vvoid, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid Le")
        (* Check Later : Whether to add str comparison undecided yet. *) )
  | Binop.Ge -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        if IntLit.geq i1 i2 then Vint (IntLit.of_int 1) else Vint (IntLit.of_int 0)
    | Vfloat f, Vint i ->
        if not (Stdlib.Float.sign_bit (Stdlib.Float.add f (Stdlib.Float.neg (IntLit.to_float i))))
        then Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vint i, Vfloat f ->
        if not (Stdlib.Float.sign_bit (Stdlib.Float.add (IntLit.to_float i) (Stdlib.Float.neg f)))
        then Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vfloat f1, Vfloat f2 ->
        if not (Stdlib.Float.sign_bit (Stdlib.Float.add f1 (Stdlib.Float.neg f2))) then
          Vint (IntLit.of_int 1)
        else Vint (IntLit.of_int 0)
    | Vvoid, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Invalid Ge")
        (* Check Later : Whether to add str comparison undecided yet. *) )
  | Binop.Eq -> (
    match (v1, v2) with
    | Vvoid, Vvoid ->
        Vint IntLit.one
    | Vint i1, Vint i2 ->
        if IntLit.eq i1 i2 then Vint IntLit.one else Vint IntLit.zero
    | Vfloat f, Vint i | Vint i, Vfloat f ->
        if Stdlib.Float.equal f (IntLit.to_float i) then Vint IntLit.one else Vint IntLit.zero
    | Vfloat f1, Vfloat f2 ->
        if Stdlib.Float.equal f1 f2 then Vint IntLit.one else Vint IntLit.zero
    | Vstr s1, Vstr s2 ->
        if Stdlib.String.equal s1 s2 then Vint IntLit.one else Vint IntLit.zero
    | Vptr ptr1, Vptr ptr2 ->
        if Stdlib.Int.equal ptr1 ptr2 then Vint IntLit.one else Vint IntLit.zero
    | Vfun _, _ | Vclass _, _ | _ ->
        Vint IntLit.zero )
  | Binop.Ne -> (
    match (v1, v2) with
    | Vvoid, Vvoid ->
        Vint IntLit.zero
    | Vint i1, Vint i2 ->
        if IntLit.eq i1 i2 then Vint IntLit.zero else Vint IntLit.one
    | Vfloat f, Vint i | Vint i, Vfloat f ->
        if Stdlib.Float.equal f (IntLit.to_float i) then Vint IntLit.zero else Vint IntLit.one
    | Vfloat f1, Vfloat f2 ->
        if Stdlib.Float.equal f1 f2 then Vint IntLit.zero else Vint IntLit.one
    | Vstr s1, Vstr s2 ->
        if Stdlib.String.equal s1 s2 then Vint IntLit.zero else Vint IntLit.one
    | Vptr ptr1, Vptr ptr2 ->
        if Stdlib.Int.equal ptr1 ptr2 then Vint IntLit.zero else Vint IntLit.one
    | Vfun _, _ | Vclass _, _ | _ ->
        Vint IntLit.one )
  | Binop.BAnd -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        Vint (IntLit.logand i1 i2)
    | Vvoid, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ | _ ->
        raise (Failure "Undefined BAnd") )
  | Binop.BXor -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        Vint (IntLit.logxor i1 i2)
    | Vvoid, _ | Vint _, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ ->
        raise (Failure "Undefined BXor") )
  | Binop.BOr -> (
    match (v1, v2) with
    | Vint i1, Vint i2 ->
        Vint (IntLit.logor i1 i2)
    | Vvoid, _ | Vint _, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ ->
        raise (Failure "Undefined BOr") )
  | Binop.LAnd -> (
    match (v1, v2) with
    | Vint i1, _ when IntLit.eq i1 IntLit.zero ->
        Vint IntLit.zero
    | Vint i1, Vint i2 ->
        if IntLit.eq i1 IntLit.one && IntLit.eq i2 IntLit.one then Vint IntLit.one
        else Vint IntLit.zero
    | Vvoid, _ | Vint _, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ ->
        raise (Failure "Undefined LAnd") )
  | Binop.LOr -> (
    match (v1, v2) with
    | Vint i1, _ when IntLit.eq i1 IntLit.one ->
        Vint IntLit.one
    | Vint i1, Vint i2 ->
        if IntLit.eq i1 IntLit.one || IntLit.eq i2 IntLit.one then Vint IntLit.one
        else Vint IntLit.zero
    | Vvoid, _ | Vint _, _ | Vfloat _, _ | Vstr _, _ | Vfun _, _ | Vptr _, _ | Vclass _, _ ->
        raise (Failure "Undefined LOr") )


let rec eval (env : env) (e : Exp.t) : value =
  match e with
  | Exp.Var id ->
      lookup env (Ident.to_string id)
  | Exp.Const c ->
      eval_const c
  | Exp.Cast (_, e1) ->
      eval env e1 (* Check Later : How to deal with type castings? *)
  | Exp.UnOp (op, e1, _) ->
      eval_unop op (eval env e1)
  | Exp.BinOp (bop, e1, e2) ->
      eval_binop bop (eval env e1) (eval env e2)
  | Exp.Exn e1 ->
      eval env e1 (* Check Later : How to deal with Exceptions? *)
  | Exp.Closure _ | Exp.Lvar _ | Exp.Lfield _ | Exp.Lindex _ | Exp.Sizeof _ ->
      Vvoid


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
