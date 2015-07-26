(** Convert memory style accesses to array accesses.

   This modules converts all TMem references to normalized array references.

    @author Edward J. Schwartz
*)

(* TODO: Handle different endianness.  Use the type of the array expression *)

module D = Debug.Make(struct let name="memory2array" and default=`Debug end)
open D

open Ast
open Ast_convenience
open BatListFull
open Big_int_Z
open Grammar_scope
open Type
open Util
open Var

let numelems t mt =
  let bits = Typecheck.bits_of_width t in
  let elebits = Typecheck.bits_of_width mt in
  if (bits mod elebits) <> 0 then failwith "Expected all memory accesses to be multiples of the element type";
  bits / elebits

let split_load array index indextype valuetype accesstype num =
  let indexplus = BinOp(PLUS, index, Int(big_int_of_int num, indextype)) in
  (* we are loading base values, so endian doesn't matter here *)
  Load(array, indexplus, exp_false, valuetype)

let split_load_list mvar index indextype valuetype loadtype endian =
  let nelem = numelems loadtype valuetype in
  let open BatPervasives in
  let enum =
    if endian
    then 0 -- (nelem - 1) (* big *)
    else ((nelem - 1) --- 0) (* little *)
  in
  let explist = map (split_load (Var mvar) index indextype valuetype loadtype) enum in
  explist

let split_loads array index loadtype endian =
  let t = Typecheck.infer_ast array in
  let indextype = Typecheck.index_type_of t in
  let valuetype = Typecheck.value_type_of t in
  let mvar = Var_temp.nt "loadnorm" (Array(indextype, valuetype)) in
  (* build the expression for big and little endian *)
  let f endian =
    let reads = split_load_list mvar index indextype valuetype loadtype endian in
    concat_explist reads
  in
  let exp = exp_ite endian (f true) (f false) in
  Let(mvar, array, exp)

let split_write array index indextype valuetype accesstype data nelems endian num =
  let bits = Typecheck.bits_of_width valuetype in
  let exp = extract (((num+1) * bits) - 1) (num*bits) data in
  let indexplus =
    if endian
    then index +* (Int(big_int_of_int (nelems - 1 - num), indextype))
    else index +* (Int(big_int_of_int num, indextype))
  in
  (* we are writing base values, so endian doesn't matter here *)
  let exp = Store(array, indexplus, exp, exp_false, valuetype) in
  exp

let split_write_list tempmemvar tempvalvar index indextype valuetype storetype nelems endian data =
  let open BatPervasives in
  let singlewrites = map (split_write (Var tempmemvar) index indextype valuetype storetype (Var tempvalvar) nelems endian) ((nelems - 1) --- 0) in
  (singlewrites, tempmemvar, tempvalvar)

let split_writes array index storetype endian data =
  let t = Typecheck.infer_ast array in
  let indextype = Typecheck.index_type_of t in
  let valuetype = Typecheck.value_type_of t in
  let nelems = numelems storetype valuetype in
  let tempmemvar = Var_temp.nt "tempmem" (Array(indextype, valuetype)) in
  let tempvalvar = Var_temp.nt "tempval" storetype in
  let f endian =
    let (singlewrites, tempmemvar, tempvalvar) = split_write_list tempmemvar tempvalvar index indextype valuetype storetype nelems endian data in
    let open BatPervasives in
    let letexp = fold (fun expr new_expr -> Let(tempmemvar, new_expr, expr)) (Var tempmemvar) singlewrites in
    letexp
  in
  let letexp = exp_ite endian (f true) (f false) in
  let letexp = Let(tempvalvar, data, letexp) in
  Let(tempmemvar, array, letexp)

class memory2array_visitor ?scope hash =

  let get_array_var (Var.V(_, _, t) as avar) =
    match t with
    | TMem (idxt, Reg bitwidth) ->
      let array =
        try VarHash.find hash avar
        with Not_found ->
          (* We haven't mapped variable v before. If a scope of
             variables was provided, let's see if v_array is in
             scope. *)
          let new_name = (Var.name avar)^"_array" in
          let new_type = Array (idxt, Reg bitwidth) in
          let make_new_var () = newvar new_name new_type in
          let newarrvar = match scope with
            | Some(scope) ->
              (try Scope.get_lval_if_defined scope new_name (Some new_type)
               with Not_found -> make_new_var ())
            | None ->
              make_new_var ()
          in

          (* Map in the m2a hash *)
          VarHash.add hash avar newarrvar;

          (* Update the scope if defined *)
          (match scope with
          | Some(scope) ->
            (* First, ensure that if new_name is already in Scope, that
               it maps to newarrvar.  If not, the memory2array hash and
               Scope are out of sync, which indicates someone did
               something bad. *)
            (try assert (Var.equal (Scope.get_lval_if_defined scope new_name (Some new_type)) newarrvar)
             with Not_found -> (* If not in Scope, it's okay *) ());

            (* Now, update the Scope *)
            ignore(Scope.add_var scope new_name newarrvar);

          | None -> ());

          newarrvar
      in
      array
    | _ -> failwith "get_array_var expects a TMem variable only"
  in

object (self)
  inherit Ast_visitor.nop

  method visit_avar avar =
    match Var.typ avar with
    | TMem _ ->
      ChangeToAndDoChildren (get_array_var avar)
    |   _ ->
      DoChildren

  method visit_rvar = self#visit_avar

  method visit_exp exp =
    (* Printf.printf "Visiting expression %s\n" (Pp.ast_exp_to_string exp); *)
    match exp with
    | Load(arr,idx,endian,t) -> (
      let width = numelems t (Typecheck.value_type_of (Typecheck.infer_ast arr)) in
      match width with
      | 1 ->
        DoChildren
      | _ ->
        let arr = Ast_visitor.exp_accept self arr in
        let newexpr = split_loads arr idx t endian
        in
        ChangeToAndDoChildren newexpr)
    | Store(arr,idx,data,endian,t) -> (
      let width = numelems t (Typecheck.value_type_of (Typecheck.infer_ast arr)) in
      match width with
      | 1 ->
        DoChildren
      | _ ->
        let arr = Ast_visitor.exp_accept self arr in
        let newexpr = split_writes arr idx t endian data in
        ChangeToAndDoChildren newexpr)
    | _ -> DoChildren
  end

type state = Ast.var VarHash.t

let create_state () = VarHash.create 1000

let coerce_prog prog =
  let hash = create_state () in
  let visitor = new memory2array_visitor hash in
  Ast_visitor.prog_accept visitor prog

let coerce_prog_state ?scope hash prog =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.prog_accept visitor prog

let coerce_exp exp =
  let hash = create_state () in
  let visitor = new memory2array_visitor hash in
  Ast_visitor.exp_accept visitor exp

let coerce_exp_state ?scope hash exp =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.exp_accept visitor exp

let coerce_rvar_state ?scope hash v =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.rvar_accept visitor v
