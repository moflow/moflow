(* Type checking for BAP.
*)

open Type
open Ast
open Big_int_Z
open Big_int_convenience
module D = Debug.Make(struct let name="Typecheck" and default=`NoDebug end)
open D

exception TypeError of string

let terror s = raise(TypeError s)

(* returns true if t1 equals t2 *)
let eq t1 t2 =
  t1 = t2

(* returns true if t1 is a multiple of t2 *)
let mult t1 t2 = match t1, t2 with
  | Reg x, Reg y -> x mod y = 0
  (* Can we do anything else for memory types? *)
  | x, y -> eq x y

let check checkf errmsg t1 t2 =
  if not (checkf t1 t2) then
    terror (Printf.sprintf errmsg (Pp.typ_to_string t1) (Pp.typ_to_string t2))

let check_eq t1 t2 f =
  check eq f t1 t2

let check_mult t1 t2 f =
  check mult f t1 t2

(* let check_eq t1 t2 f = *)
(*   if not (eq t1 t2) then *)
(*     terror (Printf.sprintf f (Pp.typ_to_string t1) (Pp.typ_to_string t2)) *)

let is_integer_type = function
  | Reg _ -> true
  | TMem _ | Array _ -> false

let is_mem_type t = not (is_integer_type t)

let index_type_of = function
  | TMem (it, _) | Array (it, _) -> it
  | Reg _ -> invalid_arg "index_type_of"

let value_type_of = function
  | TMem (_, vt) -> vt
  | Array (_, vt) -> vt
  | Reg _ -> invalid_arg "value_type_of"

let bits_of_width = function
  | Reg n -> n
  | _ -> invalid_arg "bits_of_width"

let bytes_of_width t =
  let b = bits_of_width t in
  if not ((b mod 8) = 0) then invalid_arg "bytes_of_width";
  b / 8

let rec infer_ast_internal check e =
  match e with
  | Var v ->
      (* FIXME: Check context *)
    Var.typ v
  | UnOp(_, e) ->
    if check then 
      (let t = infer_ast_internal true e in
       check_reg t);
    infer_ast_internal false e;
  | BinOp(o,e1,e2) as e ->
    if check then (
      let t1 = infer_ast_internal true e1
      and t2 = infer_ast_internal true e2 in
      check_same t1 t2 ~e;
      match o with
      | EQ | NEQ -> ()
      | _ -> check_reg t1);
    (match o with
    | EQ | NEQ | LT | LE | SLT | SLE -> reg_1
    | _ -> infer_ast_internal false e1
    )
  | Ite(b,e1,e2) ->
    if check then 
      (let t1 = infer_ast_internal true e1
       and t2 = infer_ast_internal true e2 in
       check_same t1 t2);
    infer_ast_internal false e1
  | Extract(h,l,e) ->
    let ns = int_of_big_int(h -% l +% bi1) in
    let nt = Reg ns in
    if check then (
      match infer_ast_internal true e with
      | Reg(oldn) ->
        if (ns <= 0) then terror("Extract must extract at least one bit");
        if l <% bi0 then terror("Lower bit index must be at least 0");
        if h >% (big_int_of_int oldn) -% bi1 then terror("Upper bit index must be at most one less than the size of the original register")
      | _ -> terror ("Extract expects Reg type")
    );
    nt
  | Concat(le, re) ->
    let lt, rt = infer_ast_internal check le, infer_ast_internal check re in
    let nt = match lt, rt with
      | Reg(lb), Reg(rb) ->
        Reg(lb+rb)
      | _ -> terror "Concat expects Reg type"
    in
    nt
  | Lab s ->
      (* FIXME: no type for labels yet *)
    reg_64
  | Int(_,t)
  | Unknown(_,t) ->
    t
  | Cast(ct,t,e) ->
    let te = infer_ast_internal check e in
    if check then (
      check_reg t;
      check_reg te;
      let bitse = bits_of_width te in
      let bitst = bits_of_width t in
      match ct with
      | CAST_UNSIGNED
      | CAST_SIGNED ->
        if bitst < bitse then terror (Printf.sprintf "Cast type %s is a widening case, but it was used to narrow %s to %s" (Pp.ct_to_string ct) (Pp.typ_to_string te) (Pp.typ_to_string t))
      | CAST_HIGH
      | CAST_LOW ->
        if bitst > bitse then terror (Printf.sprintf "Cast type %s is a narrowing case, but it was used to widen %s to %s" (Pp.ct_to_string ct) (Pp.typ_to_string te) (Pp.typ_to_string t))
    );
    t
  | Let(v,e1,e2) ->
    (* XXX: Need a type context to check this correctly *)
    if check then ignore(infer_ast_internal true e1);
    infer_ast_internal check e2
  | Load(arr,idx,endian, t) ->
    if check then check_mem arr idx endian t;
    t
  | Store(arr,idx,vl, endian, t) ->
    if check then (
      check_mem arr idx endian t;
      let tv = infer_ast_internal true vl in
      check_eq tv t "Store of value with type %s performed using a Store of type %s";
    );
    infer_ast_internal false arr

and check_same ?e ?s t1 t2 =
  if t1 <> t2 then
    let probs = match e, s with
      | Some e, _ -> ("\nProblem expression: "^(Pp.ast_exp_to_string e))
      | _, Some s -> ("\nProblem statement: "^(Pp.ast_stmt_to_string s))
      | None, None -> "" in
    terror ("Similar types expected: "^(Pp.typ_to_string t1)^" <> "^(Pp.typ_to_string t2)^probs)

and check_reg t =
  if not (is_integer_type t) then
    terror (Printf.sprintf "Expected integer type, but got %s" (Pp.typ_to_string t))

and check_bool t =
  if t <> Reg 1 then
    terror (Printf.sprintf "Expected bool type, but got %s" (Pp.typ_to_string t))

and check_mem arr idx endian t =
  let ta = infer_ast_internal true arr
  and ti = infer_ast_internal true idx
  and te = infer_ast_internal true endian in
  if te <> reg_1 then terror "Endian must be a boolean";
  if not(is_integer_type ti) then terror "Index must be a register type";
  match ta with
  | Array(i,e) ->
    check_eq ti i "Index type not suitable for indexing into this array. Index has type %s, but array has type %s.";
    check_eq t e "Can't get/put a %s from array with element type of %s";
  | TMem(i,e) ->
    check_eq ti i "Index type not suitable for indexing into this array. Index has type %s, but the memory has type %s.";
    check_mult t e "Can't get/put a %s from memory with element type of %s"
  | _ -> terror "Indexing only allowed from array or mem."

and check_cjmp_direct e =
  if Ast.lab_of_exp e = None then terror "Conditional jump targets must be direct (to a constant address or label)"

let infer_ast = infer_ast_internal false

let rec infer_ssa = function
  | Ssa.Int(_,t) -> t
  | Ssa.Var v -> Var.typ v
  | Ssa.Lab _ ->
    (* FIXME: no type for labels yet *)
    reg_64
  | Ssa.Load(_,_,_,t)
  | Ssa.Cast(_,t,_)
  | Ssa.Unknown(_,t)
    -> t
  | Ssa.BinOp((EQ|NEQ|LT|LE|SLT|SLE),_,_)
    -> reg_1
  | Ssa.Ite(_,v,_)
  | Ssa.BinOp(_,v,_)
  | Ssa.Store(v,_,_,_,_)
  | Ssa.UnOp(_,v) ->
      infer_ssa v
  | Ssa.Extract(h, l, v) ->
      let n = ((h -% l) +% bi1) in
      assert(n >=% bi1);
      Reg(int_of_big_int n)
  | Ssa.Concat(lv, rv) ->
      (match infer_ssa lv, infer_ssa rv with
      | Reg(lt), Reg(rt) -> Reg(lt + rt)
      | _ -> failwith "infer_ssa")
  | Ssa.Phi(x::_)
    -> Var.typ x
  | Ssa.Phi []
    -> failwith "Empty phi has no type"

let typecheck_expression e = ignore(infer_ast_internal true e)

(* Quick, informal, AST statement type checking.

   XXX: Formal type checking rules!
*)
let typecheck_stmt =
  let infer_te = infer_ast_internal true in
  function
    | Move(v, e, _) as s ->
      let vt = Var.typ v in
      let et = infer_te e in
      check_same ~s vt et
    | Jmp(e, _) ->
      let et = infer_te e in
      check_reg et
    | CJmp(ce, t1, t2, _) ->
      let et = infer_te ce in
      let t1t = infer_te t1 in
      let t2t = infer_te t2 in
      check_bool et;
      check_reg t1t;
      check_reg t2t;
      check_cjmp_direct t1;
      check_cjmp_direct t2
    | Halt(e, _) ->
      let et = infer_te e in
      (* Can we return a memory? Does this make sense? *)
      check_reg et
    | Assert(e, _)
    | Assume(e, _) ->
      let et = infer_te e in
      check_bool et
    | Label _
    | Comment _
    | Special _ ->
      ()

let typecheck_prog prog =
  let last_label = ref None in
  let is_asm = function
    | Asm _ -> true
    | _ -> false
  in
  let ts = function
    | Label (_, attrs) as s when List.exists is_asm attrs -> last_label := Some s
    | _ -> ()
  in
  List.iter (fun stmt -> ts stmt;
    try typecheck_stmt stmt
    with TypeError s ->
      match !last_label with
      | Some ls ->
        raise (TypeError (s^"\nat "^Pp.ast_stmt_to_string ls))
      | None -> raise (TypeError s)) prog
