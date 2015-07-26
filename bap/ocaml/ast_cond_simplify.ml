(* See Section 3.4.2, Predicates for Conditional Branch Instructions,
   of Gogul Balakrishnan's dissertation, WYSINWYX: WHAT YOU SEE IS NOT
   WHAT YOU EXECUTE.

   Available at http://pages.cs.wisc.edu/~bgogul/Research/Thesis/bgogul-thesis.pdf

   XXX: Verify these using SMT

*)

open Ast
open Big_int_convenience
module C=Cfg.AST
module D=Debug.Make(struct let name="Ast_cond_simplify" and default=`NoDebug end)
open D
open Type

let rec reverse_visit f e =
  let g = reverse_visit f in
  let e = match e with
    | Load(e1,e2,e3,t1) -> Load(g e1, g e2, g e3, t1)
    | Store(e1,e2,e3,e4,t1) -> Store(g e1, g e2, g e3, g e4, t1)
    | Ite(e1,e2,e3) -> Ite(g e1, g e2, g e3)
    | Extract(h,l,e) -> Extract(h, l, g e)
    | Concat(e1,e2) -> Concat(g e1, g e2)
    | BinOp(bt,e1,e2) -> BinOp(bt, g e1, g e2)
    | UnOp(ut,e) -> UnOp(ut, g e)
    | (Var _ | Lab _ | Int _ | Unknown _) as e -> e
    | Cast(ct,t,e) -> Cast(ct, t, g e)
    | Let(v,e,e') -> Let(v, g e, g e')
  in
  let e' = f e in
  (* If we changed e, use f on it again *)
  if e' <> e then g e' else e'

let simplify_flat = function
  | BinOp((OR|AND),
          e,
          e') when e === e' -> e
  | BinOp(EQ,
          Int(i0, _),
          BinOp(MINUS, e1, e2)) when i0 = bi0 ->
    (* e - e2 = 0 -> e = e2 *)
    BinOp(EQ, e1, e2)
  | BinOp(OR,
          BinOp(EQ, e1', e2'),
          BinOp(LT|SLT as bop, e1, e2))
  | BinOp(OR,
          BinOp(LT|SLT as bop, e1, e2),
          BinOp(EQ, e1', e2')) when e1 = e1' && e2 = e2' ->
    (* a < b || a == b -> a <= b *)
    let newbop = match bop with
      | LT -> LE
      | SLT -> SLE
      | _ -> failwith "impossible"
    in
    BinOp(newbop, e1, e2)
  | BinOp(LT|LE|SLT|SLE|EQ as op,
          BinOp(PLUS|MINUS as op2,
                e,
                Int(i1, t1)),
          Int(i2, _t2)) ->
    (* a + i (>|>=|eq) i' -> a (>|>=|eq) i' - i *)
    let newop = match op2 with PLUS -> MINUS | MINUS -> PLUS | _ -> failwith "impossible" in
    let newi, _ = Arithmetic.binop newop (i2,t1) (i1,t1) in
    BinOp(op,
          e,
          Int(newi, t1))
  | UnOp(NOT,
         BinOp(LE|LT|SLE|SLT|EQ|NEQ as bop, e1, e2)) ->
    (* not (e1 < e2) -> e2 <= e1 *)
    let newbop = match bop with
      | LE -> LT
      | LT -> LE
      | SLE -> SLT
      | SLT -> SLE
      | EQ -> NEQ
      | NEQ -> EQ
      | _ -> failwith "impossible" in
    BinOp(newbop, e2, e1)
  | BinOp(XOR,
          Cast(CAST_HIGH, Reg 1,
               BinOp(MINUS, e1, Int(i1, t1))),
          Cast(CAST_HIGH, Reg 1,
               BinOp(AND,
                     BinOp(XOR, e2, Int(i2, _t2)),
                     BinOp(XOR, e3,
                           BinOp(MINUS,
                                 e4,
                                 Int(i3, _t3)))))) when e1 = e2 && e2 = e3 && e3 = e4 && i1 = i2 && i2 = i3 ->
    (* Not as intuitive: signed less than comparison *)
    BinOp(SLT, e1, Int(i1, t1))
  | BinOp(LT|LE|EQ as bop, Int(i, t), Cast(CAST_UNSIGNED, t2, e)) when i <% (bi1 <<% (Typecheck.bits_of_width (Typecheck.infer_ast e))) ->
    let nt = Typecheck.infer_ast e in
    BinOp(bop, Int(i, nt), e)
  (* Mixed unsigned/signed. Not sure why compiler would generate this. *)
  | BinOp(LT|LE as bop, Int(i, t), Cast(CAST_SIGNED, t2, e)) when i <% (bi1 <<% (Typecheck.bits_of_width (Typecheck.infer_ast e))) && i <% (bi1 <<% (Typecheck.bits_of_width t2 - 1)) ->
    let nt = Typecheck.infer_ast e in
    BinOp(bop, Int(i, nt), e)

  | Cast(CAST_LOW, t2, Cast(CAST_UNSIGNED, t, e)) when Typecheck.bits_of_width t >= Typecheck.bits_of_width t2 ->
    Cast(CAST_UNSIGNED, t2, e)
  (* noop cast *)
  | Cast(_, t2, e) when t2 = Typecheck.infer_ast e ->
    e
  | e -> e

let simplify_flat e =
  if debug () then
    let e' = simplify_flat e in
    if e <> e' then
      dprintf "Simplified %s to %s" (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string e');
    e'
  else simplify_flat e

let simplify_exp = reverse_visit simplify_flat

let simplifycond_cfg g =
  (* Don't copy propagate over memory, since most programs do
     comparisons on registers. *)
  let stop_before =
    function
      | Store _ -> true
      | _ -> false
  in
  let stop_after =
    function
      | Load _ -> true
      | _ -> false
  in
  let cp = Copy_prop.copyprop_ast ~stop_before ~stop_after g in
  C.G.fold_vertex (fun v g ->
    let stmts = C.get_stmts g v in
    match List.rev stmts with
    | CJmp(e, tt, tf, a)::tl ->
      let _, _, copyprop = cp (v, List.length tl) in
      let e' = simplify_exp (copyprop e) in
      dprintf "e: %s (copyprop e): %s e': %s" (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (copyprop e)) (Pp.ast_exp_to_string e');
      (* Update statement *)
      let s = CJmp(e', tt, tf, a)::tl in
      let g = C.set_stmts g v (List.rev s) in

      (* Update edge conditions *)
      C.G.fold_succ_e (fun e g ->
        let g = C.remove_edge_e g e in
        let dst = C.G.E.dst e in
        let newlabel = match C.G.E.label e with
        | Some (Some true, _) -> Some(Some true, simplify_exp (BinOp(EQ, e', exp_true)))
        | Some (Some false, _) -> Some(Some false, simplify_exp (BinOp(EQ, e', exp_false)))
        | _ -> failwith "Unexpected unlabeled edge found from CJmp" in
        let newe = C.G.E.create v newlabel dst in
        C.add_edge_e g newe
      ) g v g
    | _ -> g
  ) g g
