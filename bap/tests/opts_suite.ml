open Ast
open Ast_convenience
open OUnit
open Type

let duplicate () =
  let x = Var.newvar "x" reg_1 in
  let y = Var.newvar "y" reg_1 in
  (* let z = Var.newvar "z" reg_1 in *)
  let e = Var.newvar "e" reg_1 in
  let l1,l2,le = newlab (), newlab (), newlab () in
  let p = [
    CJmp(Var e, exp_of_lab l1, exp_of_lab l2, []);
    Ast.Label(l1, []);
    Move(x, exp_true, []);
    Jmp(exp_of_lab le, []);
    Ast.Label(l2, []);
    Move(x, exp_false, []);
    Ast.Label(le, []);
    Move(y, Var x, []);
    Assert(Var y, []);
    (* Move(z, Var y, [Liveout]); *)
  ] in
  let ssacfg = Cfg_ssa.of_ast p in
  let ssacfg = Ssa_simp.simp_cfg ssacfg in
  (* Cfg_pp.SsaStmtsDot.output_graph stdout ssacfg; *)
  let num_assigns = ref 0 in
  let v = object(self)
    inherit Ssa_visitor.nop
    method visit_stmt = function
      | Ssa.Move(v, _, _) when (Var.name v) = "x" ->
        incr num_assigns; SkipChildren
      | _ -> SkipChildren
  end in
  ignore(Ssa_visitor.cfg_accept v ssacfg);
  assert_equal ~printer:string_of_int 3 !num_assigns

let suite = "Opts" >:::
  [
    "duplicate" >:: duplicate
  ]
