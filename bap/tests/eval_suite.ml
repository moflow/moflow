open Arch
open Ast
open Ast_convenience
open Big_int
open Big_int_convenience
open OUnit
open Pcre
open Test_common
open Type

let test_file = "C/test";;
let il_file = "C/test.il";;

(** Lift C/test and convert it to bap.  Then inject "halt true" after the return
    in the main function. Print this out to the file test.il. *)
let concrete_eval_setup _ =
  let out = open_out il_file in
  let pp = new Pp.pp_oc out in
  let prog = Asmir.open_program test_file in
  let ranges = Func_boundary.get_function_ranges prog in
  let (start_addr,_) = find_funs ranges ["main"; "_main"] in
  let ir = Asmir.asmprogram_to_bap prog in
  let arch = Asmir.get_asmprogram_arch prog in
  let outir = inject_stmt ir start_addr "ret" halt_stmt in 
  pp#ast_program outir;
  pp#close;
  (ranges, start_addr, arch);;


(** Open the file test.il and run two concrete executions.  The first verifies
    running from main results in the desired value (42 = 0x2aL).  The second
    concrete execution changes the value on the stack to 43 (i), starts
    the execution at the "call <g>" assembly instruction in main, and verifies
	that the result is -1. *)
let concrete_eval_test (ranges, s, arch) = 
  (* i represents the change on the stack to the "wrong" value for function g *)
  let addrtype, sp = match arch with
    | X86_32 -> "u32", "R_ESP"
    | X86_64 -> "u64", "R_RSP"
  in
  let memtype = addrtype ^ "?u8" in
  let i =
    let a,_ = Parser.exp_from_string (sp ^ ":" ^ addrtype) in
    let e,_ = Parser.exp_from_string ("43:" ^ addrtype) in
    let t = Typecheck.infer_ast e in
    let m = match Parser.exp_from_string ("mem:" ^ memtype) with
      | Var(v), _ -> v
      | _ -> assert false
    in
    let s = Move(m, Store(Var(m), a, e, exp_false, t), []) in
    [s]
  in
  let prog,_ = Parser.program_from_file il_file in
  let ctx1 = Symbeval.concretely_execute ~s prog in
  let eax1 = biconst 0x2a in
  let (start_addr,end_addr) = find_funs ranges ["main"; "_main"] in
  let main_prog = Test_common.find_prog_chunk prog start_addr end_addr in
  let s = find_call main_prog in
  let ctx2 = Symbeval.concretely_execute ~s ~i prog in
  let regtype = Disasm_i386.type_of_mode (Disasm.arch_to_x86_mode arch) in
  let eax2 = Arithmetic.to_big_int(bim1, regtype) in
  let msg = "from check_functions" in
  typecheck prog;
  (try check_functions msg ranges ["main"; "g"]
   with _ -> check_functions msg ranges ["_main"; "_g"]);
  check_eax ctx1 eax1;
  check_eax ctx2 eax2;;


let concrete_eval_tear_down _ = rm_and_ignore il_file;;

let array_test () =
  let m = Var.newvar "mem_awesome" (TMem(Reg 7, reg_16)) in
  let le = Var.newvar "le" reg_32 in
  let be = Var.newvar "be" reg_32 in
  let p =
    Move(m, Store(Var m, Int(bi0, Reg 7), Int(bi1, reg_16), exp_false, reg_16), [])
    :: Move(m, Store(Var m, Int(bi1, Reg 7), Int(bi2, reg_16), exp_false, reg_16), [])
    :: Move(m, Store(Var m, Int(bi2, Reg 7), Int(bi3, reg_16), exp_false, reg_16), [])
    :: Move(le, Load(Var m, Int(bi0, Reg 7), exp_false, reg_32), [])
    :: Move(be, Load(Var m, Int(bi0, Reg 7), exp_true, reg_32), [])
    :: Assert(Var le ==* Int(bi64 0x00020001L, reg_32), [])
    :: Assert(Var be ==* Int(bi64 0x00010002L, reg_32), [])
    :: []
  in
  ignore(Symbeval.concretely_execute p)

let suite = "Eval" >:::
  [
    "concrete_eval_test" >::
      (bracket concrete_eval_setup concrete_eval_test concrete_eval_tear_down);
    "array_test" >:: array_test;
  ]
