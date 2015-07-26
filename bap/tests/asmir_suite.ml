open OUnit

let open_program_test () =
  ignore (Asmir.open_program "asm/nop");;

let recover asmp (n,s,e) = ignore(Asmir_disasm.vsa_at asmp s)

let recover_switch asmp (n,s,e) =
  let astcfg, vsaresult = Asmir_disasm.vsa_at_full asmp s in
  match vsaresult with
  | None -> ()
  | Some vsaresult ->
    let ssacfg, allgood = Switch_condition.add_switch_conditions_disasm vsaresult in
    assert_bool "CFG with switch conditions was not recovered (disasm interface)" allgood;
    (* XXX: Use a real calling convention *)
    let astcfg = Hacks.filter_calls_cfg astcfg in
    let ssacfg, allgood = Switch_condition.add_switch_conditions_ssacfg asmp (Cfg_ssa.of_astcfg astcfg) in
    assert_bool "CFG with switch conditions was not recovered (ssacfg interface)" allgood

let resolve_program_test f p =
  let asmp = Asmir.open_program p in
  let funcs = Func_boundary.get_function_ranges asmp in
  List.iter (f asmp) funcs

let run_resolve_test s () =
  resolve_program_test recover s

let run_resolve_switch_test s () =
  resolve_program_test recover_switch s

let make_recover_test name fname =
  [
    name^"_regular" >:: run_resolve_test fname;
    name^"_switch" >:: run_resolve_switch_test fname;
  ]

let suite = "Asmir" >:::
  [
    "open_program_test" >:: open_program_test;
  ]
  @ make_recover_test "resolve_test" "C/recover-hard"
  @ make_recover_test "resolve_test_opt" "C/recover-hard-opt"
  @ make_recover_test "resolve_test_pointer" "C/recover-hard-pointer"
