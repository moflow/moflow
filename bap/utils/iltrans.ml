let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

open Arch
open Utils_common
open Big_int_convenience

type ast = Ast.program
type astcfg = Cfg.AST.G.t
type ssa = Cfg.SSA.G.t

type prog =
  | Ast of ast
  | AstCfg of astcfg
  | Ssa of ssa

type cmd =
  | AnalysisAst of (ast -> unit)
  | AnalysisModeAst of (arch -> ast -> unit)
  | AnalysisAstCfg of (astcfg -> unit)
  | AnalysisModeAstCfg of (arch -> astcfg -> unit)
  | AnalysisSsa of (ssa -> unit)
  | TransformAst of (ast -> ast)
  | TransformModeAst of (arch -> ast -> ast)
  | TransformAstCfg of (astcfg -> astcfg)
  | TransformSsa of (ssa -> ssa)
  | ToCfg
  | ToAst
  | ToSsa
 (* add more *)

let pipeline = ref []
let startdebug = ref 1

let tac = ref true

(* used for some trace exploit options *)
let offset_helper f p =
  (fun arch ->
    let bytes = bi (Arch.bytes_of_arch arch) in
    f bytes p arch)

let output_ast f p =
  let oc = open_out f in
  let pp = new Pp.pp_oc oc in
  pp#ast_program p;
  pp#close

let output_ast_cfg f p =
  let oc = open_out f in
  Cfg_pp.AstStmtsDot.output_graph oc p;
  close_out oc

let output_ast_asms f p =
  let oc = open_out f in
  Cfg_pp.AstAsmsDot.output_graph oc p;
  close_out oc

let output_ast_bbids f p =
  let oc = open_out f in
  Cfg_pp.AstBBidDot.output_graph oc p;
  close_out oc

let output_ast_cdg f p =
  let oc = open_out f in
  let cdg = Depgraphs.CDG_AST.compute_cdg p in
    Cfg_pp.AstBBidDot.output_graph oc cdg;
    close_out oc

let output_ast_pdg f p =
  let oc = open_out f in
  let pdg = Depgraphs.PDG_AST.compute_pdg p in
    Cfg_pp.AstStmtsDot.output_graph oc pdg;
    close_out oc

let output_ssa f p =
  let oc = open_out f in
  Cfg_pp.SsaStmtsDot.output_graph oc p;
  close_out oc

let output_ssa_bbids f p =
  let oc = open_out f in
  Cfg_pp.SsaBBidDot.output_graph oc p;
  close_out oc

let output_ssa_cdg f p =
  let oc = open_out f in
  let cdg = Depgraphs.CDG_SSA.compute_cdg p in
    Cfg_pp.SsaBBidDot.output_graph oc cdg;
    close_out oc

let output_ssa_ddg f p =
  let oc = open_out f in
  let ddg = Depgraphs.DDG_SSA.compute_ddg p in
    Cfg_pp.SsaStmtsDot.output_graph oc ddg;
    close_out oc

let output_c f p =
  let oc = open_out f in
  let ft = Format.formatter_of_out_channel oc in
  let pp = new To_c.pp ft in
  pp#ast_program p;
  close_out oc

let to_dsa p =
  let p,_ = Traces.to_dsa p in
  p

let output_domtree g =
  let module C = Cfg.AST in
  let module D = Dominator.Make(struct
    include C.G
    let v2s = C.v2s
  end) in
  let idom = D.compute_idom g (C.find_vertex g Cfg.BB_Entry) in
  let dom_tree = D.idom_to_dom_tree g idom in
  C.G.iter_vertex (fun v ->
    Printf.printf "%s is the immediate dominator of %s\n" (C.v2s v) (BatString.join ", " (List.map C.v2s (dom_tree v)));
  ) g

let output_pdomtree g =
  let module C = Cfg.AST in
  let module C' = Depgraphs.MakeRevCfg(C) in
  let module D = Dominator.Make(struct
    include C'
    let v2s = C.v2s
  end) in
  let idom = D.compute_idom g (C.find_vertex g Cfg.BB_Exit) in
  let dom_tree = D.idom_to_dom_tree g idom in
  C.G.iter_vertex (fun v ->
    Printf.printf "%s is the immediate post-dominator of %s\n" (C.v2s v) (BatString.join ", " (List.map C.v2s (dom_tree v)));
  ) g

let output_structanal p =
  let cfg = Prune_unreachable.prune_unreachable_ast p in
  let sa = Structural_analysis.structural_analysis cfg in
  print_endline "Structural analysis results:";
  print_endline (Structural_analysis.node2s sa)

let sccvn p =
  fst(Sccvn.replacer p)
let deadcode p =
  fst(Deadcode.do_dce p)
let adeadcode p =
  fst(Deadcode.do_aggressive_dce p)
let jumpelim p =
  fst(Ssa_simp_misc.cfg_jumpelim p)
let ast_coalesce = Coalesce.coalesce_ast
let ssa_coalesce = Coalesce.coalesce_ssa

let vsa_print a g =
  let g = Hacks.ast_remove_indirect g in
  let g = Ast_cond_simplify.simplifycond_cfg g in
  let opts = Vsa_ast.build_default_arch_options a in
  let _df_in, df_out = Vsa_ast.vsa ~nmeets:50 opts g in
  Cfg.AST.G.iter_vertex (fun v ->
    Printf.printf "VSA @%s" (Cfg_ast.v2s v);
    Vsa_ast.AbsEnv.pp print_string (BatOption.get (df_out (Vsa_ast.last_loc g v)));
    print_string "\n\n"
  ) g

(* Chop code added *)
let ast_chop srcbb srcn trgbb trgn p =
  Ast_slice.CHOP_AST.chop p !srcbb !srcn !trgbb !trgn
let ssa_chop srcbb srcn trgbb trgn p =
  Ssa_slice.CHOP_SSA.chop p !srcbb !srcn !trgbb !trgn

let usedef p =
  let module UD = Depgraphs.UseDef_AST in
  let module VM = Var.VarMap in
  let h,_ = UD.usedef p in
  Hashtbl.iter
    (fun (bb,i) varmap ->
      Printf.printf "At location %s %d:\n" (Cfg_ast.v2s bb) i;
      VM.iter
        (fun v defset ->
          let defs = try BatList.reduce (fun s s2 -> s^" "^s2) (List.map UD.LocationType.to_string (UD.LS.elements defset)) with _ -> "" in
          Printf.printf "use %s -> def %s\n" (Pp.var_to_string v) defs
        ) varmap;
      Printf.printf "\n"
    ) h

let defuse p =
  let module UD = Depgraphs.UseDef_AST in
  let module VM = Var.VarMap in
  let h,_ = UD.defuse p in
  Hashtbl.iter
    (fun (bb,i) defset ->
      Printf.printf "The location %s %d is used by:\n" (Cfg_ast.v2s bb) i;
      let elements = UD.LS.elements defset in
      let defs = match elements with
        | [] -> "none"
        | _ -> BatList.reduce (fun s s2 -> s^" "^s2) (List.map UD.LocationType.to_string elements)
      in
      Printf.printf "%s" defs;
      Printf.printf "\n"
    ) h

let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

let speclist =
  ("-pp-ast", Arg.String(fun f -> add(AnalysisAst(output_ast f))),
   "<file> Pretty print AST to <file>.")
  ::("-pp-ast-cfg", Arg.String (fun f -> add(AnalysisAstCfg(output_ast_cfg f))),
     "<file> Pretty print AST graph to <file> (in Graphviz format)")
  ::("-pp-ast-asms", Arg.String (fun f -> add(AnalysisAstCfg(output_ast_asms f))),
     "<file> Pretty print AST graph to <file> (in Graphviz format) (only assembly)")
  ::("-pp-ast-bbids", Arg.String(fun f -> add(AnalysisAstCfg(output_ast_bbids f))),
     "<file> Pretty print AST graph to <file> (in Graphviz format) (no stmts)")
  ::("-pp-ast-cdg", Arg.String (fun f -> add(AnalysisAstCfg(output_ast_cdg f))),
     "Output the AST CDG (bbid's)")
  ::("-pp-ast-pdg", Arg.String (fun f -> add(AnalysisAstCfg(output_ast_pdg f))),
     "Output the AST DDG (bbid's)")
  ::("-pp-ssa", Arg.String(fun f -> add(AnalysisSsa(output_ssa f))),
     "<file> Pretty print SSA graph to <file> (in Graphviz format)")
  ::("-pp-ssa-bbids", Arg.String(fun f -> add(AnalysisSsa(output_ssa_bbids f))),
     "<file> Pretty print SSA graph to <file> (in Graphviz format) (no stmts)")
  ::("-pp-ssa-cdg", Arg.String (fun f -> add(AnalysisSsa(output_ssa_cdg f))),
     "Output the SSA CDG (bbid's)")
  ::("-pp-ssa-ddg", Arg.String (fun f -> add(AnalysisSsa(output_ssa_ddg f))),
     "Output the SSA DDG (bbid's)")
  ::("-pp-novarnums", Arg.Unit (fun () -> Pp.output_varnums := false),
     "Print variables without variable ID numbers")
  ::("-struct", Arg.Unit (fun () -> add(AnalysisAstCfg(output_structanal))),
     "Structural analysis.")
  ::("-domtree", Arg.Unit (fun () -> add(AnalysisAstCfg output_domtree)),
     "Output the dominator tree.")
  ::("-pdomtree", Arg.Unit (fun () -> add(AnalysisAstCfg output_pdomtree)),
     "Output the post dominator tree.")
  ::("-to-cfg", uadd(ToCfg),
     "Convert to an AST CFG.")
  ::("-to-ast", uadd(ToAst),
     "Convert to the AST.")
  ::("-to-ssa", uadd(ToSsa),
     "Convert to SSA.")
  :: ("-to-c", Arg.String(fun f -> add(AnalysisAst(output_c f))),
      "<file> Output C to file."
     )
  ::("-ast-chop",
      Arg.Tuple
        (let srcbb = ref 0 and srcn = ref 0
         and trgbb = ref 0 and trgn = ref 0 in
         [Arg.Set_int srcbb ; Arg.Set_int srcn ;
          Arg.Set_int trgbb ; Arg.Set_int trgn ;
               uadd(TransformAstCfg(ast_chop srcbb srcn trgbb trgn)) ]),
     "<src-bbnum> <src-stmtnum> <trg-bbnum> <trg-stmtnum> Calculate the chop of an AST")
  ::("-ssa-chop",
      Arg.Tuple
        (let srcbb = ref 0 and srcn = ref 0
         and trgbb = ref 0 and trgn = ref 0 in
         [Arg.Set_int srcbb ; Arg.Set_int srcn ;
          Arg.Set_int trgbb ; Arg.Set_int trgn ;
               uadd(TransformSsa(ssa_chop srcbb srcn trgbb trgn)) ]),
     "<src-bbnum> <src-stmtnum> <trg-bbnum> <trg-stmtnum> Calculate the chop of an SSA")
  ::("-sccvn", uadd(TransformSsa sccvn),
     "Apply Strongly Connected Component based Value Numbering")
  ::("-deadcode", uadd(TransformSsa deadcode),
     "Perform dead code ellimination.")
  ::("-adeadcode", uadd(TransformSsa adeadcode),
     "Perform aggressive dead code ellimination.")
  ::("-coalesce-ast", uadd(TransformAstCfg ast_coalesce),
     "Perform coalescing on the AST.")
  ::("-coalesce-ssa", uadd(TransformSsa ssa_coalesce),
     "Perform coalescing on the SSA.")
  ::("-jumpelim", uadd(TransformSsa jumpelim),
     "Control flow optimization.")
  (* ::("-memtoscalar", uadd(AnalysisSsa memory2scalardef), *)
  (*    "Convert memory accesses to scalars (default mode).") *)
  (* ::("-memtoscalar-initro", uadd(AnalysisSsa memory2scalariroptir), *)
  (*    "Convert memory accesses to scalars (IndirectROPTIR mode).") *)
  ::("-simp-ssa", uadd(TransformSsa Ssa_simp.simp_cfg),
     "Perform all supported optimizations on SSA")
  ::("-single-stmt-ssa",
     uadd(TransformSsa Depgraphs.DDG_SSA.stmtlist_to_single_stmt),
     "Create new graph where every node has at most 1 SSA statement"
    )
  ::("-filter-specials", uadd(TransformAst Hacks.filter_specials),
     "Remove specials from AST program.")
  ::("-trace-cut", Arg.Int(fun i -> add(TransformAst(BatList.take i))),
     "<n>  Get the first <n> instructions of the trace")
  ::("-trace-concrete",
     uadd(TransformModeAst Traces.concrete),
     "Execute the trace concretely and obtain a straightline trace"
    )
  ::("-trace-concrete-subst",
     uadd(TransformModeAst Traces_surgical.concrete_substitution),
     "Execute the trace concretely and obtain a straightline trace"
    )
  ::("-trace-slice",
     uadd(TransformModeAst Traces_surgical.check_slice),
     "Slice a trace based on the overwritten return address"
    )
  ::("-trace-reconcrete",
     Arg.String(fun f -> add(TransformAst(Traces.concrete_rerun f))),
     "Execute a concretized trace with the specified input file."
    )
  ::("-trace-dce",
     uadd(TransformAst Traces.trace_dce),
     "Trace dead-code elimination."
    )
  ::("-trace-start-debug",
     Arg.Set_int(startdebug),
     "Start debugging at item n."
    )
  ::("-trace-debug",
     uadd(AnalysisModeAst Traces.TraceSymbolic.trace_valid_to_invalid ),
     "Formula debugging. Prints to files form_val and form_inv"
    )
  ::("-trace-conc-debug",
     Arg.Unit
       (fun () ->
	  let f = Traces.TraceSymbolic.formula_valid_to_invalid ~min:!startdebug in
	  add(AnalysisModeAst f)
       ),
     "Formula debugging. Prints to files form_val and form_inv. Concretizes BEFORE debugging; useful for finding which assertion doesn't work."
    )
  ::("-trace-dsa",
     uadd(TransformAst to_dsa),
     "Convert to DSA form.")
   ::("-trace-target",
     Arg.String (fun addr -> add(TransformModeAst(
       Traces.control_flow (Big_int_Z.big_int_of_string addr)))),
     "<addr> Provide the target address <addr>"
    )
   ::("-trace-symbolic-target",
     uadd(TransformModeAst Traces.limited_control),
     "Use a symbolic jump target (to determine the amount of control we have)"
    )
   ::("-trace-payload",
     Arg.String (fun p -> add(TransformModeAst(Traces.add_payload bi0 p))),
     "<binstring> Provide a payload to be inserted at the return address (BEWARE of null bytes)"
    )
   ::("-trace-payload-file",
     Arg.String (fun p -> add(TransformModeAst(Traces.add_payload_from_file bi0 p))),
     "<binfile> Provide a payload to be inserted at the return address"
    )
   ::("-trace-payload-after-file",
     Arg.String (fun p ->
       add(TransformModeAst(offset_helper Traces.add_payload_from_file_after p))),
      "<binfile> Provide a payload to be inserted past the return address"
    )
   ::("-trace-payload-after",
     Arg.String (fun p ->
       add(TransformModeAst(offset_helper Traces.add_payload_after p))),
     "<binstring> Provide a payload to be inserted past the return address (BEWARE of null bytes)"
    )
   ::("-trace-shell",
     Arg.Int (fun off -> add(TransformModeAst(Traces.inject_shellcode off))),
     "<nopsled> Insert shellcode with a nopsled of the given size"
    )
  ::("-trace-pivot",
     Arg.Tuple(
       let gaddr = ref bi0 in
       let maddr = ref bi0 in
       [
   	 Arg.String (fun a -> gaddr := Big_int_Z.big_int_of_string a);
  	 Arg.String (fun a -> maddr := Big_int_Z.big_int_of_string a);
  	 Arg.String (fun a -> add(TransformModeAst(Traces.add_pivot !gaddr !maddr a)));
       ]),
     "<gaddress> <maddress> <payload string> Use pivot at gaddress to transfer control to payload at maddress."
    )
  ::("-trace-pivot-file",
     Arg.Tuple(
       let gaddr = ref bi0 in
       let maddr = ref bi0 in
       [
   	 Arg.String (fun a -> gaddr := Big_int_Z.big_int_of_string a);
  	 Arg.String (fun a -> maddr := Big_int_Z.big_int_of_string a);
  	 Arg.String (fun a -> add(TransformModeAst(Traces.add_pivot_file !gaddr !maddr a)));
       ]),
     "<gaddress> <maddress> <payload string> Use pivot at gaddress to transfer control to payload at maddress."
    )
  ::("-trace-seh-pivot",
     Arg.Tuple(
       let gaddr = ref bi0 in
       let maddr = ref bi0 in
       let sehaddr = ref bi0 in
       [
  	 Arg.String (fun a -> gaddr := Big_int_Z.big_int_of_string a);
  	 Arg.String (fun a -> maddr := Big_int_Z.big_int_of_string a);
	 Arg.String (fun a -> sehaddr := Big_int_Z.big_int_of_string a);
  	 Arg.String (fun a -> add(TransformModeAst(Traces.add_seh_pivot !gaddr !sehaddr !maddr a)));
       ]),
     "<gaddress> <maddress> <sehaddress> <payload string> Use pivot at gaddress to transfer control (by overwriting SEH handler at sehaddress) to payload at maddress."
    )
  ::("-trace-seh-pivot-file",
     Arg.Tuple(
       let gaddr = ref bi0 in
       let maddr = ref bi0 in
       let sehaddr = ref bi0 in
       [
  	 Arg.String (fun a -> gaddr := Big_int_Z.big_int_of_string a);
  	 Arg.String (fun a -> maddr := Big_int_Z.big_int_of_string a);
	 Arg.String (fun a -> sehaddr := Big_int_Z.big_int_of_string a);
  	 Arg.String (fun a -> add(TransformModeAst(Traces.add_seh_pivot_file !gaddr !sehaddr !maddr a)));
       ]),
     "<gaddress> <maddress> <sehaddress> <payload file> Use pivot at gaddress to transfer control (by overwriting SEH handler at sehaddress) to payload at maddress."
    )
  ::("-trace-formula",
     Arg.String(fun f -> add(AnalysisModeAst(Traces.TraceSymbolic.generate_formula (f,!Solver.solver)))),
     "<file> Output a trace formula to <file>"
    )
  ::("-trace-solver", Arg.String Solver.set_solver,
     ("Use the specified solver for traces. Choices: " ^ Solver.solvers))
  ::("-trace-exploit",
     Arg.String(fun f -> add(AnalysisModeAst(Traces.TraceSymbolic.output_exploit (f,!Solver.solver)))),
     "<file> Output the exploit string to <file>"
    )
  ::("-trace-assignments",
     uadd(TransformAst(Traces.add_assignments)),
     "Explicitly assign the concrete values to the trace variables"
    )
  ::("-trace-length",
     uadd(AnalysisAst(Traces.trace_length)),
     "Output the length of the trace"
    )
  ::("-trace-no-padding",
     Arg.Unit(fun () -> Traces.padding := false),
     "Apply padding for symbolic unused bytes."
    )
  ::("-trace-symbolic-indices",
     Arg.Set Traces.allow_symbolic_indices,
     "Allow the existence of symbolic indices during formula generation"
    )
  ::("-trace-check",
     Arg.Set Traces.consistency_check,
     "Perform consistency checks"
    )
  ::("-trace-check-all",
     Arg.Set Traces.checkall,
     "Perform extra consistency checks possible when all instructions are logged"
    )
  ::("-trace-noopt",
     Arg.Clear Traces.dce,
     "Disable trace optimizations"
    )
  :: ("-normalize-mem", uadd(TransformAst Memory2array.coerce_prog),
      "Normalize memory accesses as array accesses")
  :: ("-prune-cfg",
      uadd(TransformAstCfg Prune_unreachable.prune_unreachable_ast),
      "Prune unreachable nodes from an AST CFG")
  :: ("-prune-ssa",
      uadd(TransformSsa Prune_unreachable.prune_unreachable_ssa),
      "Prune unreachable nodes from a SSA CFG")
  :: ("-unroll",
      Arg.Int (fun i -> add (TransformAstCfg(Unroll.unroll_loops ~count:i))),
      "<n> Unroll loops n times")
  :: ("-rm-cycles", uadd(TransformAstCfg Hacks.remove_cycles),
      "Remove cycles")
  :: ("-rm-indirect-ast", uadd(TransformAstCfg Hacks.ast_remove_indirect),
      "Remove BB_Indirect")
  :: ("-rm-indirect-ssa", uadd(TransformSsa Hacks.ssa_remove_indirect),
      "Remove BB_Indirect")
  :: ("-typecheck", uadd(AnalysisAst Typecheck.typecheck_prog),
      "Typecheck program")
  :: ("-uniqueify-labels", uadd(TransformAst Hacks.uniqueify_labels),
      "Ensure all labels are unique")
  :: ("-replace-unknowns", uadd(TransformAst Hacks.replace_unknowns),
      "Replace all unknowns with zeros")
  :: ("-bberror-assume-false", uadd(TransformAstCfg Hacks.bberror_assume_false),
      "Add an \"assume false\" statement to BB_Error and add an edge to BB_Exit")
  :: ("-flatten-mem", uadd(TransformAst Flatten_mem.flatten_mem_program),
      "Flatten memory accesses")
  :: ("-vsa", uadd(AnalysisModeAstCfg vsa_print),
      "Run value set analysis and print the results.")
  :: ("-simplify-conds", uadd(TransformAstCfg Ast_cond_simplify.simplifycond_cfg),
      "Simplify conditions")
  :: ("-no-ssa-tac", Arg.Unit (fun () -> tac := false),
     "Disable three address code in SSA representation.")
  :: ("-usedef", uadd(AnalysisAstCfg usedef),
      "Compute and print use def chains")
  :: ("-defuse", uadd(AnalysisAstCfg defuse),
      "Compute and print def use chains")
  :: ("-ret2jmp", uadd(TransformAst Hacks.ret_to_jmp),
      "Hack to replace returns with a jump to the end of program.")
  :: Input.speclist

let () = Tunegc.set_gc ()
let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let pipeline = List.rev !pipeline

let prog, scope, arch =
  try Input.get_program ()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let rec apply_cmd prog = function
  | AnalysisAst f -> (
    match prog with
    | Ast p as p' -> f p; p'
    | _ -> failwith "need explicit translation to AST"
  )
  | AnalysisModeAst f -> (
    match prog with
    | Ast p as p' -> f (Input.get_arch arch) p; p'
    | _ -> failwith "need explicit translation to AST"
  )
  | AnalysisAstCfg f -> (
    match prog with
    | AstCfg p as p' -> f p; p'
    | _ -> failwith "need explicit translation to AST CFG"
  )
  | AnalysisModeAstCfg f -> (
    match prog with
    | AstCfg p as p' -> f (Input.get_arch arch) p; p'
    | _ -> failwith "need explicit translation to AST CFG"
  )
  | AnalysisSsa f -> (
    match prog with
    | Ssa p as p' -> f p; p'
    | _ -> failwith "need explicit translation to SSA"
  )
  | TransformAst f -> (
    match prog with
    | Ast p -> Ast(f p)
    | _ -> failwith "need explicit translation to AST"
  )
  | TransformModeAst f -> (
    match prog with
    | Ast p -> Ast(f (Input.get_arch arch) p)
    | _ -> failwith "need explicit translation to AST"
  )
  | TransformAstCfg f -> (
    match prog with
    | AstCfg p -> AstCfg(f p)
    | _ -> failwith "need explicit translation to AST CFG"
  )
  | TransformSsa f -> (
    match prog with
    | Ssa p -> Ssa(f p)
    | _ -> failwith "need explicit translation to SSA"
  )
  | ToCfg -> (
    match prog with
    | Ast p -> AstCfg(Cfg_ast.of_prog p)
    | Ssa p -> AstCfg(Cfg_ssa.to_astcfg p)
    | AstCfg _ as p -> prerr_endline "Warning: null transformation"; p
  )
  | ToAst -> (
    match prog with
    | AstCfg p -> Ast(Cfg_ast.to_prog p)
    | p -> apply_cmd (apply_cmd p ToCfg) ToAst
  )
  | ToSsa -> (
    match prog with
    | AstCfg p -> Ssa(Cfg_ssa.of_astcfg ~tac:!tac p)
    | p -> apply_cmd (apply_cmd p ToCfg) ToSsa
  )
;;

List.fold_left apply_cmd (Ast prog) pipeline
