let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

(* XXX: How can we allow each element in the pipeline to maintain its
   own state between blocks?  The problem is each element might need a
   different type.  If we could have polymorphic arrays/lists, that would
   work.  *)

open Arch
open BatListFull
open Utils_common

module VH = Var.VarHash

type ast = Ast.program
(*type astcfg = Cfg.AST.G.t
  type ssa = Cfg.SSA.G.t*)

type prog =
  | Ast of ast

type cmd =
  | AnalysisAst of (ast -> unit)
  | AnalysisModeAst of (arch -> ast -> unit)
  | TransformAst of (ast -> ast)
  | TransformModeAst of (arch -> ast -> ast)

let pipeline = ref [];;

let final = ref [];;

let add c =
  pipeline := c :: !pipeline

let addfinal c =
  final := c :: !final

let uadd c =
  Arg.Unit(fun()-> add c)

(** Prints the block *)
let prints f =
  let oc = open_out f in
  let pp = new Pp.pp_oc oc in
  (fun block ->
    pp#ast_program block;
    block)

let speclist =
  ("-pp-ast", Arg.String(fun f ->
    add(TransformAst(prints f));
   ),
   "<file> Print each statement in the trace to file.")
  ::("-trace-check",
     Arg.Set Traces.consistency_check,
     "Perform consistency checks"
    )
  ::("-trace-check-all",
     Arg.Unit (fun () -> Traces.checkall := true;
       Traces.consistency_check := true),
     "Perform extra consistency checks possible when all instructions are logged"
    )
  ::("-trace-concrete",
     uadd(TransformModeAst(Traces_stream.concrete true)),
     "Concretely execute, and passes on concretized IL to next analysis.")
  ::("-trace-concrete-drop",
     uadd(TransformModeAst(Traces_stream.concrete false)),
     "Concretely execute and do not pass on concretized IL to next analysis.")
  ::("-trace-formula",
     Arg.String(fun f ->
       let stream, final = Traces_stream.generate_formula f !Solver.solver in
       add(AnalysisModeAst stream);
       addfinal(final)
     ),
     "<file> Generate and output a trace formula to <file>.")
  ::("-trace-solver", Arg.String Solver.set_solver,
     ("Use the specified solver for traces. Choices: " ^ Solver.solvers))
  :: Input.stream_speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let pipeline = List.rev !pipeline
let final = List.rev !final

let prog, arch =
  try Input.get_stream_program ()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1
;;

let apply_arch = function
  | AnalysisModeAst f -> AnalysisAst (f (Input.get_arch arch))
  | TransformModeAst f -> TransformAst (f (Input.get_arch arch))
  | (AnalysisAst _ | TransformAst _) as x -> x

let pipeline = List.map apply_arch pipeline;;

let rec apply_cmd prog = function
  | AnalysisAst f -> (
    match prog with
    | Ast p as p' -> f p; p'
  )
  | TransformAst f -> (
    match prog with
    | Ast p -> Ast(f p)
  )
  | AnalysisModeAst _ | TransformModeAst _ ->
    failwith "impossible"
;;

Stream.iter
  (fun block ->
    ignore(List.fold_left apply_cmd (Ast block) pipeline)
  ) prog;

List.iter (fun f -> f ()) final;;
