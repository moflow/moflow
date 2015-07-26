open Arch
open Big_int_convenience
open OUnit
open Pcre
open Test_common
open Traces_backtaint
open Type

let bof = "C/bof1";;
let taint_file = "tainted_file";;
let exploit_file = "exploit";;
let tag = "pin_suite";;


let create_input_file _ =
  let out = open_out taint_file in
  output_string out "helloooooooooooooooooooooooooooooooo\n";
  close_out out;;


let pin_trace_setup arch () =
  let bof = match arch with
    | X86_32 -> bof
    | X86_64 -> bof^"_64"
  in
  create_input_file();
  let pintool_args = ["-taint-files"; taint_file] in
  run_trace ~tag ~arch ~pintool_args bof [taint_file]

module MakeTraceTest(TraceSymbolic:Traces.TraceSymbolic) = struct
  let pin_trace_test pin_out =
    let prog, arch = Asmir.serialized_bap_from_trace_file pin_out in
    typecheck prog;
    Traces.consistency_check := true;
    ignore(Traces.concrete arch prog);
    Traces.consistency_check := false;
    let t1 = Traces.add_payload bi0 "test" arch prog in
    (* We should not get an exception because this should be satisfiable *)
    Traces.TraceSymbolic.output_exploit (exploit_file,Smtexec.STP.si) arch t1;
    let t2 = Traces.add_payload bi0 "\x00" arch prog in
    (* Null bytes are not allowed, so we should get an exception *)
    (* We need to cleanup traces in between runs, or we'll get an
       error. *)
    Traces.cleanup();
    let unsat =
      try
        Traces.TraceSymbolic.output_exploit (exploit_file,Smtexec.STP.si) arch t2;
        false
      with Failure "Formula was unsatisfiable"
      | Failure "No model found" -> true
    in
    assert_equal ~msg:"Exploit should be impossible" unsat true
end

let pin_stream_trace_test solver pin_out =
  skip_if (not (solver#in_path ())) (solver#solvername ^ " not on path");
  let open Traces.TraceSymbolicStream in
  let closething, stream, arch = Asmir.serialized_bap_stream_from_trace_file !Input.streamrate pin_out in
  let streamf, finalf = Traces_stream.generate_formula formula_storage solver in
  Stream.iter (streamf arch) stream;
  finalf ();
  match solver#solve_formula_file ~getmodel:true formula_storage with
  | Smtexec.Invalid m ->
    (try parse_answer_to m exploit_file
     with Failure "No model found" ->
       skip_if true ("Model parsing of "^solver#solvername^" not complete"))
  | Smtexec.Valid -> assert_failure "Trace should be satisfiable but is unsatisfiable"
  | _ ->
    skip_if (solver == Smtexec.STPSMTLIB.si) "STP has a bug right now: https://groups.google.com/d/topic/stp-users/OVXDFyCgTuY/discussion";
    assert_failure "An error occured while solving the formula"

let backwards_taint_test pin_out =
  Traces.cleanup();
  let prog, arch = Asmir.serialized_bap_from_trace_file pin_out in
  typecheck prog;
  let input_locations = Test_common.backwards_taint arch prog in
  (* The buffer is eight bytes large, so make sure all bytes are
     coming from after that.  This isn't exact, since copying eight bytes
     at a time (XMM register) could make us off by seven bytes, but that
     seems unlikely... *)
  assert_bool "Early symbolic bytes affect crash"
    (LocSet.for_all
       (function
         | Loc.V v ->
           (try
             let n = Traces.get_symb_num v in
             n > 8 && n < 30
            with _ -> assert_failure "Unable to find symbolic byte number")
         | Loc.M _ -> assert_failure "Expected only symbolic bytes to influence crash")
       input_locations);

  let n = LocSet.cardinal input_locations in
  Printf.printf "n = %d\n" n;
  assert_bool "There must be at least one and less than sixteen bytes causing the crash" (n >= 1 && n <= 16)

let backwards_taint_test x =
  try backwards_taint_test x
  with _ -> skip_if true "Fix backwards_taint_test. Issue #17"

(* Note: This will leave the files pin.log and pintool.log by intention *)
let pin_trace_cleanup pin_out =
  rm_and_ignore_list [pin_out ; exploit_file ; taint_file];
  Traces.cleanup()
;;

let exploit_test trace =
  let trace, arch = Asmir.serialized_bap_from_trace_file trace in
  let t = Arch.type_of_arch arch in
  let addr = Arithmetic.to_big_int (bim1, t) in
  let trace = Traces.control_flow addr arch trace in
  let trace = Traces.add_payload_after bi0 "hello" arch trace in
  let () = Traces.TraceSymbolic.output_exploit (exploit_file,Smtexec.STP.si) arch trace in
  let buf = String.create 1000 in
  let ic = open_in exploit_file in
  (try really_input ic buf 0 1000
   with End_of_file -> ());
  assert_bool "Exploit does not contain the correct string" (BatString.exists buf "\xff\xff\xff\xffhello")

let fold_solvers (s,f) =
  List.map (fun solver ->
    s^"_"^solver#solvername >:: f solver
  ) (Util.get_hash_values Smtexec.solvers)

let suite arch =
  let pin_trace_setup = pin_trace_setup arch in
  let suffix = match arch with X86_32 -> "_32" | X86_64 -> "_64" in
  "Traces" >:::
  [
    (* We record the same trace multiple times, which is kind of dumb *)
    "backwards_taint_test"^suffix >::
      bracket pin_trace_setup backwards_taint_test pin_trace_cleanup;
    "pin_trace_test"^suffix >::
      (let module M = MakeTraceTest(Traces.TraceSymbolic) in
       bracket pin_trace_setup M.pin_trace_test pin_trace_cleanup);
    "pin_trace_letbind_test"^suffix >::
      (let module M = MakeTraceTest(Traces.TraceSymbolicStream) in
       bracket pin_trace_setup M.pin_trace_test pin_trace_cleanup);
    "exploit_test"^suffix >::
      bracket pin_trace_setup exploit_test pin_trace_cleanup;
  ] @
  fold_solvers ("pin_trace_stream_test",
                (fun solver ->
                  bracket pin_trace_setup (pin_stream_trace_test solver) pin_trace_cleanup))
