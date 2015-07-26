open BatListFull
open Big_int_convenience

let bfd_target = ref None
let init_ro = ref false
let inputs = ref []
and streaminputs = ref None
and streamrate = ref 10000L (* Unless specified grab this many frames at a time *)
and pintrace = ref false

let typecheck = ref true

let toint64 s =
  try Int64.of_string s
  with Failure "int_of_string" -> raise(Arg.Bad("invalid int64: "^s))

let tobigint s =
  try Big_int_Z.big_int_of_string s
  with Failure _ -> raise(Arg.Bad("invalid big_int: "^s))

let setint64 r s = r := toint64 s

let setbigint r s = r := tobigint s

let stream_speclist =
  (* let addinput i = streaminputs := i :: !streaminputs in *)
  [
    ("-rate",
     Arg.String(setint64 streamrate), "<rate> Stream at rate frames");
    ("-tracestream",
     Arg.String(fun s ->
       streaminputs := Some(`Tracestream s)),
     "<file> Read a trace to be processed as a stream.");
  ]

let addinput i = inputs := i :: !inputs

let trace_speclist =
[
    ("-trace",
     Arg.String(fun s ->
       addinput (`Trace s)),
     "<file> Read in a trace and lift it to the IL");
]

let speclist =
  [
    ("-init-ro", Arg.Set init_ro, "Access rodata.");
    ("-bfd-target", Arg.String
      (fun s -> bfd_target := Some s),
     "Set BFD target architecture.");
    ("-bin",
     Arg.String(fun s -> addinput (`Bin s)),
     "<file> Convert a binary to the IL");
    ("-binrange",
     Arg.Tuple(let f = ref ""
               and s = ref (bi 0) in
               [Arg.Set_string f; Arg.String(setbigint s);
                Arg.String(fun e->addinput(`Binrange(!f, !s, tobigint e)))]),
     "<file> <start> <end> Convert the given range of a binary to the IL");
    ("-binrecurse",
     Arg.String(fun s -> addinput (`Binrecurse s)),
     "<file> Lift binary to the IL using a recursive descent algorithm.");
    ("-binrecurseat",
     Arg.Tuple(let f = ref "" in
               [Arg.Set_string f;
                Arg.String (fun s -> addinput (`Binrecurseat (!f, tobigint s)))]),
     "<file> <start> Lift binary to the IL using a recursive descent algorithm starting at <start>.");
    ("-il",
     Arg.String(fun s -> addinput (`Il s)),
     "<file> Read input from an IL file.");
  ] @ trace_speclist

let get_program () =
  if !inputs = [] then raise(Arg.Bad "No input specified");
  let open_program f = Asmir.open_program ?target:!bfd_target f in
  let get_one (oldp,oldscope,_) = function
    | `Il f ->
      let newp, newscope = Parser.program_from_file ~scope:oldscope f in
      List.append newp oldp, newscope, None
    | `Bin f ->
      let p = open_program f in
      let arch = Asmir.get_asmprogram_arch p in
      List.append (Asmir.asmprogram_to_bap ~init_ro:!init_ro p) oldp, oldscope, Some arch
    | `Binrange (f, s, e) ->
      let p = open_program f in
      let arch = Asmir.get_asmprogram_arch p in
      List.append (Asmir.asmprogram_to_bap_range ~init_ro:!init_ro p s e) oldp, oldscope, Some arch
    | `Binrecurse f ->
      let p = open_program f in
      let arch = Asmir.get_asmprogram_arch p in
      List.append (fst (Asmir_rdisasm.rdisasm p)) oldp, oldscope, Some arch
    | `Binrecurseat (f, s) ->
      let p = open_program f in
      let arch = Asmir.get_asmprogram_arch p in
      List.append (fst (Asmir_rdisasm.rdisasm_at p [s])) oldp, oldscope, Some arch
    | `Trace f ->
      let prog, arch = Asmir.serialized_bap_from_trace_file f in
      List.append prog oldp, oldscope, Some arch
  in
  try
    let p,scope,arch = List.fold_left get_one ([], Grammar_private_scope.default_scope (), None) (List.rev !inputs) in
    (* Always typecheck input programs. *)
    if !typecheck then Printexc.print Typecheck.typecheck_prog p;
    p,scope,arch
  with e ->
    Printf.eprintf "Exception %s occurred while lifting\n" (Printexc.to_string e);
    raise e

let get_stream_program () = match !streaminputs with
  | None -> raise(Arg.Bad "No streaming input specified")
  | Some(`Tracestream f) ->
    let closething, stream, arch = Asmir.serialized_bap_stream_from_trace_file !streamrate f in
    stream, Some arch

let get_arch = function
  | Some a -> a
  | None -> raise (Invalid_argument "Tried to get program architecture for IL")
