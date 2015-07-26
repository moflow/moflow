open Ast
open Ast_convenience
open Big_int_convenience
module CA = Cfg.AST
module CS = Cfg.SSA
module D = Debug.Make(struct let name = "Asmir_disasm" and default=`NoDebug end)
open D
open Type
module VM = Var.VarMap

(* XXX: Handle conditional function calls *)
(* VSA dataflow reuse *)
(* Call/ret behavior *)
(* Reprocess indirect jumps *)

(* Raise an exception instead of adding an indirect edge *)
let no_indirect = ref true

(* If VSA fails to read the value of a jump table, try to read the
   value at BB_Entry.  This hack is important in practice because a
   function that writes to an input pointer could conservatively write to
   any memory location (including the jump table).

   XXX: Eventually we should confine this hack to only look at
   read-only memory.
*)
let vsa_mem_hack = ref true

type succs = | Addrs of label list
             | Error
             | Exit
             | Indirect

module type STATE = sig
  type t
  val init : t
end

module NOOPSTATE = struct
  type t = unit
  let init = ()
end

module type FUNCID = sig
  module State : STATE
  val find_calls : Cfg.AST.G.t -> Cfg.AST.G.V.t list -> Cfg_ast.unresolved_edge list -> State.t -> Cfg_ast.unresolved_edge list * State.t
  val find_rets : Cfg.AST.G.t -> Cfg.AST.G.V.t list -> Cfg_ast.unresolved_edge list -> State.t -> Cfg_ast.unresolved_edge list * State.t
end

module FUNCFINDER_DUMB = struct
  module State = NOOPSTATE
  let is_call_stmt = function
    | Jmp(_, [StrAttr "call"]) -> true
    | _ -> false
  let is_ret_stmt = function
    | Jmp(_, attrs)
    (* Check for a 'ret' instruction that has been translated into a
       comment by dumb_translate below. *)
    | Comment(_, attrs) when List.mem (StrAttr "ret") attrs -> true
    | _ -> false
  let check_last f c nodes unresolved_edges () =
    List.filter (fun (v,_,_) -> let stmts = CA.get_stmts c v in
                                match List.rev stmts with
                                | last::_ -> f last
                                | [] -> false) unresolved_edges, ()
  let find_calls = check_last is_call_stmt
  let find_rets = check_last is_ret_stmt
end

module type DISASM = sig
  module State : STATE
  val get_succs : Asmir.asmprogram -> Cfg.AST.G.t -> Cfg_ast.unresolved_edge list -> State.t -> (Cfg_ast.unresolved_edge * succs) list * State.t
  (** Function that returns the successors of one or more nodes in the
      unresolved list. *)

  val fixpoint : bool
  (** Should [get_succs] be called until a fixpoint is reached? *)
end

module RECURSIVE_DESCENT_SPEC = struct
  module State = struct
    type t = unit
    let init = ()
  end
  let get_succs_int no_indirect _asmp g edges () =
    let resolve_edge ((v,l,e) as edge) =
      let o = match List.rev (CA.get_stmts g v), l with
        | last::_, None when FUNCFINDER_DUMB.is_ret_stmt last ->
          Exit
        | CJmp _::_, Some _ ->
          (match lab_of_exp e with
          | Some l -> Addrs [l]
          | None -> Indirect)
        | CJmp _::_, _ -> failwith "error"
        (* Fallthrough/Jmp *)
        | _::_, None ->
          (match lab_of_exp e with
          | Some l -> Addrs [l]
          | None ->
            if no_indirect
            then failwith "Indirect jump encountered by recursive descent"
            else Indirect)
        | _::_, Some _ -> failwith "error"
        | [], _ -> Addrs []
      in
      (edge, o)
    in
    List.map resolve_edge edges, ()
  let get_succs asmp g e () = get_succs_int !no_indirect asmp g e ()

  let fixpoint = false
end

type vsaresult = {origssa: CS.G.t;
                  optssa: CS.G.t;
                  vsa_in: Cfg.ssastmtloc -> Vsa_ssa.AbsEnv.t option;
                  vsa_out: Cfg.ssastmtloc -> Vsa_ssa.AbsEnv.t option;}

module VSA_SPEC = struct
  module State = struct
    type t = vsaresult option
    let init = None
  end

  let jumpe g v =
    match List.rev (CS.get_stmts g v) with
    | Ssa.Jmp(e, _)::_ -> e
    | _ -> failwith (Printf.sprintf "jumpe: Unable to find jump at %s" (Cfg_ssa.v2s v))

  let get_succs asmp g edges st =
    let rdresolved, rdunresolved =
      let rd, () = RECURSIVE_DESCENT_SPEC.get_succs_int false asmp g edges () in
    List.partition (function (_,Indirect) -> false | _ -> true) rd
    in
    (* If RD resolves any edges, return these.  We want to make as
       much progress as possible before running VSA. *)
    match rdresolved, rdunresolved with
    | _::_, _ ->
      rdresolved, st
    | [], ((_::_) as indirect_edges) ->

      let edges = List.map (fun (e,x) -> assert (x = Indirect); e) indirect_edges in

      let cfg = Hacks.ast_exit_indirect (CA.copy g) in
      (* XXX: Use a real calling convention to avoid filtering all
         calls *)
      let cfg = Hacks.filter_calls_cfg cfg in
      let cfg = Prune_unreachable.prune_unreachable_ast cfg in
      let cfg = Hacks.add_sink_exit cfg in
      (* Cfg_pp.AstStmtsDot.output_graph (open_out "vsacfg.dot") cfg; *)

      (* Start by converting to SSA three address code. *)
      let ssacfg = Cfg_ssa.of_astcfg ~tac:true cfg in
      let origssacfg = ssacfg in
      let ssacfg =
        let vs = List.map (fun (v,_,_) -> CS.find_vertex ssacfg (CA.G.V.label v)) edges in
        Vsa_ssa.prepare_ssa_indirect ~vs ssacfg
      in

      (* Cfg_pp.SsaStmtsDot.output_graph (open_out "vsapre.dot") ssacfg; *)

      (* (\* Do an initial optimization pass.  This is important so that *)
      (*    simplifycond_ssa can recognize syntactically equal *)
      (*    computations. *\) *)
      (* let ssacfg = Ssa_simp.simp_cfg ssacfg in *)

      (* (\* Simplify the SSA conditions so they can be parsed by VSA *\) *)

      (* (\* Get ssa expression and vertices *\) *)
      (* let ssaves = List.map get_ssaev edges in *)
      (* let ssavs = List.map (function (_,v,_) -> v) ssaves in *)
      (* let ssaes = List.map (function (_,_,e) -> e) ssaves in *)
      (* if debug () then List.iter (fun (_,ssav,ssae) -> dprintf "ssavs %s %s" (Cfg_ssa.v2s ssav) (Pp.ssa_exp_to_string ssae)) ssaves; *)
      (* let ssacfg = Ssa_cond_simplify.simplifycond_targets_ssa ssaes ssacfg in *)
      (* (\* Cfg_pp.SsaStmtsDot.output_graph (open_out "vsacond.dot") ssacfg; *\) *)

      (* (\* Redo TAC so that we can simplify the SSA conditions. This *)
      (*    should ensure that all variables are their canonical form.  This *)
      (*    is important so that the edge conditions are consistent with the *)
      (*    rest of the program. *\) *)
      (* let ssacfg = Cfg_ssa.do_tac_ssacfg ssacfg in *)
      (* (\* Cfg_pp.SsaStmtsDot.output_graph (open_out "vsatac.dot") ssacfg; *\) *)

      (* (\* Simplify. *\) *)
      (* let ssacfg = Ssa_simp.simp_cfg ssacfg in *)
      (* (\* Cfg_pp.SsaStmtsDot.output_graph (open_out "vsasimp.dot") ssacfg; *\) *)

      (* (\* Now our edge conditions look like (Var temp).  We need to use *)
      (*    shadow copy propagation to convert them to something like (EAX *)
      (*    < 10). *\) *)

      (* (\* XXX: Should this go elsewhere? *\) *)
      (* let fix_edges g = *)
      (*   let _, m, _ = Copy_prop.copyprop_ssa g in *)
      (*   CS.G.fold_edges_e *)
      (*     (fun e g -> *)
      (*       match CS.G.E.label e with *)
      (*       | None -> g *)
      (*       | Some(b, Ssa.BinOp(EQ, Ssa.Var v, e2)) -> *)
      (*         (try let cond = Some(b, Ssa.BinOp(EQ, VM.find v m, e2)) in *)
      (*              let src = CS.G.E.src e in *)
      (*              let dst = CS.G.E.dst e in *)
      (*              let e' = CS.G.E.create src cond dst in *)
      (*              CS.add_edge_e (CS.remove_edge_e g e) e' *)
      (*          with Not_found -> g) *)
      (*       | Some(_, e) -> (\* Sometimes we might see a constant like true/false *\) g *)
      (*     ) g g *)
      (* in *)

      (* let ssacfg = fix_edges ssacfg in *)

      (* let ssacfg = Coalesce.coalesce_ssa ~nocoalesce:ssavs ssacfg in *)
      (* Cfg_pp.SsaStmtsDot.output_graph (open_out "vsa.dot") ssacfg; *)

      dprintf "Starting VSA now";
      let opts = Vsa_ssa.build_default_prog_options asmp in
      let df_in, df_out = Vsa_ssa.vsa ~nmeets:0 opts ssacfg in

      let resolve_edge ((v,l,e) as edge) =

        (* Do VSA stuff *)
        dprintf "Resolving %s with VSA" (Pp.ast_exp_to_string e);
        if l <> None then failwith "VSA-enabled lifting currently assumes that conditional jumps are not indirect";
        let get_ssaev ((v,_,_) as edge) =
          let ssav = CS.find_vertex ssacfg (CA.G.V.label v) in
          let ssae = jumpe ssacfg ssav in
          (edge, ssav, ssae)
        in
        let (_, ssav, ssae) = get_ssaev edge in
        dprintf "ssae: %s" (Pp.ssa_exp_to_string ssae);
        let ssaloc = Vsa_ssa.last_loc ssacfg ssav in

        let _, m, cp =
          let stop_before = function
            | Ssa.Load _ | Ssa.Store _ -> true
            | _ -> false
          in
          let stop_after = function
            | Ssa.Load _ -> true
            | _ -> false
          in
          Copy_prop.copyprop_ssa ~stop_before ~stop_after ssacfg
        in

        let add_indirect edge =
          if debug () then (
            Printf.eprintf "VSA @%s" (Cfg_ssa.v2s ssav);
            Vsa_ssa.AbsEnv.pp prerr_string (BatOption.get (df_out (Vsa_ssa.last_loc ssacfg ssav)));
            dprintf "\n\n"
          );
          if !no_indirect
          then failwith "VSA disassembly failed to resolve an indirect jump to a specific concrete set"
          else (edge, Indirect)
        in

        let fallback edge =
          dprintf "fallback";
          let vs = Vsa_ssa.exp2vs (BatOption.get (df_out (Vsa_ssa.last_loc ssacfg ssav))) ssae in
          dprintf "VSA resolved %s to %s" (Pp.ast_exp_to_string e) (Vsa_ssa.VS.to_string vs);
          (match Vsa_ssa.VS.concrete ~max:1024 vs with
          | Some x -> dprintf "VSA finished";
            (edge, Addrs (List.map (fun a -> Addr a) x))
          | None -> wprintf "VSA disassembly failed to resolve %s/%s to a specific concrete set" (Pp.ast_exp_to_string e) (Vsa_ssa.VS.to_string vs);
            add_indirect edge)
        in

        (* Value sets are very good at representing the addresses that
           point to the elements in a jump table.  For instance,
           jump_table + 4*index is a value set.  However, there is no
           guarantee that the address stored *in* the jump table,
           i.e. M[jump_table + 4*index] are aligned.  To ameliaorate
           this fact, we can apply copy propagation to the jump
           expression, to see if we get something like [Jump Load(e)].
           If we do, we can enumerate the addresses in [e], and then put
           the values [M[e]] in a set.  This is basically what VSA will
           do, except that it will also convert M[e] to a value set
           before it returns, which introduced imprecision. *)

        let special_memory m indexe endian t =
          (* The variable copy propagates to a memory load.
             Perfect.  This is looking like a jump table lookup. *)

          dprintf "special_memory %s %s cp %s" (Pp.ssa_exp_to_string m) (Pp.ssa_exp_to_string indexe) (Pp.ssa_exp_to_string (cp indexe));
          let l = BatOption.get (df_in ssaloc) in
          let exp2vs = Vsa_ssa.exp2vs l in
          let vs = exp2vs indexe in
          dprintf "VSA resolved memory index %s to %s" (Pp.ssa_exp_to_string indexe) (Vsa_ssa.VS.to_string (Vsa_ssa.exp2vs l indexe));
          (match Vsa_ssa.VS.concrete ~max:1024 vs with
          | Some l -> dprintf "VSA finished";
            (* We got some concrete values for the index.  Now
               let's do an abstract load for each index, and try to
               get concrete numbers for that. *)

            let do_read meme loc =
              let reads = List.map (fun a ->
                let exp2vs = Vsa_ssa.exp2vs (BatOption.get (df_in loc)) in
                let exp = Ssa.Load(meme, Ssa.Int(a, Typecheck.infer_ssa indexe), endian, t) in
                let vs = exp2vs exp in
                dprintf "memory %s %s" (Pp.ssa_exp_to_string exp) (Vsa_ssa.VS.to_string vs);
                let conc = Vsa_ssa.VS.concrete ~max:1024 vs in
                match conc with
                | Some l -> l
                | None ->
                  failwith (Printf.sprintf "Unable to read from jump table at %s" (~% a))
              ) l
              in
              (edge, Addrs (List.map (fun a -> Addr a) (List.flatten reads)))
            in
            (try do_read m ssaloc with
            | e when !vsa_mem_hack ->
              (* Hack: We couldn't read the jump table, so try again at
                 BB_Entry.  The underlying assumption is that the memory
                 should be read only, so it couldn't have changed. *)
              dprintf "VSA mem hack";
              do_read (Ssa.Var (Arch.mem_of_arch (Asmir.get_asmprogram_arch asmp))) (CS.G.V.create Cfg.BB_Entry, 0)
            | e ->
              add_indirect edge
            )
          | None -> wprintf "VSA disassembly failed to resolve %s/%s to a specific concrete set" (Pp.ssa_exp_to_string indexe) (Vsa_ssa.VS.to_string vs);
            add_indirect edge)
        in

        (match ssae with
        | Ssa.Var v ->
          (try match VM.find v m with
          | Ssa.Load(m, e, endian, t) ->
            special_memory m e endian t
          | _ -> fallback edge
           with Not_found -> fallback edge)
        (* | Ssa.Load(m, e, endian, t) -> *)
        (*   special_memory m e endian t *)
        | _ -> fallback edge)
      in

      (* There are a bunch of edges to resolve. For now we assume we
         can resolve all of them at once. *)
      List.map resolve_edge edges,
      let open State in
      Some {origssa=origssacfg;
            optssa=ssacfg;
            vsa_in=df_in;
            vsa_out=df_out;}

    | [], [] -> failwith "empty worklist"

  let fixpoint = true
end

module Make(D:DISASM)(F:FUNCID) = struct
  let disasm_at ?callsig:du p addr =
    let (tmp, entry) = Cfg_ast.create_entry (CA.empty()) in
    let (tmp, exit) = Cfg_ast.find_exit tmp in
    let (tmp, error) = Cfg_ast.find_error tmp in
    let (c, indirect) = Cfg_ast.find_indirect tmp in
    let c = CA.add_edge c indirect error in (* indirect jumps could fail *)

    (* Store the unresolved edges outgoing from each address.  This is
       used to implement propagate_edge. *)
    let edgeh = Hashtbl.create 1000 in

    let iteration (init_worklist : Cfg_ast.unresolved_edge list) (c,state) =
      let c = ref c in
      let state = ref state in
      let q = Worklist.create () in
      (* bbs of lifted insruction, unresolved outgoing edges from lifted instruction, successor address *)
      let () = Worklist.add_list init_worklist q in
      let raise_address c a =
        dprintf "raise_address %s" (~% a);
        try c, CA.find_label c (Addr a)
        with Not_found ->
          let (prog, next) = Asmir.asm_addr_to_bap p a in
          if Asmir.bap_fully_modeled prog = false then raise Asmir.Memory_error;

          let (c', edges, bbs, fallthrough) = Cfg_ast.add_prog c prog in

          (* Resolve edges internal to this instruction.  All internal
             edges will be to Name labels (instead of Addr labels).

             We need to add internal edges here so we can actually do
             analysis on the CFG (for example, concretely executing to
             see if there is a call).
          *)
          let c', edges = List.fold_left
            (fun (c,unresolved_edges) ((s,l,e) as edge) ->
              match lab_of_exp e with
              | Some (Name _ as t) -> CA.add_edge_e c (CA.G.E.create s l (CA.find_label c t)), unresolved_edges
              | _ -> c, edge::unresolved_edges
            ) (c',[]) edges in

          (* Worklist edges *)
          let edges = match fallthrough with
            | Some v -> (v,None,exp_of_lab (Addr next))::edges
            | None -> edges
          in

          (* Rewrite any calls or rets from this address *)
          let c', edges = match
            F.find_calls c' bbs edges F.State.init,
            F.find_rets c' bbs edges F.State.init
            with
            | ([], _), ([], _) -> c', edges
            | (_::[] as call_edges, _), ([] as ret_edges, _)
            | ([] as call_edges, _), (_::[] as ret_edges, _) ->
              let special_edges, typ = match call_edges, ret_edges with
                | x::_, [] -> call_edges, `Call
                | [], x::_ -> ret_edges, `Ret
                | _ -> failwith "impossible"
              in
              (* We found edges corresponding to calls or returns *)
              let others = Util.list_difference edges special_edges in
              let fallthrough = match typ with
                | `Call -> List.map (fun (s,l,e) -> (s,l, exp_of_lab (Addr next))) special_edges
                | `Ret -> special_edges
              in
              let dumb_translate cfg (s,l,e) =
                let revstmts = match List.rev (CA.get_stmts cfg s) with
                  | CJmp _::_ -> failwith "Conditional function calls are not implemented"
                  | Jmp (te,_)::tl as stmts ->
                    let comments = List.map (function
                      | Label _ as s -> s
                      | Jmp(e, attrs) as s ->
                        Comment(Printf.sprintf "Function call/ret removed: %s" (Pp.ast_stmt_to_string s), NamedStrAttr("calltarget", Pp.ast_exp_to_string e)::attrs)
                      | s ->
                        Comment(Printf.sprintf "Function call/ret removed: %s" (Pp.ast_stmt_to_string s), [])) stmts
                    in
                    let tgt = (match te with
                      | Int (a,_) -> [Target (Addr a)]
                      | Lab s -> [Target (Name s)]
                      | _ -> [])
                    in
                    (match typ with
                    | `Call ->
                      Special("function call", du, tgt)::comments
                    | `Ret -> comments)
                  | _ -> failwith "Unable to rewrite function call"
                in
                CA.set_stmts cfg s (List.rev revstmts)
              in
              let c' = List.fold_left dumb_translate c' special_edges in
              c', BatList.append others fallthrough
            | _ -> failwith "both call and ret edges found"
          in

          Hashtbl.replace edgeh a (bbs, edges, next);
          Worklist.add_list edges q;
          c', CA.find_label c' (Addr a)
      in
      (* addcond: Add condition to edge?
         (s,l,e): original edge information
         c: graph
         lbl: resolved edge destination
      *)
      let add_resolved_edge addcond (s,l,e) c lbl  =
        try
          let c, dst = match lbl with
            | Addr a -> raise_address c a
            | Name s -> failwith "add_resolved_edge: Named labels should be resolved in raise_address"
          in
          let l' = match addcond, lbl with
            | true, Addr a ->
              if l <> None then failwith "add_resolved_edge: Indirect conditional jumps are unimplemented";
              Some(None, binop EQ e (Int(a, Typecheck.infer_ast e)))
            | true, Name _ -> failwith "add_resolved_edge: It is not possible to resolve indirect jump to a named label"
            | false, _ -> l
          in

          CA.add_edge_e c (CA.G.E.create s l' dst)
        with Asmir.Memory_error ->
          (* Should this go to error or exit? *)
          wprintf "Raising address %s failed" (Pp.label_to_string lbl);
          let c,error = Cfg_ast.find_error c in
          CA.add_edge_e c (CA.G.E.create s l error)
      in
      (* Side effects: Adds to worklist *)
      let process_edges (c,state) edges =
        if debug () then List.iter (fun (s,l,t) -> dprintf "Looking at edge from %s to %s" (Cfg_ast.v2s s) (Pp.ast_exp_to_string t)) edges;
        let resolved_edges, state' = D.get_succs p c edges state in
        let resolve_edge c ((s,l,t) as e,resolved_addrs) =
          match resolved_addrs with
          | Addrs addrs ->
            dprintf "%d Addrs" (List.length addrs);
            let addcond = Ast.lab_of_exp t = None in
            List.fold_left (add_resolved_edge addcond e) c addrs
          | Error | Exit | Indirect ->
            dprintf "Special";
            let c', dest = match resolved_addrs with
              | Error -> Cfg_ast.find_error c
              | Exit -> Cfg_ast.find_exit c
              | Indirect -> Cfg_ast.find_indirect c
              | _ -> failwith "impossible"
            in
            CA.add_edge_e c' (CA.G.E.create s l dest)
        in
        List.map fst resolved_edges,
        List.fold_left resolve_edge c resolved_edges,
        state'
      in
      (* Main loop *)
      while not (Worklist.is_empty q) do
        let unresolved_edges = Worklist.all q in
        let resolved_edges,c',state' = process_edges (!c,!state) unresolved_edges in
        if List.length resolved_edges = 0 then failwith "no progress";
        Worklist.filter (fun e -> not (List.mem e resolved_edges)) q;
        c := c';
        state := state'
      done;

      !c, !state
    in
    let c, state = iteration [(entry,None,exp_of_lab (Addr addr))] (c,D.State.init) in

    let c, state =
      if D.fixpoint = false then c, state
      else
        let continue = ref true in
        let c = ref c in
        let state = ref state in
        let iter = ref 0 in
        while !continue && !iter < 5 do
          dprintf "Running cfg recovery until fixpoint: iteration %d" !iter;
          let origc = !c in
          let worklist = List.flatten (Hashtbl.fold (fun k (_,e,_) a -> e::a) edgeh []) in
          let c',state' = iteration worklist (!c,!state) in
          c := c';
          state := state';
          continue := origc <> !c;
          incr iter;
        done;
        !c, !state
    in

    (* Remove indirect if unused *)
    let c = if CA.G.in_degree c indirect = 0 then CA.remove_vertex c indirect else c in
    (* Remove error if unused *)
    let c = if CA.G.in_degree c error = 0 then CA.remove_vertex c error else c in

    c, state

  let disasm ?callsig p = disasm_at ?callsig p (Asmir.get_start_addr p)
end

let recursive_descent =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC)(FUNCFINDER_DUMB) in
  RECURSIVE_DESCENT.disasm
let recursive_descent ?callsig a = fst(recursive_descent ?callsig a)

let recursive_descent_at =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC)(FUNCFINDER_DUMB) in
  RECURSIVE_DESCENT.disasm_at
let recursive_descent_at ?callsig a b = fst(recursive_descent_at ?callsig a b)

let vsa_full =
  let module VSA = Make(VSA_SPEC)(FUNCFINDER_DUMB) in
  VSA.disasm
let vsa ?callsig a = fst(vsa_full ?callsig a)

let vsa_at_full ?callsig =
  let module VSA = Make(VSA_SPEC)(FUNCFINDER_DUMB) in
  VSA.disasm_at ?callsig
let vsa_at ?callsig a b = fst(vsa_at_full ?callsig a b)

type algorithm =
  | Vsa
  | Rd

let recover ?callsig = function
  | Vsa -> vsa ?callsig
  | Rd -> recursive_descent ?callsig

let recover_at ?callsig = function
  | Vsa -> vsa_at ?callsig
  | Rd -> recursive_descent_at ?callsig
