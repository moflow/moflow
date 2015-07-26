open Asmir_disasm
open Big_int_convenience
module CS = Cfg.SSA
module D = Debug.Make(struct let name = "Switch_condition" and default = `NoDebug end)
open D
open Ssa
open Type
module VM = Var.VarMap
module VS = Var.VarSet

let add_switch_conditions_int origssa optssa vsa_in =

  let stop_before = function
    | Ssa.Load _ | Ssa.Store _ -> true
    | _ -> false
  in
  let _, m, cp = Copy_prop.copyprop_ssa ~stop_before optssa in

  let find_leaf e =
    dprintf "find_leaf %s" (Pp.ssa_exp_to_string e);
    let leafs = Hashtbl.create 10 in
    let v = object(self)
      inherit Ssa_visitor.nop
      method visit_exp e = match e with
        (* Memory operations, and variables are leafs *)
        (* | (Load _ | Store _) -> Hashtbl.add leafs e (); SkipChildren *)
        | Var v -> Hashtbl.add leafs e (); SkipChildren
        | _ -> DoChildren
    end in
    ignore(Ssa_visitor.exp_accept v e);
    match Util.get_hash_keys leafs with
    | [x] -> Some x
    | _ -> None
  in

  let g, allgood = CS.G.fold_vertex (fun v (g,allgood) ->
    try
      match List.rev (CS.get_stmts g v) with
      | Jmp(e, _)::_ when Ssa.lab_of_exp e = None &&
                     CS.G.succ g v <> [CS.G.V.create Cfg.BB_Exit] ->
        let cpe = cp e in
        dprintf "indirect jump %s %s" (Pp.ssa_exp_to_string e) (Pp.ssa_exp_to_string cpe);
        let m, indexe, e, t = match e with
          | Var v ->
            (match VM.find v m with
            | Load(m, i, e, t) -> m, i, e, t
            | _ -> raise Not_found)
          | Load(m, i, e, t) -> m, i, e, t
          | _ -> raise Not_found
        in

      (* We found a memory lookup *)
        let cpindexe = cp indexe in
        dprintf "index %s" (Pp.ssa_exp_to_string cpindexe);

      (* Find a leaf in the memory lookup *)
        let leafe = BatOption.get (find_leaf cpindexe) in
        dprintf "leafe: %s" (Pp.ssa_exp_to_string leafe);

      (* Now use VSA to see what the values of the leaf are *)
        let vsa = BatOption.get (vsa_in (Vsa_ssa.last_loc g v)) in
        let vs = Vsa_ssa.exp2vs vsa leafe in
        dprintf "vs: %s" (Vsa_ssa.VS.to_string vs);

        (match Vsa_ssa.VS.concrete ~max:1024 vs with
        | Some x ->
          let cases =
            List.map (fun x ->
              let newinte = Int(x, Typecheck.infer_ssa indexe) in
              let newindexe = Hacks.ssa_replacer ~eq:(==) ~needle:leafe ~haystack:cpindexe ~replacement:newinte in
              dprintf "newindexe: %s" (Pp.ssa_exp_to_string newindexe);
              let newloade = Load(m, newindexe, e, t) in
              dprintf "addr %s %s" (~% x) (Pp.ssa_exp_to_string newloade);
              let vs' = Vsa_ssa.exp2vs vsa newloade in
              let conc = Vsa_ssa.VS.concrete ~max:1 vs' in
              match conc with
              | Some (addr::[]) -> Some(addr, x)
              | Some _ -> failwith "impossible"
              | None -> None
            ) x
          in
          let cases = try List.map BatOption.get cases with e -> raise Not_found in

          let rewrite_jumps g v (leafe, cases) =
            List.iter (fun (addr, bi) ->
              dprintf "switch case %s on expression %s goes to address %s" (~% bi) (Pp.ssa_exp_to_string leafe) (~% addr);
            ) cases;

            let olde = match List.rev (CS.get_stmts g v) with
              | Jmp (e, attrs)::revstmts -> e
              | _ -> failwith "impossible"
            in
            dprintf "olde: %s" (Pp.ssa_exp_to_string olde);

            let g, _ = CS.G.fold_succ_e (fun e (g,cases) ->
              let src = CS.G.E.src e in
              let dst = CS.G.E.dst e in
              let (b, le) = BatOption.get (CS.G.E.label e) in
              let target = match le with
                | BinOp(EQ, olde', Int(i, it)) (*when olde = olde'*) ->
                  i
              (* | BinOp(EQ, olde', Int(i, it)) -> failwith (Printf.sprintf "error: olde %s <> olde' %s" (Pp.ssa_exp_to_string olde) (Pp.ssa_exp_to_string olde')) *)
                | _ -> failwith (Printf.sprintf "invalid edge format %s" (Pp.ssa_exp_to_string le))
              in
              dprintf "target %s" (~% target);
              let case = List.assoc target cases in
            (* Remove this case so we find other switch cases to the same
               target *)
              let cases = List.remove_assoc target cases in
              dprintf "case %s" (~% case);

              let newlabel = Some(b, BinOp(EQ, leafe, Int(case, Typecheck.infer_ssa leafe))) in
              let newe = CS.G.E.create src newlabel dst in

              let g = CS.remove_edge_e g e in
              let g = CS.add_edge_e g newe in

              g, cases
            ) g v (g,cases) in

            g
          in
          let g = rewrite_jumps g v (leafe, cases) in
          dprintf "success at rewriting switch condition!";
          g, allgood
        | None -> raise Not_found)
      | _ -> g, allgood
    with _ -> g, false
    ) origssa (origssa,true) in
    g, allgood

let add_switch_conditions_disasm {optssa; origssa; vsa_in} =
  add_switch_conditions_int origssa optssa vsa_in

let add_switch_conditions_ssacfg asmp ssacfg =
  let has_indirect v = match List.rev (CS.get_stmts ssacfg v) with
    | Jmp(e, _)::_ when Ssa.lab_of_exp e <> None -> true
    | _ -> false
  in
  let has_a_indirect = CS.G.fold_vertex (fun v b ->
    if b then b
    else has_indirect v
  ) ssacfg false
  in
  if not has_a_indirect
  (* Don't run VSA if there are no indirect jumps *)
  then (ssacfg, true)
  else
    let optssa = Vsa_ssa.prepare_ssa_indirect ssacfg in
    (* Cfg_pp.SsaStmtsDot.output_graph (open_out "switchcondition.dot") optssa; *)
    let opts = Vsa_ssa.build_default_prog_options asmp in

    let vsa_in, _ = Vsa_ssa.vsa ~nmeets:0 opts optssa in
    add_switch_conditions_int ssacfg optssa vsa_in
