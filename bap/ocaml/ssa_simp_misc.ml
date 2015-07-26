(** Misc optimizations *)

open Ssa
module C = Cfg.SSA
module D = Debug.Make(struct let name = "SSA_simp_misc" and default=`Debug end)
open D

(* XXX: This function is buggy. See #389 *)
(** Look for conditional jumps that have a constant as the expression and replace with a jump *)
let cfg_jumpelim graph =
  let changed = ref false in
  let g = C.G.fold_vertex
    (fun bb graph ->
      let stmts = C.get_stmts graph bb in
      if List.length stmts > 0 then (
        let revstmts = List.rev stmts in
        let laststmt = List.hd revstmts in
        match laststmt with
        | CJmp(cond, l1, l2, attr)
            when full_value_eq cond val_true || full_value_eq cond val_false ->
          (try
             let bool = cond = val_true in
             let edges = C.G.succ_e graph bb in
             let toremove = List.find (fun e -> match C.G.E.label e with
               | Some(Some b, _) -> b = bool
               | _ -> failwith "expected label on cjmp") edges in
             let edges = List.filter (fun e -> e != toremove) edges in

            (* There should only be one edge remaining *)
             let dst, lastedge = match edges with
               | e::_ -> C.G.E.dst e, e
               | _ -> failwith "Expected a cjmp to have two outgoing edges"
             in

             let graph = C.remove_edge_e graph toremove in
             let graph = C.remove_edge_e graph lastedge in
             let graph = C.add_edge graph bb dst in
             let revnewstmts = Jmp(l1, attr)::(List.tl revstmts) in
             let newstmts = List.rev revnewstmts in
             changed := true;
             C.set_stmts graph bb newstmts
           with Not_found -> graph)
        | _ -> graph)
      else graph
    )
    graph graph in
  g, !changed

