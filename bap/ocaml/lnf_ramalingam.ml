module D = Debug.Make(struct let name = "LnfRamalingam" and default=`NoDebug end)
open D
open Lnf

module type RamalingamHelper = functor (Gr: G) ->
sig
  type t
  val init: Gr.G.t -> Gr.G.V.t -> t
  val find_headers: t -> Gr.G.V.t list -> Gr.G.V.t list
end

module Make (RH: RamalingamHelper) (C: G) =
struct
  (* TODO(awreece) Assert C is a concrete graph. *)
  module R = RH(C)

  (* Get loop information from Ramalingams loop forest *)
  let lnf cfg v0 =
    let module Comp = Graph.Components.Make(C.G) in
    let module VS = Set.Make(C.G.V) in

    let cfg = C.copy cfg in
    let r_t = R.init cfg v0 in

    let f cfg =
      (*
       * We only want to process loops. The scc algorithm will identify
       * isolated nodes, but we only consider those that also have a self loop.
       *)
      let sccs = List.filter (fun scc ->
        match scc with
        | [x] -> C.G.mem_edge cfg x x
        | _ -> true) (Comp.scc_list cfg) in
      let rec process_scc cfg scc =
        dprintf "process_scc";
        match scc with
        | [] -> failwith "loopinfo_from_ramalingam: impossible"
        | [x] -> dprintf "Self loop at %s" (C.v2s x);
          { headers=scc; body=scc; children=[] }
        | _ ->
          let h = Hashtbl.create (List.length scc) in
          List.iter (fun v -> dprintf "scc %s" (C.v2s v); Hashtbl.add h v ()) scc;
          let cfg = cfg in

          let entry_nodes = List.fold_right VS.add (R.find_headers r_t scc) VS.empty in

          let closing_edges = C.G.fold_edges_e (fun e l ->
            if Hashtbl.mem h (C.G.E.src e) = true && VS.mem (C.G.E.dst e) entry_nodes = true
            then e::l
            else l
          ) cfg [] in

          dprintf "entry nodes %d closing edges %d" (VS.cardinal entry_nodes) (List.length closing_edges);

          (* Progress check *)
          assert (closing_edges <> []);

          (* Remove closing edges *)
          let cfg = List.fold_left C.remove_edge_e cfg closing_edges in

          (* SCCs contained in original region *)
          let sccs = List.filter (fun scc ->
            List.for_all (Hashtbl.mem h) scc && match scc with
                                                | [x] -> C.G.mem_edge cfg x x
                                                | _ -> true
          ) (Comp.scc_list cfg) in

          { headers=List.sort compare (VS.elements entry_nodes)
          ; body=List.sort compare scc
          ; children=List.sort compare (List.map (process_scc cfg) sccs) }

      in
      match sccs with
      | [] -> []
      | _ -> List.sort compare (List.map (process_scc cfg) sccs)
    in
    f cfg


end


