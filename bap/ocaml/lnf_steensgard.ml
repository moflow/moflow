(* Steensgard's loop nesting algorithm

   See Steensgaard, B. (1993). Sequentializing Program Dependence
   Graphs for Irreducible Programs (No. MSR-TR-93-14), particularly
   page 5.  (The algorithm is very simple)
*)

open Lnf

module SteensgardHeader (Gr: Lnf.G) =
  struct
      module VS = Set.Make(Gr.G.V)

      type t = Gr.G.t

      let init cfg v0 = cfg
      let find_headers cfg scc =
        let set = Gr.G.fold_edges_e (fun e s ->
          let in_scc v = List.exists (Gr.G.V.equal v) scc in
          if in_scc (Gr.G.E.dst e) && not (in_scc (Gr.G.E.src e))
          then VS.add (Gr.G.E.dst e) s
          else s
        ) cfg VS.empty in
        VS.fold (fun v l -> v::l) set []
    end

module Make (C: Lnf.G) = Lnf_ramalingam.Make (SteensgardHeader) (C)
