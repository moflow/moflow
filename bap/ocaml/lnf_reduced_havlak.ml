module D = Debug.Make(struct let name = "LnfReducedHavlak" and default=`NoDebug end)
open D
open Lnf

module UF(T: Hashtbl.HashedType) = struct
  module H = Hashtbl.Make(T)
  (*
   * A ufnode is a Parent(rank) or a Child(parent).
   *
   * If it is a Parent, it is the representative element of the set
   * that contains it. Otherwise, the representative element can be found by
   * traversing the Child pointers. The rank is roughly the length of the
   * longest possible path from any child to the representative element.
   *)
  type ufnode = Parent of int
              | Child of T.t

  let create n = H.create n

  (* Adds v as the representative element of a set containing just itself. *)
  let add h v = H.add h v (Parent 0)

  let contains h v = H.mem h v

  (*
   * Finds the representative element of the set containing v.
   *
   * Does path compression, so after a find h v, v points directly
   * to the representative element.
   *
   * TODO(awreece) Don't require so many H.find s by making
   * ufnode = ... | Child of ufnode ?
   *)
  let rec find h v =
     match H.find h v  with
     | Parent(_) -> v
     | Child(parent) -> let new_parent = find h parent in
                        H.replace h v (Child new_parent); new_parent

  (*
   * Joins the set containing u with the set containing v.
   *
   * Does union by rank, so the resulting representative element will be the
   * one with the larger rank.
   *)
  let union h u v =
    (*
     * A convenience method, finds the rank of a parent.
     * Assumes v is the representative element of the set containing it.
     *)
    let getrank h v = match H.find h v with
                      | Parent(r) -> r
                      | _ -> assert false  in
    let pu = find h u in
    let pv = find h v in
    (* If they are already in the same set, do nothing. *)
    if T.equal pu pv then ()
    else let ur = getrank h pu in
         let vr = getrank h pv in
         (* Otherwise, reparent the one with a lower rank. *)
         if (ur < vr) then H.replace h pu (Child pv)
         else if (ur > vr) then H.replace h pv (Child pu)
         (*
          * If both have the same rank, increment one of the ranks and
          * reparent the other
          *)
         else (H.replace h pu (Parent (ur+1)); H.replace h pv (Child pu))

  let iter h f v =
    let rep = find h v in
    H.iter (fun k _ -> if T.equal (find h k) rep then f k else ()) h

  let mem h v = H.mem h v
end



module Make(C: G) =
struct
  module VH = Hashtbl.Make(C.G.V)
  module VS = Set.Make(C.G.V)

  module VUF = UF(C.G.V)

  module Dom = Dominator.Make(C.G)

  module Dfs = Graph.Traverse.Dfs(C.G)

  type dfs_edge = Tree | Forward | Back | Cross

  (* Get loop information from Havlakcs loop forest *)
  let lnf cfg v0 =
    let cfg = ref cfg in

    let loop_parent = VH.create (C.G.nb_vertex !cfg) in
    let loopTree = VUF.create (C.G.nb_vertex !cfg) in
    let mergedLoops = VUF.create (C.G.nb_vertex !cfg) in
    let crossFwdEdges = VH.create (C.G.nb_vertex !cfg) in

    let dfs_parent = VH.create (C.G.nb_vertex !cfg) in
        let rev_dfs_order = ref [] in
    let prev = Stack.create () in
    let () = Stack.push v0 prev in
    let () = Dfs.iter_component ~pre:(fun v ->
                                        rev_dfs_order := v::!rev_dfs_order;
                                        VH.add dfs_parent v (Stack.top prev);
                                        VH.add crossFwdEdges v [];
                                        VUF.add loopTree v;
                                        Stack.push v prev
                                     )
                                ~post:(fun v ->
                                        ignore (Stack.pop prev);
                                      )
                                !cfg v0 in
    let () = VH.remove dfs_parent v0 in


    let classify_edge u v =
      let rec is_ancestor u v =
        if C.G.V.equal u v then true
        else if VH.mem dfs_parent v then is_ancestor u (VH.find dfs_parent v)
        else false in

      (* The entry node has indegree 0, so everything else has a dfs parent. *)
      if C.G.V.equal u (VH.find dfs_parent v) then Tree
      else if is_ancestor u v then Forward
      else if is_ancestor v u then Back
      else Cross in

    let collapse loopBody loopHeader =
      List.iter (fun z -> VH.replace loop_parent z loopHeader;
                          VUF.union loopTree loopHeader z
                ) loopBody in

    let findloop potentialHeader =
      (*  worklist =
       *    {LoopTree.find( y ) | y → potentialHeader is a backedge}
       *    - {potentialHeader};
       *)
      let workList = Queue.create () in

      (* TODO(awreece) Do we have to recompute here? We *do* modify the graph. *)
      let dom = (Dom.compute_all !cfg v0).Dom.dom in

      let () = C.G.iter_pred (fun y ->
        if dom potentialHeader y  (* If y -> potentialHeader is a backedge. *)
        then
          let ltfy = VUF.find loopTree y in
          if not (C.G.V.equal ltfy potentialHeader)
          then Queue.push ltfy workList
          else ()
        else ()
      ) !cfg potentialHeader in

      let loopBody = ref [] in

      while not (Queue.is_empty workList) do
        let y = Queue.pop workList in
        let () = loopBody := y::!loopBody in
        C.G.iter_pred (fun z ->
          let ltfz = VUF.find loopTree z in
          if not (List.exists (C.G.V.equal ltfz) !loopBody) &&
             not (C.G.V.equal ltfz potentialHeader) &&
             not (Queue.fold (fun p v -> p || (C.G.V.equal ltfz v)) false workList)
          then Queue.push ltfz workList
          else ()
          ) !cfg y
      done;
      if not (BatList.is_empty !loopBody)
      then (collapse !loopBody potentialHeader;
            VUF.add mergedLoops potentialHeader)
      else () in

    let mergeLoopsWithEntryVertex z =
      let bt = ref (if VUF.contains mergedLoops z
                   then VUF.find mergedLoops z
                   else z) in
      while VH.mem loop_parent !bt do
        let t = VH.find loop_parent !bt in
        let u = VUF.find mergedLoops t in
        let () = bt := u in

        if VH.mem loop_parent !bt
        then VUF.union mergedLoops u (VH.find loop_parent !bt)
        else ()
      done in

    let processCrossFwdEdges x =
      List.iter (fun (y,z) ->
        (* TODO(awreece) Oh god, which UF do we use? I think loopTree... *)
        cfg := C.add_edge !cfg (VUF.find loopTree y) (VUF.find loopTree z);
        mergeLoopsWithEntryVertex z
      ) (VH.find crossFwdEdges x) in

    let lca x y =
      let rec add_all stack node =
        if VH.mem dfs_parent node
        then (Stack.push node stack; add_all stack (VH.find dfs_parent node))
        else () in

      let rec findFirstDifferent stack1 stack2 oldNode =
        if Stack.is_empty stack1 || Stack.is_empty stack2
        then oldNode
        else let node1 = Stack.pop stack1 in
             let node2 = Stack.pop stack2 in
             if C.G.V.equal node1 node2
             then findFirstDifferent stack1 stack2 node1
             else oldNode in

      let parentX = Stack.create () in
      let parentY = Stack.create () in
      let () = add_all parentX x in
      let () = add_all parentY y in

      findFirstDifferent parentX parentY v0 in

    let constructReducedHavlakForest () =
      let () = C.G.iter_edges (fun y x ->
        match classify_edge y x with
        | Cross | Forward ->
            let ancestor = lca y x in
            let newList = (y,x)::(VH.find crossFwdEdges ancestor) in
            let () = cfg := C.remove_edge !cfg y x in
            VH.replace crossFwdEdges ancestor newList
        | _ -> ()
      ) !cfg in
      List.iter (fun x ->
        processCrossFwdEdges x;
        findloop x
      ) !rev_dfs_order in

    let makeExplicitRepresentation () =
      let lnfNodes = VH.create (C.G.nb_vertex !cfg) in
      let topLevelLoops = ref VS.empty in
      let repToChildren = VH.create (C.G.nb_vertex !cfg) in

      let getLnfNode repHeader = if VH.mem lnfNodes repHeader
                                 then VH.find lnfNodes repHeader
                                 else {headers=[]; body=[]; children=[]} in

      let getChildren node = if VH.mem repToChildren node
                             then VH.find repToChildren node
                             else [] in

      let process_vertex v =
        if VUF.mem mergedLoops v  (* If v is the header of some loop. *)
        then let rep = VUF.find mergedLoops v in
             let () = if VH.mem loop_parent rep
                      then let parent = VH.find loop_parent rep in
                           VH.replace repToChildren parent (rep::(getChildren parent))
                      else topLevelLoops := VS.add rep !topLevelLoops in

             let pnode = getLnfNode rep in
             VH.replace lnfNodes rep {pnode with
                                      headers=v::pnode.headers;
                                      body=v::pnode.body}
        else let parent = VH.find loop_parent v in
             let pnode = getLnfNode parent in
             VH.replace lnfNodes parent {pnode with body=v::pnode.body} in

      let rec cleanLnfNode v =
        let node = VH.find lnfNodes v in
        let children = VH.find repToChildren v in
        let children_nodes = List.map cleanLnfNode children in
        {headers=(List.sort compare node.headers);
         body=(List.sort compare node.body);
         children=(List.sort compare children_nodes)} in

      let () = List.iter process_vertex !rev_dfs_order in
      List.sort compare (List.map cleanLnfNode (VS.elements !topLevelLoops)) in

    let () = constructReducedHavlakForest () in
    makeExplicitRepresentation ()
end
