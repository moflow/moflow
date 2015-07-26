open BatPervasives
open OUnit

module G = Graph.Pack.Digraph
module DJG = Djgraph.Make(G)

let n = G.V.create;;

let v0 = n 1
and va = n 2
and vb = n 3
and vc = n 4
and vd = n 5
and ve = n 6
and vf = n 7
and vg = n 8
and vh = n 9
and vend = n 10

(* 
 * TODO(awreece) Is there a better way?
 *
 * The djgraph test spec is of the form (start_vertex, edge_list, dj_graph_spec)
 *
 * edge_list is the specification for the initial graph, and is a list of every
 * vertex and all outgoing edges. It is of the form (src, dest_list) where
 * dest_list is a list of vertices.
 *
 * dj_graph_spec is the specificaiton of the djgraph. It is a list of every
 * vertex, that vertex's expected level in the djgraph, and the expected
 * outgoing edges for that vertex. It is of the form (src, level, dj_edge_list)
 * and dj_edge_spec = (type, dest). In both cases, the src and dest vertex are
 * vertices in the underlying graph.
 *)

type dj_edge_spec = Djgraph.edge_type * G.V.t
type dj_vertex_spec = G.V.t * int * (dj_edge_spec list)
type vertex_spec = G.V.t * (G.V.t list)
type test = G.V.t * (vertex_spec list) * (dj_vertex_spec list)

let main_sreedhar : test = (v0, [
  (v0, [va; vend]);
  (va, [vb; vc]);
  (vb, [vd]);
  (vc, [vd; ve]);
  (vd, [vg; vf]);
  (ve, [vf]);
  (vf, [vh]);
  (vg, [vd; vh]);
  (vh, [va; vc; vend]); 
  (vend, [])
], [
  (v0, 0, [(Djgraph.D, va); (Djgraph.D, vend)]);
  (va, 1, [(Djgraph.D, vb); (Djgraph.D, vc); (Djgraph.D, vd); (Djgraph.D, vf); (Djgraph.D, vh)]);
  (vb, 2, [(Djgraph.CJ, vd)]);
  (vc, 2, [(Djgraph.CJ, vd); (Djgraph.D, ve)]);
  (vd, 2, [(Djgraph.CJ, vf); (Djgraph.D, vg)]);
  (ve, 3, [(Djgraph.CJ, vf)]);
  (vf, 2, [(Djgraph.CJ, vh)]);
  (vg, 3, [(Djgraph.BJ, vd); (Djgraph.CJ, vh)]);
  (vh, 2, [(Djgraph.BJ, va); (Djgraph.CJ, vc); (Djgraph.CJ, vend)]);
  (vend, 1, [])
]) 

let mini_sreedhar : test = (va, [
  (va, [vb; vc]);
  (vb, [vc; vd]);
  (vc, [vb; ve]);
  (vd, [vb]);
  (ve, [vc]);
], [
  (va, 0, [(Djgraph.D, vb); (Djgraph.D, vc)]);
  (vb, 1, [(Djgraph.CJ, vc); (Djgraph.D, vd)]);
  (vc, 1, [(Djgraph.CJ, vb); (Djgraph.D, ve)]);
  (vd, 2, [(Djgraph.BJ, vb)]);
  (ve, 2, [(Djgraph.BJ, vc)])
])

(*
 * build_graph: vertex_spec list -> G.t
 *
 * build_graph spec Constructs the input graph from the provided specification.
 *)
let build_graph vertex_spec_list = 
  let g = G.create () in
  let rec add_edges (src,dl) = match dl with
    | [] -> ()
    | dst::vl -> (G.add_edge g src dst; add_edges (src,vl)) in
  List.iter add_edges vertex_spec_list; g;;


exception NotFound of G.V.t

(*
 * verify_djgraph: (dj_vertex_spec list) -> DJG.t -> unit
 *
 * verify_djgraph djg_spec djg verifies that the djgraph conforms to the
 *                             provided spec. It does this by asserting
 *                             that each edge in the graph is matched
 *                             by an edge in the spec and asserting the spec and
 *                             the djgraph have the same cardinality (so the 
 *                             djgraph is missing no edges). 
 *)
let verify_djgraph djg_spec djg = 
  let s_of_v v = (string_of_int (G.V.label v)) in
  let s_of_djv v = s_of_v (DJG.to_underlying v) in
  let find_edge_spec edge_specs v =
    try List.find (fun (_,u) -> u = v) edge_specs
    with Not_found -> raise (NotFound(v)) in
  (*
   * verify_edge: dj_edge_spec list -> DJG.E.t -> int -> int
   * verify_edge edge_specs edge nedges Verifies that the edge corresponds to a
   *                                    valid edge in edge_specs and returns
   *                                    nedges + 1. This is done so we can count
   *                                    the number of edges verified.
   *)
  let verify_edge edge_specs edge nedges = 
    let (edge_type, _) = 
      find_edge_spec edge_specs (DJG.to_underlying (DJG.E.dst edge)) in
    assert_equal ~msg:("Invalid type for edge " 
                       ^ (s_of_djv (DJG.E.src edge)) ^  "->"
                       ^ (s_of_djv (DJG.E.dst edge))) 
                 ~printer:(fun t -> match t with | Djgraph.D -> "D"
                                                 | Djgraph.CJ -> "CJ"
                                                 | Djgraph.BJ -> "BJ")
                 edge_type (DJG.E.label edge); nedges + 1 in
  let find_vertex_spec v = 
    try List.find (fun (u, _, _) -> u = v) djg_spec
    with Not_found -> 
      assert_failure ("No vertex spec found for " ^ (s_of_v v)) in
  (*
   * verify_vertex: DJG.V.t -> int -> int
   * verify_vertex vertex nedges Verifies that the vertex is at the correct
   *                             level in the djgraph and verifies that every
   *                             outgoing edge is valid. 
   *
   *                             Returns nedges + number_outgoing_edges(vertex)
   *                             This is done so we can count the number of 
   *                             edges verified.
   *)
  let verify_vertex v nedges =
    let (_, level, edges) = find_vertex_spec (DJG.to_underlying v) in
    assert_equal ~msg:("Invalid level for vertex") ~printer:string_of_int 
                 level (DJG.level v);
    try DJG.fold_succ_e (verify_edge edges) djg v nedges
    with NotFound(d) -> assert_failure ("No edge found for " 
                                        ^ (s_of_djv v) ^ "->" ^ (s_of_v d)) in
  (* Verify all edges and count number of verified edges. *)
  let djg_nedges = DJG.fold_vertex verify_vertex djg 0 in
  let nedges = List.fold_left (fun s (_,_,l) -> s + List.length l) 0 djg_spec in
  assert_equal ~msg:("Graph sizes are not the same") ~printer:string_of_int 
               nedges djg_nedges;;

(*
 * run_test: test
 * run_test test_spec runs the given test.
 *)
let run_test (v0,edge_list,djg_spec) = 
  let g = build_graph edge_list in 
  let djg = DJG.dj_graph g v0 in
  assert_equal ~msg:"DJGraph should have same number of vertices" 
               ~printer:string_of_int
               (G.nb_vertex g) (DJG.nb_vertex djg);
  verify_djgraph djg_spec djg;;

let suite = "Djgraph" >:::
  [
    "sreedhar_test" >:: (fun () -> run_test main_sreedhar);
    "mini_sreedhar_test" >:: (fun () -> run_test mini_sreedhar);
  ]
