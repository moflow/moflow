open BatPervasives
open OUnit
open Lnf

module I = struct
  type t = int
  let compare : t -> t -> int = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end

module G = struct
  include Graph.Builder.I(Graph.Imperative.Digraph.ConcreteLabeled(I)(I))
  let remove_edge g u v = G.remove_edge g u v; g
  let remove_edge_e g e = G.remove_edge_e g e; g

  let v2s v = string_of_int (G.V.label v)
end

type vertex_spec = G.G.V.t * (G.G.V.t list)
type test = G.G.V.t * (vertex_spec list) * G.G.V.t lnf

(*
 * build_graph: vertex_spec list -> G.t
 *
 * build_graph spec Constructs the input graph from the provided specification.
 *)
let build_graph vertex_spec_list =
  let g = G.G.create () in
  let rec add_edges (src,dl) = match dl with
    | [] -> ()
    | dst::vl -> ignore (G.add_edge g src dst); add_edges (src,vl) in
  List.iter add_edges vertex_spec_list; g;;

(*
 * run_test: test
 * run_test test_spec runs the given test.
 *)
let run_test compute_lnf (v0,edge_list,expected_lnf) =
  let g = build_graph edge_list in
  let lnf = compute_lnf g v0 in
  assert_bool ("Invalid lnf: " ^ (Lnf.string_of_lnf G.v2s lnf))
              (Lnf.validate_lnf lnf);
              assert_equal ~printer:(Lnf.string_of_lnf G.v2s) expected_lnf lnf
