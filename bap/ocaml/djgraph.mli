
(* TODO (ed) Add djgraph.mli file *)
(* TODO (ed) Add dot printer? Would be a good test of API, but would
   require a slightly more complex signature. (label_to_string, etc.)
   Maybe we can create a separate functor for this. *)

(* The miminal set of graph features necessary for DJGraph.Make. *)

module type G = sig
  include Dominator.G

  val iter_edges : (V.t -> V.t -> unit) -> t -> unit
end

(*
 * DJ Edges can be D edges (dominating edges) or J (joining edges). D edges are
 * edges in the underlying dominator tree. There are two types of J edge:
 *  - BJ (back join). An edge x -> y is a BJ edge if y dom x
 *  - CJ (cross join). An edge x -> y is a CJ edge otherwise.
 *)
type edge_type = D | BJ | CJ

(* 
 * Make is a functor that returns a Djgraph module. Note that a Djgraph is an
 * extension to Graph
 *)
module type MakeType =
  functor (Gr: G) ->
    sig
      include Graph.Sig.I with type E.label = edge_type

      val dj_graph: Gr.t -> Gr.V.t -> t
      (* The underlying vertex in the graph. *)
      val to_underlying: V.t -> Gr.V.t
      (* The level in the djgraph. *)
      val level: V.t -> int
    end

(* TODO(awreece) Accept a builder parameter? *)
module Make: MakeType 
