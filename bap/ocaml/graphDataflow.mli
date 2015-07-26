(** Dataflow module for use with the ocamlgraph library.  You should
    not need to use this module directly in BAP.  Instead, use
    {!CfgDataflow}.

    @author Ivan Jager
*)


(** Types of graph that data flow is defined on *)
module type G =
sig
  type t
  module V : Graph.Sig.COMPARABLE
  module E : Graph.Sig.EDGE with type vertex = V.t
  val pred_e : t -> V.t -> E.t list
  val succ_e : t -> V.t -> E.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end

(** Data flow direction.  [Forward] dataflow propagates values in the
    same direction as edges in the control flow graph, while
    [Backward] dataflow propagates values in the reverse direction. *)
type direction = Forward | Backward


(** The lattice the dataflow is defined on.  See
    {{:http://en.wikipedia.org/wiki/Meet-semilattice}here} for more
    information.
*)
module type BOUNDED_MEET_SEMILATTICE =
sig

  (** The type of a latice element *)
  type t

  (** Top of the latice (the bound)*)
  val top : t

  (** The meet operator.
      [meet v1 v2] should form a lattice. In particular,
      [meet v1 Top = meet Top v1 = v1],
      and [meet Bottom _ = meet _ Bottom = Bottom].
  *)
  val meet : t -> t -> t

  (** Equality checking for latice values.  Returns true when the two
      latice elements are the same.  *)
  val equal : t -> t -> bool
end

(** The lattice the dataflow is defined on, with a widening operator. *)
module type BOUNDED_MEET_SEMILATTICE_WITH_WIDENING =
sig

  include BOUNDED_MEET_SEMILATTICE

  (** The widening operator W.

      I'm not going to repeat the formal definition here, since it is
      intended for infinite chains, and we often have finite (but long
      chains), which are just as bad.

      Suffice to say if you have some long finite chain over the
      lattice values, applying the widening operator to the chain
      should cause it to stablize quickly.

      If [A < B], then [widen B A] should probably equal the bottom
      element of the lattice. If [A <= B], then [widen A B = A]. *)
  val widen : t -> t -> t
end

(** Run-time options for the data-flow analysis *)
module type OPTIONS = sig
  (** Type of extra information that can be specified each time the
      dataflow analysis is applied. *)
  type t

  (** Default options when none are specified *)
  val default : t
end

(** A No-op options module *)
module NOOPTIONS : OPTIONS with type t = unit

(** A dataflow problem is defined by a lattice over a graph. *)
module type DATAFLOW =
sig

  module L : BOUNDED_MEET_SEMILATTICE
  module G : G
  module O : OPTIONS

  (** The transfer function over node elements, e.g., basic
      blocks. *)
  val node_transfer_function : O.t -> G.t -> G.V.t -> L.t -> L.t

  (** The transfer function over edge elements, e.g., conditions. *)
  val edge_transfer_function : O.t -> G.t -> G.E.t -> L.t -> L.t

  (** The starting node for the analysis. *)
  val s0 : O.t -> G.t -> G.V.t

  (** The initial lattice value given to node [s0]. All other nodes
      start out with [Top]. *)
  val init : O.t -> G.t -> L.t

  (** The dataflow direction. *)
  val dir : O.t -> direction

end

(** A dataflow problem with widening. *)
module type DATAFLOW_WITH_WIDENING =
sig

  module L : BOUNDED_MEET_SEMILATTICE_WITH_WIDENING
  module G : G
  module O : OPTIONS

  (** The transfer function over node elements, e.g., basic
      blocks. *)
  val node_transfer_function : O.t -> G.t -> G.V.t -> L.t -> L.t

  (** The transfer function over edge elements, e.g., conditions. *)
  val edge_transfer_function : O.t -> G.t -> G.E.t -> L.t -> L.t

  (** The starting node for the analysis. *)
  val s0 : O.t -> G.t -> G.V.t

  (** The initial lattice value given to node [s0]. All other nodes
      start out with [Top]. *)
  val init : O.t -> G.t -> L.t

  (** The dataflow direction. *)
  val dir : O.t -> direction
end

(** Build a custom dataflow algorithm for the given dataflow problem [D]. *)
module Make :
  functor (D : DATAFLOW) ->
sig
  (** [worklist_iterate g] returns a worklist algorithm for graph [g]
      as a pair of functions [in,out]. [in], when given a node [v],
      computes the lattice value going in to that node, [v]. [out],
      when given a node [v], computes the lattice value exiting
      [v]. *)
  val worklist_iterate : ?init:(D.O.t -> D.G.t -> D.L.t) ->
    ?opts:D.O.t -> D.G.t -> (D.G.V.t -> D.L.t) * (D.G.V.t -> D.L.t)
end

(** Build a custom dataflow algorithm for the given dataflow problem
    with widening operator [D]. *)
module MakeWide :
  functor (D : DATAFLOW_WITH_WIDENING) ->
sig
  (** Same as [worklist_iterate], but additionally employs the
      widening operator as lattice values propagate over backedges in
      the CFG.  Backedges are identified by observing when lattices
      values flow in cycles.

      @param nmeets The number of times a widening edge should be
      computed using meet.  After this threshold is reached, the widening
      operator will be applied.
  *)
  val worklist_iterate_widen : ?init:(D.O.t -> D.G.t -> D.L.t) ->
    ?nmeets:int -> ?opts:D.O.t -> D.G.t -> (D.G.V.t -> D.L.t) * (D.G.V.t -> D.L.t)
end
