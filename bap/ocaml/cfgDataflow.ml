(* Dataflow for CFGs *)

module D = Debug.Make(struct let name = "CfgDataflow" and default = `NoDebug end)
open D
open GraphDataflow

module type CFG =
sig
  type exp
  type stmt
  type lang = stmt list
  module G : sig
    type t
    module V : Graph.Sig.COMPARABLE
    module E : Graph.Sig.EDGE with type vertex = V.t and type label = (bool option * exp) option
    val pred_e : t -> V.t -> E.t list
    val succ_e : t -> V.t -> E.t list
    val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  end
  val get_stmts : G.t -> G.V.t -> lang
  val v2s : G.V.t -> string
end

module type DATAFLOW =
sig

  module L : BOUNDED_MEET_SEMILATTICE
  module CFG : CFG
  module O : OPTIONS

  val stmt_transfer_function : O.t -> CFG.G.t -> CFG.G.V.t * int -> CFG.stmt -> L.t -> L.t
  val edge_transfer_function : O.t -> CFG.G.t -> CFG.G.E.t -> CFG.exp option -> L.t -> L.t
  val s0 : O.t -> CFG.G.t -> CFG.G.V.t
  val init : O.t -> CFG.G.t -> L.t
  val dir : O.t -> direction
end

module type DATAFLOW_WITH_WIDENING =
sig

  module L : BOUNDED_MEET_SEMILATTICE_WITH_WIDENING
  module CFG : CFG
  module O : OPTIONS

  val stmt_transfer_function : O.t -> CFG.G.t -> CFG.G.V.t * int -> CFG.stmt -> L.t -> L.t
  val edge_transfer_function : O.t -> CFG.G.t -> CFG.G.E.t -> CFG.exp option -> L.t -> L.t
  val s0 : O.t -> CFG.G.t -> CFG.G.V.t
  val init : O.t -> CFG.G.t -> L.t
  val dir : O.t -> direction
end

module MakeWide (D:DATAFLOW_WITH_WIDENING) =
struct
  let fold o f l stmts = match D.dir o with
    | Forward -> List.fold_left (fun a b -> f b a) l stmts
    | Backward -> BatList.fold_right f stmts l
  module DFSPECW = struct
    module L=D.L
    module G=D.CFG.G
    module O=D.O
    let node_transfer_function o g v l =
      dprintf "node_transfer_function @%s" (D.CFG.v2s v);
      let l, _ = fold o (fun s (l,i) -> D.stmt_transfer_function o g (v,i) s l, i+1) (l,0) (D.CFG.get_stmts g v) in
      l
    let edge_transfer_function o g e l =
      let arg = match G.E.label e with
        | Some(_,e) -> Some e
        | None -> None
      in
      let o = D.edge_transfer_function o g e arg l in
      dprintf "edge_transfer_done";
      o
    let s0 = D.s0
    let init = D.init
    let dir = D.dir
  end
  module DFW = GraphDataflow.MakeWide(DFSPECW)
  let worklist_iterate_widen =
    DFW.worklist_iterate_widen
  let worklist_iterate_widen_stmt ?init ?nmeets ?(opts=D.O.default) g =
    let win,wout = worklist_iterate_widen ?init ?nmeets ~opts g in
    let winstmt (v,n) =
      let l = win v in
      let l,_ = fold opts (fun s (l,i) -> D.stmt_transfer_function opts g (v,i) s l, i+1) (l,0) (BatList.take n (D.CFG.get_stmts g v)) in
      l
    and woutstmt (v,n) =
      let l = win v in
      let l, _ = fold opts (fun s (l,i) -> D.stmt_transfer_function opts g (v,i) s l, i+1) (l,0) (BatList.take (n+1) (D.CFG.get_stmts g v)) in
      l
    in
    winstmt, woutstmt
  let last_loc g v =
    v, List.length (D.CFG.get_stmts g v) - 1
end

module Make (D:DATAFLOW) =
struct
  let worklist_iterate, worklist_iterate_stmt, last_loc =
    let module DFSPEC = struct
      module L = struct
        include D.L
        let widen = D.L.meet
      end
      module CFG = D.CFG
      module O = D.O
      let stmt_transfer_function = D.stmt_transfer_function
      let edge_transfer_function = D.edge_transfer_function
      let s0 = D.s0
      let init = D.init
      let dir = D.dir
    end in
    let module DF = MakeWide(DFSPEC) in
    DF.worklist_iterate_widen ~nmeets:0,
    DF.worklist_iterate_widen_stmt ~nmeets:0,
    DF.last_loc
end
