(** Pretty printing for CFGs. *)

open Cfg

module CS = Cfg.SSA
module CA = Cfg.AST

module type DOTTYG =
sig
  type t 
  module V : Graph.Sig.COMPARABLE
  module E :
  sig
    type t
    type label
    val label : t -> label
    val src : t -> V.t
    val dst : t -> V.t
  end


  val iter_vertex : (V.t -> unit) -> t -> unit

  val iter_edges_e : (E.t -> unit) -> t -> unit
  val graph_attributes : t -> Graph.Graphviz.DotAttributes.graph list
  val default_vertex_attributes : t -> Graph.Graphviz.DotAttributes.vertex list
  val vertex_name : V.t -> string
  val vertex_attributes : V.t -> Graph.Graphviz.DotAttributes.vertex list
  val get_subgraph : V.t -> Graph.Graphviz.DotAttributes.subgraph option
  val default_edge_attributes : t -> Graph.Graphviz.DotAttributes.edge list
  val edge_attributes : E.t -> Graph.Graphviz.DotAttributes.edge list
end

let red = 0xff0000
let green = 0x00ff00
let blue = 0x0000ff
let fill = 0xf5f5f5
let linenum_color = "#b8b8b8"

(* Just for convenience *)
module DefAttrs =
struct
  let graph_attributes _ = []
  let default_vertex_attributes _ = [`Shape `Box; `Style `Filled; `Fillcolor fill]
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = [`Color blue]
  let edge_attributes _ = []
end

module DefAttributor(G:CFG) =
struct
  let vertex_attributes _ _ = []
  and edge_attributes _ _ = []
end

module EdgeColorAttributor(G:CFG) =
struct
  let vertex_attributes _ _ = []
  and edge_attributes _ e =
    match Cfg.edge_direction (G.G.E.label e) with
    | Some true -> [`Color green]
    | Some false -> [`Color red]
    | None -> []
end

module type Attributor =
  functor (G:CFG) ->
    sig
      val vertex_attributes: G.G.t -> G.G.V.t -> Graph.Graphviz.DotAttributes.vertex list
      val edge_attributes: G.G.t -> G.G.E.t -> Graph.Graphviz.DotAttributes.edge list
    end

(** Makes a module suitable for use with Graph.Graphviz.Dot  for writting out
    a CFG. *)
module MakeCfgPrinter
  (G:CFG)
  (Printer:sig val print: G.G.t -> (G.G.V.t -> string) * (G.G.E.t -> string) end)
  (Attributor:Attributor)
  =
struct

  include Graph.Graphviz.Dot(
    struct

      type t = G.G.t

      module V =
      struct
        type t = G.G.V.t * G.G.t
        let hash (v,g) = G.G.V.hash v
        let equal x y = G.G.V.equal (fst x) (fst y)
        let compare x y = G.G.V.compare (fst x) (fst y)
      end
      module E =
      struct
        type t = G.G.E.t * G.G.t
        type label = G.G.E.label
        let label (e,g) = G.G.E.label e
        let src (e,g) = (G.G.E.src e, g)
        let dst (e,g) = (G.G.E.dst e, g)
      end

      module Attributor = Attributor(G)

      let iter_edges_e f g =
        G.G.iter_edges_e (fun e -> f (e,g)) g

      let iter_vertex f g =
        G.G.iter_vertex (fun v -> f (v,g)) g

      include DefAttrs

      let vprinter = ref (fun _ -> failwith "Uninitialized vertex printer")
      let eprinter = ref (fun _ -> failwith "Uninitialized edge printer")

      let graph_attributes g =
      (* Use this as an initialization routine *)
        match Printer.print g with
        | (vp, ep) ->
          vprinter := vp;
          eprinter := ep;
          []

      let vertex_name (v,g) = Cfg.bbid_to_string (G.G.V.label v)

      let vertex_attributes (v,g) =
      (* FIXME: The Dot module really should be the one doing the escaping here *)
        `Label (String.escaped(!vprinter v)) :: Attributor.vertex_attributes g v

      let edge_attributes (e,g) = (`Label (String.escaped(!eprinter e))) :: Attributor.edge_attributes g e

    end)

end



module PrintSsaStmts =
struct
  let print g =
    let buf = Buffer.create 1000 in
    let ft = Format.formatter_of_buffer buf in
    let pp = new Pp.pp ft in
    let pr = Buffer.add_string buf in
    (fun b ->
    let stmts = CS.get_stmts g b in
    pr(Cfg.bbid_to_string (CS.G.V.label b));
    pr "\n";
    pp#ssa_stmts stmts;
    Format.pp_print_flush ft ();
    let o = Buffer.contents buf in
    Buffer.clear buf;
    o),
    (fun e ->
      match CS.G.E.label e with
      | Some (_, e) ->
        pp#ssa_exp e;
        Format.pp_print_flush ft ();
        let o = Buffer.contents buf in
        Buffer.clear buf;
        o
      | None -> ""
    )
end

module PrintAstStmts =
struct
  let print g =
    let buf = Buffer.create 1000 in
    let ft = Format.formatter_of_buffer buf in
    let pp = new Pp.pp ft in
    let pr = Buffer.add_string buf in
    (fun b ->
    let stmts = CA.get_stmts g b in
    pr(Cfg.bbid_to_string (CA.G.V.label b));
    pr "\n";
    pp#ast_program stmts;
    Format.pp_print_flush ft ();
    let o = Buffer.contents buf in
    Buffer.clear buf;
    o),
    (fun e ->
      match CA.G.E.label e with
      | Some (_, e) ->
        pp#ast_exp e;
        Format.pp_print_flush ft ();
        let o = Buffer.contents buf in
        Buffer.clear buf;
        o
      | None -> ""
    )
end

module PrintAstAsms =
struct

  exception Found of string

  let append olds news =
    if olds = "" then news
    else olds ^ "\n" ^ news

  let print g =
    (fun b ->
    let open Type in
    let stmts = CA.get_stmts g b in
    let out = List.fold_left (fun s stmt -> match stmt with
    | Ast.Label(Addr a, attrs) ->
      let addrstr = Printf.sprintf "0x%s" (Big_int_convenience.(~%) a) in
      let newasmsstr = 
        try let newasms = BatList.find_map
              (function
                | Asm asm -> Some asm
                | _ -> None) attrs in
            newasms
        with Not_found -> "Unknown" in
      append s (addrstr ^ ": " ^ newasmsstr)
    | _ -> s) "" stmts in
    match out with
    | "" -> Cfg.bbid_to_string(CA.G.V.label b)
    | _ -> out),
    (fun e ->
      match CA.G.E.label e with
      | Some (Some b, _) ->
        string_of_bool b
      | Some (None, e) ->
        Pp.ast_exp_to_string e
      | _ -> ""
    )
end

module SsaStmtsDot = MakeCfgPrinter (CS) (PrintSsaStmts) (EdgeColorAttributor)

module AstStmtsDot = MakeCfgPrinter (CA) (PrintAstStmts) (EdgeColorAttributor)

module AstAsmsDot = MakeCfgPrinter (CA) (PrintAstAsms) (EdgeColorAttributor)

module SsaBBidPrinter =
struct
  include CS.G
  include DefAttrs
  let vertex_name v = Cfg.bbid_to_string(CS.G.V.label v)
end
module SsaBBidDot = Graph.Graphviz.Dot(SsaBBidPrinter)

module AstBBidPrinter =
struct
  include CA.G
  include DefAttrs
  let vertex_name v = Cfg.bbid_to_string(CA.G.V.label v)
end
module AstBBidDot = Graph.Graphviz.Dot(AstBBidPrinter)

module SsaStmtsAttDot = MakeCfgPrinter (CS) (PrintSsaStmts) (EdgeColorAttributor)

module AstStmtsAttDot = MakeCfgPrinter (CA) (PrintAstStmts) (EdgeColorAttributor)
