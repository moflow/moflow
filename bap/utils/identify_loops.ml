(*
 * Identifies loops for a cfg. Cfg passed on stdin as a dotfile and output is a
 * dotfile emitted to stdout.
 *)

let usage = "Usage: "^Sys.argv.(0)^" [options] <input dot file>\n\
             Identify loops for cfg from file, prints dotfile showing loops"
let algorithm = ref "steensgard"
let entry = ref "BB_Entry"
let infilename = ref None
let oc = ref stdout
let speclist =
  ("-algorithm",
   Arg.Set_string algorithm,
   "<algorithm> Use the given lnf algorithm (default: " ^ !algorithm ^ ")")
  ::("-entry",
     Arg.Set_string entry,
     "<name> Use the node with the given label as the entry node (default: " ^ !entry ^ ")")
  ::("-o",
     Arg.String (fun n -> oc := open_out n),
     "<filename> Write the new graph to filename (default: stdout)")
  ::[]

module G = struct
  include Graph.Builder.I(Graph.Imperative.Digraph.ConcreteLabeled(struct
    type t = string
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end)(struct
    type t = string
    let compare = compare
    let default = "default"
  end))

  let remove_edge g u v = G.remove_edge g u v; g
  let remove_edge_e g e = G.remove_edge_e g e; g

  let v2s = G.V.label
end
module DotParser =
  Graph.Dot.Parse
    (G)
    (struct
      let node (id,_) _ = match id with
        | Graph.Dot_ast.Ident s
        | Graph.Dot_ast.Number s
        | Graph.Dot_ast.String s
        | Graph.Dot_ast.Html s -> s
      let edge _ = "hello"
    end)
module DotPrinter = Lnf.Dot(G)
module Steensgard = Lnf_steensgard.Make(G)
module Havlak = Lnf_havlak.Make(G)
module Sreedhar = Lnf_sreedhar.Make(G)

let find_vertex g s =
  match G.G.fold_vertex
    (fun v o -> match o with
    | Some vert -> Some vert
    | None -> if G.G.V.label v = s then Some v
      else None)
    g None with
  | Some vert -> vert
  | None -> raise Not_found

let n = ref 0
let anon x = match !n with
  | 0 -> infilename := Some x
  | _ -> Arg.usage speclist usage; exit 1
;;

let () = Arg.parse speclist anon usage
let () = if !infilename = None then (
  Arg.usage speclist usage;
  exit 1
);;

let g = DotParser.parse (BatOption.get !infilename)
let start_vertex = find_vertex g !entry
let cfg = match !algorithm with
          | "steensgard" -> Steensgard.lnf g start_vertex
          | "havlak" -> Havlak.lnf g start_vertex
          | "sreedhar" -> Sreedhar.lnf g start_vertex
          | _ -> raise Not_found
let () = output_string !oc ((DotPrinter.to_dot g cfg)^"\n")
