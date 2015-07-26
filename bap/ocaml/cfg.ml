open BatListFull
open Type

module D = Debug.Make(struct let name = "Cfg" and default=`NoDebug end)
open D

(* a label map *)
module LM = Map.Make(struct type t = label let compare=compare end)

type bbid =
  | BB_Entry
  | BB_Exit
  | BB_Indirect
  | BB_Error
  | BB of int

let bbid_to_string = function
  | BB_Entry     -> "BB_Entry"
  | BB_Exit      -> "BB_Exit"
  | BB_Indirect  -> "BB_Indirect"
  | BB_Error     -> "BB_Error"
  | BB n         -> "BB_"^string_of_int n

let edge_direction = function
  | Some(b, _) -> b
  | None -> None

module BBid =
struct
  type t = bbid
  let compare = compare
  let hash = function
    | BB_Entry     ->  -1
    | BB_Exit      ->  -2
    | BB_Indirect  ->  -3
    | BB_Error     ->  -4
    | BB n         ->   n
  let equal = (=)
end

module BS = Set.Make(BBid)
module BH = Hashtbl.Make(BBid)
module BM = Map.Make(BBid)



module type CFG =
sig

  type stmt
  type lang = stmt list
  type exp

  include Graph.Builder.S with type G.V.label = bbid and type G.E.label = (bool option * exp) option


  val find_vertex : G.t -> G.V.label -> G.V.t
  val find_label : G.t -> Type.label -> G.V.t
  val get_stmts : G.t -> G.V.t -> lang
  val default : lang
  val set_stmts : G.t -> G.V.t -> lang -> G.t
  val join_stmts : lang -> lang -> lang
  val lang_to_string : lang -> string
  (*val newid : G.t -> bbid*)
  val create_vertex : G.t -> lang -> G.t * G.V.t
  val copy_map : G.t -> G.t

  (* extra builder-like stuff *)
  val remove_vertex : G.t -> G.V.t -> G.t
  val remove_edge : G.t -> G.V.t -> G.V.t -> G.t
  val remove_edge_e : G.t -> G.E.t -> G.t

  (* val vlabel_to_string : G.V.label -> string *)
  val v2s : G.V.t -> string

end

type ('a,'b,'c) pcfg =
    {
      g: 'a;
      s: 'b;
      l: 'c;
      nextid : int
    }

module type Language =
sig
  type stmt
  type lang = stmt list
  type exp
  val default : lang
  val join : lang -> lang -> lang
  val iter_labels : (label->unit) -> lang -> unit
  val to_string : lang -> string
end

module E(Lang: Language) =
struct
  type t = (bool option * Lang.exp) option
  let compare = compare
  let default = None
end

(* Begin persistent implementation *)
module MakeP (Lang: Language) =
struct
  (* A simple implementation for now... We can worry about optimizing later. *)
  module G' = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(BBid)(E(Lang))

  type lang = Lang.lang
  type stmt = Lang.stmt
  type exp = Lang.exp


  module G = struct

    module V = G'.V
    type vertex = V.t
    module E = G'.E
    type edge = E.t


(*    type t = {
      g: G'.t;
      s: Lang.t BM.t;
      l: V.t LM.t;
      nextid : int
    }
*)
    type t = (G'.t, Lang.lang BM.t, V.t LM.t) pcfg

    let is_directed = true


    (* boring wrappers *)

    let is_empty         x = G'.is_empty        x.g
    let nb_vertex        x = G'.nb_vertex       x.g
    let nb_edges         x = G'.nb_edges        x.g
    let out_degree       x = G'.out_degree      x.g
    let in_degree        x = G'.in_degree       x.g
    let mem_vertex       x = G'.mem_vertex      x.g
    let mem_edge         x = G'.mem_edge        x.g
    let mem_edge_e       x = G'.mem_edge_e      x.g
    let find_edge        x = G'.find_edge       x.g
    let find_all_edges   x = G'.find_all_edges  x.g
    let succ             x = G'.succ            x.g
    let pred             x = G'.pred            x.g
    let succ_e           x = G'.succ_e          x.g
    let pred_e           x = G'.pred_e          x.g

    let iter_vertex  x y = G'.iter_vertex x y.g
    let iter_edges   x y = G'.iter_edges  x y.g
    let fold_vertex  x y = G'.fold_vertex x y.g
    let fold_edges   x y = G'.fold_edges  x y.g
    let iter_edges_e x y = G'.iter_edges_e x y.g
    let fold_edges_e x y = G'.fold_edges_e x y.g
    let iter_succ    x y = G'.iter_succ   x y.g
    let iter_pred    x y = G'.iter_pred   x y.g
    let fold_succ    x y = G'.fold_succ   x y.g
    let fold_pred    x y = G'.fold_pred   x y.g
    let iter_succ_e  x y = G'.iter_succ_e x y.g
    let fold_succ_e  x y = G'.fold_succ_e x y.g
    let iter_pred_e  x y = G'.iter_pred_e x y.g
    let fold_pred_e  x y = G'.fold_pred_e x y.g


    let add_edge c v1 v2    = { c with g = G'.add_edge c.g v1 v2 }
    let add_edge_e c e      = { c with g = G'.add_edge_e c.g e }
    let remove_edge c v1 v2 = { c with g = G'.remove_edge c.g v1 v2 }
    let remove_edge_e c e   = { c with g = G'.remove_edge_e c.g e }
    let add_vertex c v      = { c with g = G'.add_vertex c.g v }

    (* Less boring wrappers *)
    let empty =
      {
        g = G'.empty;
        s = BM.empty;
        l = LM.empty;
        nextid = 0;
      }

    let map_vertex f c =
      failwith "map_vertex: unimplemented"

    let copy_map {s=s; l=l; nextid = nextid} = {g = G'.empty; s=s; l=l; nextid=nextid}

  end (* module G *)

    (* Extra stuff to make this a CFG *)
  let find_vertex c id =
    let v = G.V.create id in
    if G.mem_vertex c v then v else raise Not_found

  let find_label c l = LM.find l c.l

  let get_stmts c v =
    try BM.find v c.s
    with Not_found -> Lang.default

  let default = Lang.default

  let join_stmts = Lang.join

  let lang_to_string = Lang.to_string

    (* helper *)
  let fold_labels f l a =
    let r = ref a in
    Lang.iter_labels (fun l -> r := f l !r) l;
    !r
  let remove_labels c v =
    { c with
      l = fold_labels (fun l lm -> LM.remove l lm) (get_stmts c v) c.l
    }

  let set_stmts c v s =
    let c = remove_labels c v in
    let sm = BM.add v s c.s in
    let lm = fold_labels
      (fun l lm ->
        (* dprintf "Adding label %s to bbid %s" (Pp.label_to_string l) (bbid_to_string (V.label v)); *)
        if debug() && LM.mem l lm then (
            (* wprintf "stmt: %s" (Lang.to_string s); *)
            let oldstmts = get_stmts c (LM.find l lm) in
            let newstmts = s in
            wprintf "Duplicate of %s:\n\noldstmts: %s\n\n newstmts: %s" (Pp.label_to_string l) (Lang.to_string oldstmts) (Lang.to_string newstmts);
            failwith (Printf.sprintf "Duplicate of %s" (Pp.label_to_string l)));
        LM.add l v lm
      ) s c.l in
    { c with l=lm; s=sm }

  let newid c =
    let id = c.nextid in
    if id = -1 then failwith "newid: wrapped around";
    (BB id, {c with nextid = id + 1 })

  let create_vertex c stmts =
    let (i,c) = newid c in
    let v = G.V.create i in
    let c = G.add_vertex c v in
    (set_stmts c v stmts, v)

  let remove_vertex c v =
    let c = remove_labels c v in
    let sm = BM.remove v c.s in
    let g = G'.remove_vertex c.g v in
    { c with g=g; s=sm }



  (* Copied from ocamlgraph's Builder.P so that G doesn't eat our extensions *)
  let empty () = G.empty
  let copy g = g
  let add_vertex = G.add_vertex
  let add_edge = G.add_edge
  let add_edge_e = G.add_edge_e

  (* extra, builder-like stuff *)
  let remove_edge = G.remove_edge
  let remove_edge_e = G.remove_edge_e

  let copy_map = G.copy_map

  let v2s v = bbid_to_string (G.V.label v)

end
(* end persistent implementation *)

module Make = MakeP

module LangAST =
struct
  type lang = Ast.stmt list
  type stmt = Ast.stmt
  type exp = Ast.exp
  let default = []
  let join sl1 sl2 = match List.rev sl1 with
    | Ast.Jmp (e, _) :: sl1' when Ast.lab_of_exp e <> None -> List.append (List.rev sl1') sl2
    | (Ast.Jmp _ as jmp) :: sl1' ->
      let s = Ast.Comment(Printf.sprintf "join: Removed resolved jump to single target: %s" (Pp.ast_stmt_to_string jmp), [Synthetic]) in
      BatList.append (List.rev (s::sl1')) sl2
    | _ -> BatList.append sl1 sl2
  let iter_labels f =
    List.iter (function Ast.Label(l, _) -> f l  | _ -> () )
  let to_string stmts = List.fold_left (fun acc s -> acc^" "^(Pp.ast_stmt_to_string s)) "" stmts
end

module LangSSA =
struct
  type lang = Ssa.stmt list
  type stmt = Ssa.stmt
  type exp = Ssa.exp
  let default = []
  let join sl1 sl2 = match List.rev sl1 with
    | Ssa.Jmp (e, _) :: sl1' when Ssa.lab_of_exp e <> None -> List.append (List.rev sl1') sl2
    | (Ssa.Jmp _ as jmp) :: sl1' ->
      let s = Ssa.Comment(Printf.sprintf "join: Removed resolved jump to single target: %s" (Pp.ssa_stmt_to_string jmp), [Synthetic]) in
      BatList.append (List.rev (s::sl1')) sl2
    | _ -> BatList.append sl1 sl2
  let iter_labels f =
    (* optimization: assume labels are at the beginning *)
    let rec g = function
      | Ssa.Label(l,_) :: xs -> f l; g xs
      | Ssa.Comment _ :: xs -> g xs
      | _ -> ()
    in g
  let to_string stmts = List.fold_left (fun acc s -> acc^" "^(Pp.ssa_stmt_to_string s)) "" stmts
end

module AST = Make(LangAST)
module SSA = Make(LangSSA)

type aststmtloc = AST.G.V.t * int
type ssastmtloc = SSA.G.V.t * int

module type CFG_PRIV =
sig
  type lang
  type exp
  module G' : Graph.Sig.G

  include Graph.Builder.S
    with type G.V.label = bbid
    and type G.E.label = (bool option * exp) option
    and type G.t = (G'.t, lang BM.t, G'.V.t LM.t) pcfg

  val get_stmts  : G.t -> G.V.t -> lang
  val default    : lang
  val set_stmts  : G.t -> G.V.t -> lang -> G.t
end

module MkMap(A:CFG_PRIV)(B:CFG_PRIV) =
struct
  module VM = Map.Make(B.G.V)
  module EM = Map.Make(B.G.E)
  let map conv_stmts conv_exp ({nextid=n} as cfg) =
    let s = B.empty() in
    let t vertex = B.G.V.create (A.G.V.label vertex) in
    let per_edge e (g, em) =
      let new_lab = match A.G.E.label e with
        | Some(b, e) -> Some(b, conv_exp e)
        | None -> None
      in
      let e' = B.G.E.create (t (A.G.E.src e)) new_lab (t (A.G.E.dst e)) in
      let em = EM.add e' e em in
      B.add_edge_e g e', em
    in
    let per_vertex v (g, vm) =
      let v' = t v in
      let g = B.add_vertex g v' in
      let vm = VM.add v' v vm in
      B.set_stmts g v' (conv_stmts (A.get_stmts cfg v)), vm
    in
    let s, vm = A.G.fold_vertex per_vertex cfg (s, VM.empty) in
    let s, em = A.G.fold_edges_e per_edge cfg (s, EM.empty) in
    { s with nextid = n }, (fun v -> VM.find v vm), (fun e -> EM.find e em)
end

module M2ssa = MkMap(AST)(SSA)
module M2ast = MkMap(SSA)(AST)

let map_ast2ssa = M2ssa.map
let map_ssa2ast = M2ast.map

