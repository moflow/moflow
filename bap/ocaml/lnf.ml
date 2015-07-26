module type G =
sig
  include Graph.Builder.S

  val remove_edge_e : G.t -> G.E.t -> G.t
  val remove_edge : G.t -> G.V.t -> G.V.t -> G.t
  val v2s : G.V.t -> string
end

(* body is a superset of headers. *)
type 'a lnt = { headers: 'a list; body: 'a list; children: 'a lnf }
and 'a lnf = 'a lnt list

let rec is_sorted l =
  match l with
  | [] -> true
  | [v] -> true
  | u::v::rest -> u <= v && is_sorted (v::rest)

(* invariant: sub is sorted and sup is sorted. *)
let rec is_subset sub sup =
  match sub, sup with
  | [], _ -> true
  | x::_, [] -> false
  | x::subrest, y::suprest when x = y -> is_subset subrest suprest
  | _, y::suprest -> is_subset sub suprest

let rec setminus a b =
  match a, b with
  | _, [] -> a
  | [], _ -> []
  | x::arest, y::brest when x = y -> setminus arest brest
  | x::arest, y::brest when x < y -> x::(setminus arest b)
  | x::arest, y::brest (* when x > y *) -> setminus a brest

let rec validate_lnf lnf =
  is_sorted lnf && List.fold_left (fun p lnt -> p && validate_lnt lnt) true lnf
and validate_lnt lnt =
  is_sorted lnt.headers
  && is_sorted lnt.body
  && is_subset lnt.headers lnt.body
  && validate_lnf lnt.children

let rec string_of_list print_fun l =
  match l with
  | [] -> ""
  | [v] -> print_fun v
  (* TODO(awreece) Use ocaml list syntax? *)
  | v::rest -> (print_fun v) ^ ", " ^ string_of_list print_fun rest

let rec string_of_lnf print_fun lnf =
  match lnf with
  | [] -> ""
  | _ -> "LNF(" ^ (string_of_list (string_of_lnt print_fun) lnf) ^ ")"
and string_of_lnt print_fun lnt =
  "LNT(headers=" ^ (string_of_list print_fun lnt.headers) ^
  "; body=" ^ (string_of_list print_fun lnt.body) ^
  "; children=" ^ (string_of_lnf print_fun lnt.children) ^
  ")"

module type MakeType =
  functor (Gr: G) ->
    sig
      val lnf : Gr.G.t -> Gr.G.V.t -> Gr.G.V.t lnf
    end

module Dot(Gr: G) =
struct
  let to_dot ?(e2s=(fun _ -> "")) graph lnf =
    let module VS = Set.Make(Gr.G.V) in
    let module H = Hashtbl.Make(Gr.G.V) in
    (*
     * A map from vertex -> the set of nodes that are a header of it
     * for some loop. This allows us to identify loop back edges.
     *)
    let h = H.create (Gr.G.nb_vertex graph) in
    let loops_processed = ref 0 in

    (*
     * process_lnf: 'a lnf -> 'a list -> ('a list, string)
     * process_lnf lnf processed_headers returns the tuple of:
     *    - The list of body nodes processed so far.
     *    - The string representation of the forest.
     *)
    let rec process_lnf lnf processed_headers =
      List.fold_left
        (fun (backs,string) lnt ->
          let nb, ns = process_lnt lnt processed_headers in
          nb @ backs, ns ^ string)
        ([],"") lnf
    (*
     * process_lnt: 'a lnt -> 'a list -> ('a list, string)
     * process_lnt lnt processed_headers returns the tuple of:
     *    - The list of body nodes processed so far.
     *    - The string representation of the tree.
     *)
    and process_lnt lnt processed_headers =
      (* Emit all headers that haven't been emitted yet. *)
      let header_nodes = setminus lnt.headers processed_headers in
      let string_of_header node = (Gr.v2s node) ^ " [shape=box];\n" in
      let headers_string =
        List.fold_left (fun p n -> p ^ (string_of_header n)) "" header_nodes in

      (*
       * We need to be able to uniquely identify each loop for dot. We do this
       * by incrememnting a ref before we recurse.
       *)
      let loop_num = !loops_processed in
      let _ = loops_processed := !loops_processed + 1 in
      let processed_body_nodes, substring = process_lnf lnt.children
                                                        lnt.headers in

      (*
       * Emit all body nodes that are unique to this loop and aren't a header.
       *)
      let body_nodes = setminus lnt.body processed_body_nodes in
      let body_nodes = setminus body_nodes header_nodes in
      let string_of_body node = (Gr.v2s node) ^ ";\n" in
      let bodys_string =
        List.fold_left (fun p n -> p ^ (string_of_body n)) "" body_nodes in

      (*
       * In order to figure out which edges are loop back edges later, we need
       * to record for each body node the set of headers that dominate it.
       *)
      let add_header_relationship vertex header =
        let () = if not (H.mem h vertex)
                 then H.add h vertex VS.empty
                 else () in
        let s = H.find h vertex in
        H.replace h vertex (VS.add header s) in
      let () = List.iter (fun v -> List.iter (add_header_relationship v)
                                             lnt.headers)
                         lnt.body in

      let string = "subgraph cluster_" ^ (string_of_int loop_num) ^ " {\n"
                 ^ "graph[style=dotted];\n"
                 ^ headers_string ^ substring ^ bodys_string
                 ^ "}\n" in
      (lnt.body, string) in

    (*
     * Initiation the top level call to "process_lnf" and figure out all the
     * subgraphs.
     *)
    let _, subgraphs_string = process_lnf lnf [] in
    let string_of_edge edge =
      let src = Gr.G.E.src edge in
      let dst = Gr.G.E.dst edge in
      let headers_of_src = if H.mem h src then H.find h src else VS.empty in
      (*
       * To keep the output pretty, loopback edges shouldn't constrain the
       * layout.
       * *)
      let constraining_edge = not (VS.mem dst headers_of_src) in
      (Gr.v2s src) ^ " -> " ^ (Gr.v2s dst) ^
      " [" ^ (if constraining_edge then "" else "constraint=false,")
           ^ "label=\"" ^ (e2s edge) ^ "\"];\n" in

    (* And finally, the return value! *)
    "digraph top {\n" ^
    subgraphs_string ^
    (Gr.G.fold_edges_e (fun e p -> p ^ (string_of_edge e)) graph "") ^
    "}"
end
