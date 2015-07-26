(** Loop nesting forest definitions *)

module type G =
sig
  include Graph.Builder.S

  val remove_edge_e : G.t -> G.E.t -> G.t
  val remove_edge : G.t -> G.V.t -> G.V.t -> G.t
  val v2s : G.V.t -> string
end

(* body is a superset of headers. *)
(* headers, body, and children are all sorted *)
type 'a lnt = { headers: 'a list; body: 'a list; children: 'a lnf }
and 'a lnf = 'a lnt list

val validate_lnf : 'a lnf -> bool
val validate_lnt : 'a lnt -> bool

val string_of_lnf : ('a -> string) -> 'a lnf -> string
val string_of_lnt : ('a -> string) -> 'a lnt -> string

module type MakeType =
  functor (Gr: G) ->
    sig
      val lnf : Gr.G.t -> Gr.G.V.t -> Gr.G.V.t lnf
    end

module Dot : functor(Gr: G) ->
sig
  val to_dot: ?e2s:(Gr.G.E.t -> string) -> Gr.G.t -> Gr.G.V.t lnf -> string
end
