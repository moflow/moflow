open Lnf

module type RamalingamHelper = 
  functor (Gr: G) ->
    sig
      type t
      val init: Gr.G.t -> Gr.G.V.t -> t
      val find_headers: t -> Gr.G.V.t list -> Gr.G.V.t list
    end

module Make : functor (RH: RamalingamHelper) -> MakeType

