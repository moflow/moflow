(** Simplify predicates so that VSA and other abstract interpretations
    can use them. *)

(** Simplify conditions used in edge labels *)
val simplifycond_cfg : Cfg.AST.G.t -> Cfg.AST.G.t
