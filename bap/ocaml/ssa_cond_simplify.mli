(** Simplify predicates so that VSA and other abstract interpretations
    can use them. *)

(** Simplify conditions used in edge labels *)
val simplifycond_ssa : Cfg.SSA.G.t -> Cfg.SSA.G.t

(** Simplify conditions used in edge labels. This version is for
    resolving the target expressions passed as arguments.  It does so
    by never copy propagating beyond one of the variables used in any
    of the target expressions.  This should ensure that the simplified
    conditions are "in terms of" variables in the target expression.
    This is important for VSA cfg recovery. *)
val simplifycond_targets_ssa : Ssa.exp list -> Cfg.SSA.G.t -> Cfg.SSA.G.t
