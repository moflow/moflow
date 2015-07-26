(** Copy propagation analysis. *)

(** Compute copy propagation for SSA CFGs. Returns a tuple
    [cm,rm,lookup]. [cm] maps all variables with known copy
    propagations to their copy propagated expression.  [rm] maps all
    variables to their known constant expression, if any, without copy
    propagating any subexpressions. [lookup e] will perform copy
    propagation on all variables in [e]. If it propagates some
    expression e' and [stop_before e'] is true, e' will not be
    included in the final propagated expression.  Similarly if
    [stop_after e'] holds e' will be in the final propagated
    expression, but propagation will not occur on subexpressions of
    [e']. *)
val copyprop_ssa : ?stop_before:(Ssa.exp -> bool) -> ?stop_after:(Ssa.exp -> bool) -> Cfg.SSA.G.t -> Ssa.exp Var.VarMap.t lazy_t * Ssa.exp Var.VarMap.t * (Ssa.exp -> Ssa.exp)

(** Same as {!copyprop_ssa} but for AST CFGs. Note that analysis
    results for AST CFGs are not global, and so the analysis returns
    information before the specified program statement. The last tuple
    returned is a map from variables to their constant expression, if
    one exists, and the location of the assignment. *)
val copyprop_ast : ?stop_before:(Ast.exp -> bool) -> ?stop_after:(Ast.exp -> bool) -> Cfg.AST.G.t -> (Cfg.aststmtloc -> Ast.exp Var.VarMap.t * (Cfg.aststmtloc * Ast.exp) Var.VarMap.t * (Ast.exp -> Ast.exp))

(** [get_vars g l] uses copy propagation to return a set of variables
    used in the computation of any expression in [l] in the graph
    [g]. *)
val get_vars : ?stop_before:(Ssa.exp -> bool) -> ?stop_after:(Ssa.exp -> bool) -> Cfg.SSA.G.t -> Ssa.exp list -> Var.VarSet.t
