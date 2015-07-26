(** 
    Static Single Assignment translation

    @todo Export trans_cfg, after finding a good name for it.

    @author Ivan Jager
*)

val v2s : Cfg.SSA.G.V.t -> string

val of_astcfg : ?tac:bool -> Cfg.AST.G.t -> Cfg.SSA.G.t
(** Translates an AST CFG into a SSA CFG. *)

val of_ast : ?tac:bool -> Ast.program -> Cfg.SSA.G.t
(** Translates an AST program into an SSA CFG. *)

val to_astcfg : ?remove_temps:bool -> ?dsa:bool -> Cfg.SSA.G.t -> Cfg.AST.G.t
(** Convert a SSA CFG to an AST CFG. *)

val to_ast : ?remove_temps:bool -> Cfg.SSA.G.t -> Ast.program
(** Convert a SSA CFG to an AST program. *)

type cfg_translation_results = {
  ssacfg : Cfg.SSA.G.t;
  to_ssaexp: Cfg.aststmtloc -> Ast.exp -> Ssa.exp; (** Maps CFG location and expression to equivalent SSA expression *)
  to_ssavar: Var.t -> Var.t; (** Maps AST vars to SSA at end of exit node. *)
  to_astvar: Var.t -> Var.t; (** Maps SSA vars back to the variable they came from *)
  to_astloc: Var.t -> Cfg.aststmtloc; (** Maps non-phi SSA vars to the location of the AST definition *)
}
(** The translated SSA CFG and three maps. [to_ssavar] maps from the
    original AST variables to the corresponding SSA variable at the
    end of the exit node.  [to_astvar] maps from SSA variables to the
    variables they originally came from.  [to_astloc] maps non-phi SSA
    variables to the definition location they originally came from.
    Both [to_astvar] and [to_ssavar] act like the identity function
    for variables that don't map to anything. (This is to avoid
    raising exceptions for corner cases, such as when variables are
    never assigned.)  [to_astloc] raises the [Not_found] exception.
*)

val trans_cfg : ?tac:bool -> Cfg.AST.G.t -> cfg_translation_results
(** Translates an AST CFG into SSA form. *)

type ssa_translation_results = {
  cfg : Cfg.AST.G.t;
  to_astexp: Ssa.exp -> Ast.exp; (** Maps SSA expressions to AST expressions. *)
}

val trans_ssacfg : ?remove_temps:bool -> ?dsa:bool -> Cfg.SSA.G.t -> ssa_translation_results
(** Translates a SSA CFG to an AST CFG. *)

val do_tac_ssacfg : Cfg.SSA.G.t -> Cfg.SSA.G.t
(** Put SSA CFG in three address code form without adding phis. *)
