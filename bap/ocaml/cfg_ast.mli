(** Conversions from AST programs to AST CFGs and vice versa. *)

type unresolved_edge = Cfg.AST.G.V.t * Cfg.AST.G.E.label * Ast.exp

(** [of_prog p] converts [p] to an AST CFG. *)
val of_prog : ?special_error:bool -> Ast.program -> Cfg.AST.G.t

(** [to_prog cfg] converts [cfg] to an AST program. *)
val to_prog : Cfg.AST.G.t -> Ast.program

(** {3 CFG Manipulation Functions} *)

(** Add [BB_Entry] in a graph. *)
val create_entry : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t

(** Find [BB_Entry] in a graph.

    @raises [Failure] if not already present. *)
val find_entry : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t

(** Find [BB_Error] in a graph, or add it if not already present. *)
val find_error : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t

(** Find [BB_Exit] in a graph, or add it if not already present. *)
val find_exit : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t

(** Find [BB_Indirect] in a graph, or add it if not already present. *)
val find_indirect : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t

(** {3 Convenience Functions} *)
(** [v2s v] returns the basic blocker identifier string associated
    with [v]. *)
val v2s : Cfg.AST.G.V.t -> string

(** {3 Internal Functions} *)
(** [add_prog cfg p] adds the AST program [p] into the existing AST
    CFG [cfg]. It returns a tuple consisting of the updated CFG, new
    postponed edges, newly added BBs (the first node is the entry),
    and the node through which [p] can fall-through (fall off the end
    of the program), if one exists.

    This function exists to enable incremental lifting, and should not
    be used for other applications.
*)
val add_prog : ?special_error:bool -> Cfg.AST.G.t -> Ast.program -> (Cfg.AST.G.t * unresolved_edge list * Cfg.AST.G.V.t list * Cfg.AST.G.V.t option)
