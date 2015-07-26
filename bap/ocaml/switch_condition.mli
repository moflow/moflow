(** Rewrite the outgoing edge conditions from (indirect jump) switches
    so they are in terms of the input variable, and not the target
    destination.

    @author ejs
 *)

(** Adds switch conditions to a SSA graph.  Takes the information
    returned by the VSA CFG recovery analysis as input. Returns a
    modified CFG where as many indirect jumps are rewritten as posible
    and a success flag.  The success flag is true when all indirect
    jumps were rewritten. *)
val add_switch_conditions_disasm : Asmir_disasm.vsaresult -> Cfg.SSA.G.t * bool

(** Like [add_switch_conditions_disasm], but starts from an arbitrary
    SSA CFG.  Re-runs VSA analysis. *)
val add_switch_conditions_ssacfg : Asmir.asmprogram -> Cfg.SSA.G.t -> Cfg.SSA.G.t * bool
