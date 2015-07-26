(** Sanity checks to provide more understandable error messages

    To disable all sanity checks, disable debugging on the [Checks]
    module using the facilities in the [Debug] module.

    @author Ed Schwartz
*)

exception Sanity of string

(** Sanity checks take a function-specific argument type, and a string
    describing the calling code.  If a sanity check fails, it will
    raise an exception with an error message including the passed
    string.
*)
type 'a sanityf = 'a -> string -> unit

(** {3 CFG checks} *)

(** [connected_astcfg g s] raises an exception iff g is not a connected graph. *)
val connected_astcfg : Cfg.AST.G.t sanityf

(** [connected_ssacfg g s] raises an exception iff g is not a connected graph. *)
val connected_ssacfg : Cfg.SSA.G.t sanityf

(** Build a connected check for other graphs *)
module MakeConnectedCheck :
  functor (C : Cfg.CFG) ->
    sig
      (** [connected_check g s] raises an exception iff g is not a connected graph. *)
      val connected_check : C.G.t sanityf
    end

(** [acyclic_astcfg g s] raises an exception iff g is not an acyclic
    graph. *)
val acyclic_astcfg : Cfg.AST.G.t sanityf

(** [acyclic_ssacfg g s] raises an exception iff g is not an acyclic
    graph. *)
val acyclic_ssacfg : Cfg.SSA.G.t sanityf

(** Build an acyclic check for other graphs *)
module MakeAcyclicCheck :
  functor (C : Cfg.CFG) ->
    sig
      (** [acyclic_check g s] raises an exception iff g is not an
          acyclic graph. *)
      val acyclic_check : C.G.t sanityf
    end

(** [exit_astcfg g s] raises an exception iff g contains an
    unspecified exit node.  Allowed and expected exit node bbids can
    be specified using [allowed_exits]. *)
val exit_astcfg : ?allowed_exits:(Cfg.bbid list) -> ?expected_exits:(Cfg.bbid list) -> Cfg.AST.G.t sanityf

(** [exit_ssacfg g s] raises an exception iff g contains an
    unspecified exit node.  Allowed and expected exit node bbids can
    be specified using [allowed_exits]. *)
val exit_ssacfg : ?allowed_exits:(Cfg.bbid list) -> ?expected_exits:(Cfg.bbid list) -> Cfg.SSA.G.t sanityf

(** Build an exit check for other graphs *)
module MakeExitCheck :
  functor (C : Cfg.CFG) ->
    sig
      (** [exit_check g s] raises an exception iff g contains an
          unspecified exit node.  Allowed and expected exit node bbids
          can be specified using [allowed_exits]. *)
      val exit_check : ?allowed_exits:(Cfg.bbid list) -> ?expected_exits:(Cfg.bbid list) -> C.G.t sanityf
    end

(** [indirect_astcfg g s] raises an exception iff g contains
    [BB_Indirect], the node corresponding to an unresolved indirect
    jump. *)
val indirect_astcfg : Cfg.AST.G.t sanityf

(** [indirect_ssacfg g s] raises an exception iff g contains
    [BB_Indirect], the node corresponding to an unresolved indirect
    jump. *)
val indirect_ssacfg : Cfg.SSA.G.t sanityf

(** Build an indirect check for other graphs *)
module MakeIndirectCheck :
  functor (C : Cfg.CFG) ->
    sig
      (** [indirect_check g s] raises an exception iff g contains
          [BB_Indirect], the node corresponding to an unresolved indirect
          jump. *)
      val indirect_check : C.G.t sanityf
    end
