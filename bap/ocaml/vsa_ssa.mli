(** Value-Set Analysis / Value-Set Arithmetic *)

module SI :
  sig
    type t = int * Big_int_Z.big_int * Big_int_Z.big_int * Big_int_Z.big_int
    val is_empty : t -> bool
    val to_string : t -> string
  end
(** Strided intervals *)

module VS :
  sig
    type region = Ssa.var
    type address = region * SI.t
    type t = address list
    val global : region
    val to_string : t -> string
    val concrete : ?max:int -> t -> Big_int_Z.big_int list option
  end
(** Value sets *)

module MemStore :
  sig
    module M1 : Map.S with type key = Var.t
    module M2 : Map.S with type key = Big_int_Z.big_int
    type t = VS.t M2.t M1.t
end
(** Memories *)

module AbsEnv :
  sig
    type value = [ `Array of MemStore.t | `Scalar of VS.t ]
    type t = value Var.VarMap.t
    val pp : (string -> unit) -> t -> unit
    val value_to_string : value -> string
  end
(** Abstract environments *)

val exp2vs : AbsEnv.t -> Ssa.exp -> VS.t
(** Approximate an expression using value sets in an abstract
    environment *)

val prepare_ssa_indirect : ?vs:Cfg.SSA.G.V.t list -> Cfg.SSA.G.t -> Cfg.SSA.G.t
(** Prepare SSA CFG for resolving indirect jumps *)

val vsa :
  ?nmeets:int ->
  Vsa.options ->
  Cfg.SSA.G.t ->
  (Cfg.SSA.G.V.t * int -> AbsEnv.t option) *
    (Cfg.SSA.G.V.t * int -> AbsEnv.t option)
(** Main VSA interface.  Returns functions for computing abstract
    environments before and after the given location. *)

val last_loc :
  Cfg.SSA.G.t ->
  Cfg.SSA.G.V.t -> Cfg.SSA.G.V.t * int
(** Returns the last location in a basic block. *)

val build_default_arch_options : Arch.arch -> Vsa.options
(** Build default options for arch *)

val build_default_prog_options : Asmir.asmprogram -> Vsa.options
(** Build default options for program *)
