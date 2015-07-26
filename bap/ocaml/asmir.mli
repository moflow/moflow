(** High level interface to libasmir.

    The functions in this file should be used instead of calling Libasmir functions
    directly. These functions should be easier to use, and, unlike the Libasmir
    ones, will handle garbage collection.

    @author Ivan Jager
*)

(* This interface is a work-in-progress. I just created it now to avoid exposing
   some variables I added. --aij
*)

open Arch
open Libbfd
open Libasmir

exception Memory_error
exception Disassembly_error

type asmprogram

val arch_i386 : arch
val arch_x8664 : arch

type varctx

val gamma_create : Var.t -> Var.t list -> varctx
val gamma_lookup : varctx -> string -> Var.t

(*
val gamma_extend : varctx -> string -> Ast.decl -> unit
val gamma_unextend : varctx -> string -> unit
*)

(*
val tr_exp : varctx -> Libasmir.exp -> Ast.exp
val tr_binop :
  varctx ->
  Libasmir.binop_type_t -> Libasmir.exp -> Libasmir.exp -> Ast.exp
val tr_vardecl : varctx -> Libasmir.stmt -> Var.t * (unit -> unit)
val tr_vardecls :
  varctx -> Libasmir.stmt list -> Var.t list * (unit -> unit)
val tr_stmt : varctx -> Libasmir.stmt -> Ast.stmt
val tr_bap_block_t :
  varctx -> asmprogram -> Libasmir.bap_block_t -> Ast.stmt list
val tr_bap_blocks_t :
  varctx ->
  asmprogram -> Libasmir.bap_blocks_t -> Ast.stmt list
*)


val decls_for_arch : arch -> Ast.var list
val gamma_for_arch : arch -> varctx

val get_asmprogram_arch : asmprogram -> arch

val x86_mem : Var.t
val x86_regs : Var.t list
val x64_mem : Var.t
val x64_regs : Var.t list
val x86_all_regs : Var.t list
val x64_all_regs : Var.t list
val multiarch_all_regs : Var.t list

val all_regs : arch -> Var.t list

(** Convert a BAP architecture to a BFD architecture and machine *)
val arch_to_bfd : arch -> Libbfd.bfd_architecture * Libbfd.machine_t

(** Translate libtrace architecture to BAP architecture *)
val translate_trace_arch : Trace.Arch.bfd_architecture -> Trace.Arch.machine_t -> arch

val open_program : ?base:Type.addr -> ?target:string -> string -> asmprogram
val asmprogram_to_bap : ?init_ro:bool -> asmprogram -> Ast.program
val asm_addr_to_bap : (*varctx ->*) asmprogram -> Type.addr -> Ast.program * Type.addr

val asmprogram_to_bap_range : ?init_ro:bool -> asmprogram -> Type.addr -> Type.addr  -> Ast.program

(** [bap_fully_modeled p] returns [true] when [p] represents a fully
    lifted BAP program. [bap_lift_success p] will return [false] if an
    unmodeled instruction or system call is encountered. *)
val bap_fully_modeled : Ast.program -> bool

(** Load entire trace into memory at once.  If pin is true, loads a
    PinTrace.  If pin is false, loads an old, TEMU-based trace format. *)
(* val bap_from_trace_file : ?atts:bool -> ?pin:bool -> string -> Ast.program *)
(** Load entire trace into memory from the new SerializedTrace format. *)
val serialized_bap_from_trace_file : string -> Ast.program * arch

(** Open a PinTrace/TEMU-based trace in streaming format depending on the value of [pin]. *)
(* val bap_stream_from_trace_file : ?atts:bool -> ?rate:int64 -> ?pin:bool -> string -> (Ast.stmt list) Stream.t *)
(** Open a SerializedTrace trace in streaming format. *)
val serialized_bap_stream_from_trace_file : int64 -> string -> (unit -> unit -> unit) * (Ast.stmt list) Stream.t * Arch.arch

val get_symbols : ?all:bool -> asmprogram -> asymbol array
val get_dynamic_symbols : asmprogram -> asymbol array
val find_symbol : asmprogram -> string -> asymbol

val get_flavour : asmprogram -> bfd_flavour

val get_all_asections : asmprogram -> section_ptr array

val get_section_startaddr : asmprogram -> string -> Type.addr
val get_section_endaddr : asmprogram -> string -> Type.addr

(** Lowest address of program in memory *)
val get_base_address : asmprogram -> Type.addr
(** Start address of program *)
val get_start_addr : asmprogram -> Type.addr

val get_asm_instr_string : asmprogram -> Type.addr -> string
val get_asm_instr_string_range : asmprogram -> Type.addr -> Type.addr -> string

val is_load : section_ptr -> bool
val is_code : section_ptr -> bool

val byte_insn_to_bap : arch -> Type.addr -> char array -> Ast.program * Type.addr

val byte_sequence_to_bap : char array -> arch -> Type.addr -> Ast.program list

(* val set_print_warning : bool -> unit *)

(* val get_print_warning : unit -> bool *)

(* val set_use_simple_segments : bool -> unit *)

(** [get_exec_mem_contents p] returns a function [f] such that [f
    addr] returns the executable byte in memory at [addr] if one exists.
    If no such byte exists, @raises {!Memory_error}. *)
val get_exec_mem_contents : asmprogram -> Type.addr -> char

(** [get_exec_mem_contents_list p] returns a list of [(addr, byte)]
    tuples indicating the executable memory at [addr] is [byte]. *)
val get_exec_mem_contents_list : asmprogram -> (Type.addr * char) list

(** [get_readable_mem_contents] is like {!get_exec_mem_contents} but
    for any readable memory. *)
val get_readable_mem_contents : asmprogram -> Type.addr -> char

(** [get_readable_mem_contents_list p] is like
    {!get_exec_mem_contents_list} but for any readable memory. *)
val get_readable_mem_contents_list : asmprogram -> (Type.addr * char) list
