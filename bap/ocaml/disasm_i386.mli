exception Disasm_i386_exception of string

type binopf = Ast.exp -> Ast.exp -> Ast.exp (* below: opcode *)

type mode = X86 | X8664             (* ocaml/arch.ml *)
val type_of_mode : mode -> Type.typ (* ocaml/syscall_models.ml *)

type order                      (* below: opcode *)
type direction
type operand
type jumptarget
module Pcmpstr :
  sig
    type imm8cb
    type pcmpinfo
  end
type offsetinfo

type opcode =             (* tests/disasm_i386_suite.ml needs Disasm_i386.Nop *)
    Bswap of (Type.typ * operand)
  | Retn of (Type.typ * operand) option * bool
  | Nop
  | Mov of Type.typ * operand * operand * Ast.exp option
  | Movs of Type.typ
  | Movzx of Type.typ * operand * Type.typ * operand
  | Movsx of Type.typ * operand * Type.typ * operand
  | Movdq of Type.typ * operand * Type.typ * operand * bool
  | Movoffset of (Type.typ * operand) * offsetinfo list
  | Lea of Type.typ * operand * Ast.exp
  | Call of operand * Big_int_convenience.address
  | Shift of Type.binop_type * Type.typ * operand * operand
  | Shiftd of Type.binop_type * Type.typ * operand * operand * operand
  | Rotate of Type.binop_type * Type.typ * operand * operand * bool
  | Bt of Type.typ * operand * operand
  | Bs of Type.typ * operand * operand * direction
  | Jump of jumptarget
  | Jcc of jumptarget * Ast.exp
  | Setcc of Type.typ * operand * Ast.exp
  | Hlt
  | Cmps of Type.typ
  | Lods of Type.typ
  | Scas of Type.typ
  | Stos of Type.typ
  | Push of Type.typ * operand
  | Pop of Type.typ * operand
  | Pushf of Type.typ
  | Popf of Type.typ
  | Popcnt of Type.typ * operand * operand
  | Sahf
  | Lahf
  | Add of (Type.typ * operand * operand)
  | Adc of (Type.typ * operand * operand)
  | Inc of Type.typ * operand
  | Dec of Type.typ * operand
  | Sub of (Type.typ * operand * operand)
  | Sbb of (Type.typ * operand * operand)
  | Cmp of (Type.typ * operand * operand)
  | Cmpxchg of (Type.typ * operand * operand)
  | Cmpxchg8b of operand
  | Xadd of (Type.typ * operand * operand)
  | Xchg of (Type.typ * operand * operand)
  | And of (Type.typ * operand * operand)
  | Or of (Type.typ * operand * operand)
  | Xor of (Type.typ * operand * operand)
  | Test of (Type.typ * operand * operand)
  | Ptest of (Type.typ * operand * operand)
  | Not of (Type.typ * operand)
  | Neg of (Type.typ * operand)
  | Mul of (Type.typ * operand)
  | Imul of Type.typ * (bool * operand) * operand * operand
  | Div of Type.typ * operand
  | Idiv of Type.typ * operand
  | Cld
  | Rdtsc
  | Cpuid
  | Xgetbv
  | Stmxcsr of operand
  | Ldmxcsr of operand
  | Fnstcw of operand
  | Fldcw of operand
  | Fld of operand
  | Fst of (operand * bool)
  | Punpck of
      (Type.typ * Type.typ * order * operand * operand * operand option)
  | Ppackedbinop of
      (Type.typ * Type.typ * binopf * string * operand * operand *
       operand option)
  | Pbinop of
      (Type.typ * binopf * string * operand * operand * operand option)
  | Pmov of
      (Type.typ * Type.typ * Type.typ * operand * operand * Type.cast_type *
       string)
  | Pmovmskb of (Type.typ * operand * operand)
  | Pcmp of
      (Type.typ * Type.typ * Type.binop_type * string * operand * operand *
       operand option)
  | Palignr of (Type.typ * operand * operand * operand option * operand)
  | Pcmpstr of
      (Type.typ * operand * operand * operand * Pcmpstr.imm8cb *
       Pcmpstr.pcmpinfo)
  | Pshufb of Type.typ * operand * operand * operand option
  | Pshufd of Type.typ * operand * operand * operand option * operand
  | Leave of Type.typ
  | Interrupt of operand
  | Interrupt3
  | Sysenter
  | Syscall

type prefix                     (* below: parse_instr *)

val regs_x86 : Var.t list      (* ocaml/asmir_vars.ml *)
val regs_x86_64 : Var.t list
val regs_full : Var.t list

module R32 :                    (* everywhere *)
  sig
    val eax : Var.t
    val ecx : Var.t
    val edx : Var.t
    val ebx : Var.t
    val esp : Var.t
    val ebp : Var.t
    val esi : Var.t
    val edi : Var.t
    val eip : Var.t
    val mem : Var.t
    val xmms : Var.t array
    val eflags : Var.t
    val fs_base : Var.t
    val gs_base : Var.t
  end

module R64 :                    (* everywhere *)
  sig
    val rax : Var.t
    val rcx : Var.t
    val rdx : Var.t
    val rbx : Var.t
    val rsp : Var.t
    val rbp : Var.t
    val rsi : Var.t
    val rdi : Var.t
    val rip : Var.t
    val mem : Var.t
    val r8 : Var.t
    val r9 : Var.t
    val r10 : Var.t
    val r11 : Var.t
    val r12 : Var.t
    val r13 : Var.t
    val r14 : Var.t
    val r15 : Var.t
    val ymms : Var.t array
    val rflags : Var.t
    val fs_base : Var.t
    val gs_base : Var.t
  end

val st : Var.t array
val cf : Var.t
val pf : Var.t
val af : Var.t
val zf : Var.t
val sf : Var.t
val oF : Var.t
val df : Var.t

val cs : Var.t
val ds : Var.t
val es : Var.t
val fs : Var.t
val gs : Var.t
val ss : Var.t

val mxcsr : Var.t

module ToIR :
  sig
    val add_labels :            (* ocaml/asmir.ml *)
      ?asm:string ->
      Big_int_convenience.address -> Ast.stmt list -> Ast.stmt list
  end

val parse_instr :               (* tests/disasm_i386_suite.ml *)
  mode ->
  (Big_int_convenience.address -> char) ->
  Big_int_convenience.address ->
  int list * prefix * opcode * Big_int_convenience.address

val disasm_instr :              (* ocaml/disasm.ml *)
  mode ->
  (Big_int_convenience.address -> char) ->
  Big_int_convenience.address -> Ast.stmt list * Big_int_convenience.address
