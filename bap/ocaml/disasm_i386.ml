(** Native lifter of x86 instructions to the BAP IL *)

open Int64
open Ast
open BatPervasives
open Big_int_Z
open Big_int_convenience
open Type
open BatListFull

(* Purposefully placed below BatPervasives *)
open Ast_convenience

module VH=Var.VarHash

module D = Debug.Make(struct let name = "Disasm_i386" and default=`NoDebug end)
open D

let compute_segment_bases = ref false

(* Note: In general, the function g is the get memory function.  The
   variable na refers to the next address or next instruction.

   To help understand this file, please refer to the Intel Instruction
   Set Reference. For consistency, any section numbers here are wrt
   Order Number: 253666-035US June 2010 and 253667-035US.


   The x86 instruction format is as follows:
   Instruction Prefixes: 0-4bytes (1 byte per prefix)
   Optional Rex Prefix: 1 byte
   Opcode: 1 - 3 bytes.
   ModR/M: 1 optional byte
   SIB: 1 optional byte
   Displacement: 0,1,2, or 4 bytes.
   Immediate: 0,1,2, or 4 bytes

   ModR/M has the following format:
   7:6 Mod
   5:3 Reg or extra opcode bits
   2:0 R/M

   SIB:
   7:6 Scale
   5:3 Index
   2:0 Base


   In order to get the most common unsupported opcodes, you can run something like:
   for f in bin/*; do BAP_DEBUG_MODULES=AsmirV ~/bap/trunk/utils/iltrans -bin $f ; done 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n

   To optimize for number of programs disassembled:
   for f in bin/*; do echo -n "$f "; BAP_DEBUG_MODULES=AsmirV iltrans -bin $f 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n  | wc -l; done | sort -n -k 2

*)

(* type segment = CS | SS | DS | ES | FS | GS *)

exception Disasm_i386_exception of string

type binopf = Ast.exp -> Ast.exp -> Ast.exp

type mode = X86 | X8664
let type_of_mode = function
  | X86 -> Reg 32
  | X8664 -> Reg 64
let width_of_mode mode = Typecheck.bits_of_width (type_of_mode mode)

type order = Low | High

type direction = Forward | Backward

type operand =
  | Oreg of int
  | Ovec of int
  | Oseg of int
  | Oaddr of Ast.exp
  | Oimm of big_int

type jumptarget =
  | Jabs of operand
  | Jrel of Type.addr * Type.addr (* next ins address, offset *)

(* See section 4.1 of the Intel® 64 and IA-32 Architectures Software
   Developer’s Manual, Volumes 2A & 2B: Instruction Set Reference
   (order numbers 253666 and 253667) *)
module Pcmpstr = struct

  type ssize = Bytes | Words
  let ssize_to_string = function
    | Bytes -> "Bytes"
    | Words -> "Words"
  type ssign = Signed | Unsigned
  let ssign_to_string = function
    | Signed -> "Signed"
    | Unsigned -> "Unsigned"
  type agg = EqualAny | Ranges | EqualEach | EqualOrdered
  let agg_to_string = function
    | EqualAny -> "EqualAny"
    | Ranges -> "Ranges"
    | EqualEach -> "EqualEach"
    | EqualOrdered -> "EqualOrdered"
  type outselectsig = LSB | MSB (* For PCMPESTRI/PCMPISTRI, choosees LSB or MSB.  *)
  let outselectsig_to_string = function
    | LSB -> "LSB"
    | MSB -> "MSB"
  type outselectmask = Bitmask | Bytemask (* For PCMPESTRM/PCMPISTRM, represents bit mask/word mask. *)
  let outselectmask_to_string = function
    | Bitmask -> "Bitmask"
    | Bytemask -> "Bytemask"

  let sig_to_mask = function
    | LSB -> Bitmask
    | MSB -> Bytemask

  (* See Section 4.1 of Intel manual for more
     information on the immediate control byte.

     i[0]:
     0 = 16 packed bytes
     1 =  8 packed words
     i[1]:
     0 = packed elements are unsigned
     1 = packed elements are signed
     i[3:2]:
     00 = "equal any"
     01 = "ranges"
     10 = "each each"
     11 = "equal ordered"
     i[4]:
     0 = IntRes1 unmodified
     1 = IntRes1 is negated (1's complement)
     i[5]:
     0 = Negation of IntRes1 is for all 16 (8) bits
     1 = Negation of IntRes1 is masked by reg/mem validity
     i[6]:
     0 = Use least significant bit for IntRes2
     1 = Use most significant bit for IntRes2
     i[7]: Undefined, set to 0.
  *)
  type imm8cb = {
    ssize : ssize;
    ssign : ssign;
    agg : agg;
    negintres1 : bool;
    maskintres1 : bool;
    outselectsig : outselectsig;
    outselectmask : outselectmask;
  }

type out = Index | Mask
let out_to_string = function
  | Index -> "Index"
  | Mask -> "Mask"
type len = Implicit | Explicit
let len_to_string = function
  | Implicit -> "Implicit"
  | Explicit -> "Explicit"

(** Information about the type of pcmp instruction. *)
type pcmpinfo = {
  out : out;
  len : len;
}
end

type offsetinfo = {
  offlen : typ;
  offtyp : typ;
  offop : operand;
  offsrcoffset : int;
  offdstoffset : int;
}

type opcode =
  | Bswap of (typ * operand)
  | Retn of ((typ * operand) option) * bool (* bytes to release, far/near ret *)
  | Nop
  | Mov of typ * operand * operand * (Ast.exp option) (* dst, src, condition *)
  | Movs of typ
  | Movzx of typ * operand * typ * operand (* dsttyp, dst, srctyp, src *)
  | Movsx of typ * operand * typ * operand (* dsttyp, dst, srctyp, src *)
  | Movdq of typ * operand * typ * operand * bool (* dst type, dst op, src type, src op, aligned *)
  | Movoffset of (typ * operand) * offsetinfo list
  (* dest type, dest, (src copy length, src type, src, src src offset, src dest offset)* *)
  | Lea of typ * operand * Ast.exp
  | Call of operand * Type.addr (* addr is RA *)
  | Shift of binop_type * typ * operand * operand
  | Shiftd of binop_type * typ * operand * operand * operand
  | Rotate of binop_type * typ * operand * operand * bool (* left or right, type, src/dest op, shift op, use carry flag *)
  | Bt of typ * operand * operand
  | Bs of typ * operand * operand * direction
  | Jump of jumptarget
  | Jcc of jumptarget * Ast.exp
  | Setcc of typ * operand * Ast.exp
  | Hlt
  | Cmps of typ
  | Lods of typ
  | Scas of typ
  | Stos of typ
  | Push of typ * operand
  | Pop of typ * operand
  | Pushf of typ
  | Popf of typ
  | Popcnt of typ * operand * operand (* size, src, dest *)
  | Sahf
  | Lahf
  | Add of (typ * operand * operand)
  | Adc of (typ * operand * operand)
  | Inc of typ * operand
  | Dec of typ * operand
  | Sub of (typ * operand * operand)
  | Sbb of (typ * operand * operand)
  | Cmp of (typ * operand * operand)
  | Cmpxchg of (typ * operand * operand)
  | Cmpxchg8b of operand
  | Xadd of (typ * operand * operand)
  | Xchg of (typ * operand * operand)
  | And of (typ * operand * operand)
  | Or of (typ * operand * operand)
  | Xor of (typ * operand * operand)
  | Test of (typ * operand * operand)
  | Ptest of (typ * operand * operand)
  | Not of (typ * operand)
  | Neg of (typ * operand)
  | Mul of (typ * operand) (* typ, src *)
  | Imul of typ * (bool * operand) * operand * operand (* typ, (true if one operand form, dst operand), src1, src2 *)
  | Div of typ * operand (* typ, src *)
  | Idiv of typ * operand (* typ, src *)
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
  | Punpck of (typ * typ * order * operand * operand * operand option) (* dest size, element size, low/high elements, dest, src, optional VEX src *)
  | Ppackedbinop of (typ * typ * binopf * string * operand * operand * operand option) (* Perform a generic packed binary operation. dest size, element size, binop, assembly string, dest, src, optional VEX src *)
  | Pbinop of (typ * binopf * string * operand * operand * operand option)
  | Pmov of (typ * typ * typ * operand * operand * cast_type * string) (* Packed move. dest size, dest elt size, src elt size, dest, src, ext(signed/zero), name *)
  | Pmovmskb of (typ * operand * operand)
  | Pcmp of (typ * typ * binop_type * string * operand * operand * operand option)
  | Palignr of (typ * operand * operand * operand option * operand)
  | Pcmpstr of (typ * operand * operand * operand * Pcmpstr.imm8cb * Pcmpstr.pcmpinfo)
  | Pshufb of typ * operand * operand * operand option
  | Pshufd of typ * operand * operand * operand option * operand
  | Leave of typ
  | Interrupt of operand
  | Interrupt3 (* Trap to debugger *)
  | Sysenter
  | Syscall

(* prefix names *)
let pref_lock = 0xf0
and repnz = 0xf2
and repz = 0xf3
and hint_bnt = 0x2e
and hint_bt = 0x3e
and pref_cs = 0x2e
and pref_ss = 0x36
and pref_ds = 0x3e
and pref_es = 0x26
and pref_fs = 0x64
and pref_gs = 0x65
and pref_opsize = 0x66
and pref_addrsize = 0x67

(* Prefixes that we can usually handle automatically *)
let standard_prefs = [pref_opsize; pref_addrsize; hint_bnt; hint_bt; pref_cs; pref_ss; pref_ds; pref_es; pref_fs; pref_gs]

(* See Table 2-4: REX Prefix Fields. *)
type rex = {
  rex_w : bool; (* Bit 3: 1 = 64-bit operand size *)
  rex_r : bool; (* Bit 2: Extension of ModR/M reg field *)
  rex_x : bool; (* Bit 1: Extension of SIB index field *)
  rex_b : bool; (* Bit 0: Extension of ModR/M r/m field, SIB base
                   field, or opcode reg field *)
}

type vex = {
  vex_nr : bool; (* inverted rex_r bit *)
  vex_nx : bool; (* inverted rex_x bit *)
  vex_nb : bool; (* inverted rex_b bit *)
  vex_map_select : int; (* Specifies the opcode map to use *)
  vex_we : bool; (* For int instructions, equivalent to rex.w. For non-int instructions, opcode extension bit. *)
  vex_v : int; (* additional instruction operand (XMM or YMM register) *)
  vex_l : bool; (* 0 = 128-bit operands (xmm), 1 = 256-bit vector operands (ymm) *)
  vex_pp : int; (* Specifies mandatory prefix (0=none, 1=pref_opsize 2=repz 3=repnz) *)
}

type prefix = {
  addrsize : typ;
  opsize   : typ; (* General operand size *)
  bopsize  : typ; (* Operand size that defaults to machine size
                     (e.g. for pop) *)
  mopsize  : typ; (* Multi-scalar operand size *)
  repeat   : bool;
  nrepeat  : bool;
  addrsize_override : bool;
  opsize_override : bool;
  rex : rex option;
  vex : vex option;
  r_extend : int; (* extended r bit *)
  rm_extend : int; (* extended rm bit or sib base *)
  sib_extend : int; (* extended sib index bit *)
(* add more as needed *)
}

(** disfailwith is a non-fatal disassembly exception. *)
let disfailwith s = raise (Disasm_i386_exception s)

let unimplemented s  = disfailwith ("disasm_i386: unimplemented feature: "^s)

let (&) = (land)
and (>>) = (lsr)
and (<<) = (lsl) (* tuareg >> *)
let ite t b e1 e2 =
  exp_ite ~t b e1 e2

(* register widths *)
let r1 = reg_1
let r4 = Reg 4
let r8 = reg_8
let r16 = reg_16
let r32 = reg_32
let r64 = reg_64
let r128 = reg_128
let r256 = reg_256
let xmm_t = r128
let ymm_t = r256
let st_t = Reg 80

(** Only use this for registers, not temporaries *)
let nv = Var.newvar
let nt = Var_temp.nt

type multimodereg = { v32: Var.t; v64: Var.t }
(* new multi-mode variable *)
let nmv n32 t32 n64 t64 = { v32=nv n32 t32; v64=nv n64 t64; }

let gv mode { v32; v64 } = match mode with
  | X86 -> v32
  | X8664 -> v64

let ge mode mv = Var (gv mode mv)

(* registers *)

let rbp = nmv "R_EBP_32" r32 "R_RBP" r64
and rsp = nmv "R_ESP_32" r32 "R_RSP" r64
and rsi = nmv "R_ESI_32" r32 "R_RSI" r64
and rdi = nmv "R_EDI_32" r32 "R_RDI" r64
and rip = nmv "R_EIP" r32 "R_RIP" r64 (* XXX why is eip in here? *)
and rax = nmv "R_EAX_32" r32 "R_RAX" r64
and rbx = nmv "R_EBX_32" r32 "R_RBX" r64
and rcx = nmv "R_ECX_32" r32 "R_RCX" r64
and rdx = nmv "R_EDX_32" r32 "R_RDX" r64
and rflags = nmv "R_EFLAGS" r32 "R_RFLAGS" r64 (* XXX why is eflags in here? *)
  (* condition flag bits *)
and cf = nv "R_CF" r1
and pf = nv "R_PF" r1
and af = nv "R_AF" r1
and zf = nv "R_ZF" r1
and sf = nv "R_SF" r1
and oF = nv "R_OF" r1
and df = nv "R_DF" r1

(* segment registers and bases *)
and fs_base = nmv "R_FS_BASE_32" r32 "R_FS_BASE_64" r64
and gs_base = nmv "R_GS_BASE_32" r32 "R_GS_BASE_64" r64

and cs = nv "R_CS" r16
and ds = nv "R_DS" r16
and es = nv "R_ES" r16
and fs = nv "R_FS" r16
and gs = nv "R_GS" r16
and ss = nv "R_SS" r16

and gdt = nmv "R_GDTR" r32 "R_GDTR" r64
and ldt = nmv "R_LDTR" r32 "R_LDTR" r64

and fpu_ctrl = nv "R_FPU_CONTROL" r16
and mxcsr = nv "R_MXCSR" r32

(* r8 -> r15 *)
let nums = Array.init 8 (fun i -> nmv "ERROR" (Reg 0) (Printf.sprintf "R_R%d" (i+8)) r64)

(*
let xmms = Array.init 8 (fun i -> nv (Printf.sprintf "R_XMM%d" i) xmm_t)
*)

let ymms = Array.init 16 (fun i -> nv (Printf.sprintf "R_YMM%d" i) ymm_t)

(* floating point registers *)
let st = Array.init 8 (fun i -> nv (Printf.sprintf "R_ST%d" i) st_t)

let mvs {v64; v32} = [v64; v32]

let shared_regs =
  cf::pf::af::zf::sf::oF::df::cs::ds::es::fs::gs::ss::fpu_ctrl::mxcsr::[]
  @ Array.to_list st

let shared_multi_regs =
  rbp::rsp::rsi::rdi::rip::rax::rbx::rcx::rdx::rflags::fs_base::gs_base::[]

let regs_x86 : var list =
  shared_regs
  @ List.map (fun {v64; v32} -> v32) shared_multi_regs
  @ Array.to_list (Array.sub ymms 0 8)

let (r_8, r_9, r_10, r_11, r_12, r_13, r_14, r_15) = match Array.to_list nums with
  | (r_8::r_9::r_10::r_11::r_12::r_13::r_14::r_15::[]) -> (r_8, r_9, r_10, r_11, r_12, r_13, r_14, r_15)
  | _ -> failwith "Impossible, matching against a list of known size"

let regs_x86_64 : var list =
  shared_regs
  @ List.map (fun {v64; v32} -> v64) (shared_multi_regs @ (Array.to_list nums))
  @ Array.to_list ymms

let regs_full : var list =
  shared_regs
  @ List.flatten (List.map (fun {v64; v32} -> [v32; v64]) shared_multi_regs)
  @ List.map (fun {v64; v32} -> v64) (Array.to_list nums)
  @ Array.to_list ymms

let regs_of_mode = function
  | X86 -> regs_x86
  | X8664 -> regs_x86_64

let o_rax = Oreg 0
and o_rcx = Oreg 1
and o_rdx = Oreg 2
and o_rbx = Oreg 3
and o_rsp = Oreg 4
and o_rbp = Oreg 5
and o_rsi = Oreg 6
and o_rdi = Oreg 7

let o_es = Oseg 0
and o_cs = Oseg 1
and o_ss = Oseg 2
and o_ds = Oseg 3
and o_fs = Oseg 4
and o_gs = Oseg 5

(* let esp_e = Var esp *)
(* and ebp_e = Var ebp *)
(* and esi_e = Var esi *)
(* and edi_e = Var edi *)
(* and ecx_e = Var ecx *)
(* and eax_e = Var eax *)
(* and edx_e = Var edx *)

let mem = nmv "mem32" (TMem (r32, r8)) "mem64" (TMem (r64, r8))

(* 32-bit registers *)
module R32 = struct
  let eax = rax.v32
  let ecx = rcx.v32
  let edx = rdx.v32
  let ebx = rbx.v32
  let esp = rsp.v32
  let ebp = rbp.v32
  let esi = rsi.v32
  let edi = rdi.v32
  let eip = rip.v32
  let mem = mem.v32
  let xmms = Array.sub ymms 0 8
  let eflags = rflags.v32
  let fs_base = fs_base.v32
  let gs_base = gs_base.v32
end

(* 64-bit registers *)
module R64 = struct
  let rax = rax.v64
  let rcx = rcx.v64
  let rdx = rdx.v64
  let rbx = rbx.v64
  let rsp = rsp.v64
  let rbp = rbp.v64
  let rsi = rsi.v64
  let rdi = rdi.v64
  let rip = rip.v64
  let mem = mem.v64
  let r8 = r_8.v64
  let r9 = r_9.v64
  let r10 = r_10.v64
  let r11 = r_11.v64
  let r12 = r_12.v64
  let r13 = r_13.v64
  let r14 = r_14.v64
  let r15 = r_15.v64
  let ymms = ymms
  let rflags = rflags.v64
  let fs_base = fs_base.v64
  let gs_base = gs_base.v64
end


let cf_e = Var cf
and pf_e = Var pf
and af_e = Var af
and zf_e = Var zf
and sf_e = Var sf
and of_e = Var oF

and df_e = Var df

let seg_cs = None
and seg_ss = None
and seg_ds = None
and seg_es = None
and seg_fs = Some fs_base
and seg_gs = Some gs_base

(* eflags *)
let df_to_offset mode e =
  let t = type_of_mode mode in
  ite t (e ==* exp_false) (it 1 t) (it (-1) t)

let bap_to_rflags =
  let undefined d = Unknown(Printf.sprintf "Undefined RFLAGS bit %d" d, r1) in
  let unmodeled s = Unknown("Unmodeled RFLAGS bit " ^ s, r1) in
  BatList.of_enum (map undefined (63---32))
  @  undefined 31               (* 31 *)
  :: undefined 30               (* 30 *)
  :: undefined 29               (* 29 *)
  :: undefined 28               (* 28 *)
  :: undefined 27               (* 27 *)
  :: undefined 26               (* 26 *)
  :: undefined 25               (* 25 *)
  :: undefined 24               (* 24 *)
  :: undefined 23               (* 23 *)
  :: undefined 22               (* 22 *)
  :: unmodeled "ID"             (* 21 *)
  :: unmodeled "VIP"            (* 20 *)
  :: unmodeled "VIF"            (* 19 *)
  :: unmodeled "AC"             (* 18 *)
  :: unmodeled "VM"             (* 17 *)
  :: unmodeled "RF"             (* 16 *)
  :: undefined 15               (* 15 *)
  :: unmodeled "NT"             (* 14 *)
  :: unmodeled "IOPL1"          (* 13 *)
  :: unmodeled "IOPL2"          (* 12 *)
  :: of_e                       (* 11 *)
  :: df_e                       (* 10 *)
  :: unmodeled "IF"             (*  9 *)
  :: unmodeled "TF"             (*  8 *)
  :: sf_e                       (*  7 *)
  :: zf_e                       (*  6 *)
  :: undefined 5                (*  5 *)
  :: af_e                       (*  4 *)
  :: undefined 3                (*  3 *)
  :: pf_e                       (*  2 *)
  :: undefined 1                (*  1 *)
  :: cf_e                       (*  0 *)
  :: []
let bap_to_eflags = BatList.drop 32 bap_to_rflags
let bap_to_flags = BatList.drop 16 bap_to_eflags
let bap_to_lflags = BatList.drop 8 bap_to_flags

let rflags_e = BatList.reduce (++*) bap_to_rflags
let eflags_e = BatList.reduce (++*) bap_to_eflags
and flags_e = BatList.reduce (++*) bap_to_flags
and lflags_e = BatList.reduce (++*) bap_to_lflags

let rflags_to_bap =
  let assn v = Some(v, Util.id) in
  BatList.of_enum (map (fun _ -> None) (63---32))
  @  None                       (* 31 *)
  :: None                       (* 30 *)
  :: None                       (* 29 *)
  :: None                       (* 28 *)
  :: None                       (* 27 *)
  :: None                       (* 26 *)
  :: None                       (* 25 *)
  :: None                       (* 24 *)
  :: None                       (* 23 *)
  :: None                       (* 22 *)
  :: None                       (* 21 *)
  :: None                       (* 20 *)
  :: None                       (* 19 *)
  :: None                       (* 18 *)
  :: None                       (* 17 *)
  :: None                       (* 16 *)
  :: None                       (* 15 *)
  :: None                       (* 14 *)
  :: None                       (* 13 *)
  :: None                       (* 12 *)
  :: assn oF                    (* 11 *)
  :: assn df                    (* 10 *)
  :: None                       (* 09 *)
  :: None                       (* 08 *)
  :: assn sf                    (* 07 *)
  :: assn zf                    (* 06 *)
  :: None                       (* 05 *)
  :: assn af                    (* 04 *)
  :: None                       (* 03 *)
  :: assn pf                    (* 02 *)
  :: None                       (* 01 *)
  :: assn cf                    (* 00 *)
  :: []
let eflags_to_bap = BatList.drop 32 rflags_to_bap
let flags_to_bap = BatList.drop 16 eflags_to_bap
let lflags_to_bap = BatList.drop 8 flags_to_bap
(* A list of functions for assigning each bit in rflags *)
let assns_rflags_to_bap =
  List.map
    (function
      | None -> (fun e -> [])
      | Some (v,f) -> (fun e -> [Move(v,f e,[])]))
    rflags_to_bap
let assns_eflags_to_bap = BatList.drop 32 assns_rflags_to_bap
let assns_flags_to_bap = BatList.drop 16 assns_eflags_to_bap
let assns_lflags_to_bap = BatList.drop 8 assns_flags_to_bap

(* exp helpers *)

let load_s mode s t a =
  let mem_e = ge mode mem in
  match s with
  | None -> Load(mem_e, a, little_endian, t)
  | Some v -> Load(mem_e, Var v +* a, little_endian, t)

(* exp from int64 *)
let lt i t = Int(Arithmetic.to_big_int (big_int_of_int64 i,t), t)
let l64 i = lt i r64
let l32 i = lt i r32
let l16 i = lt i r16

let int64_of_mode = function
  | X86 -> l32
  | X8664 -> l64

(* exp from int *)
let it i t = Int(Arithmetic.to_big_int (biconst i, t), t)
let i64 i = it i r64
let i32 i = it i r32

let int_of_mode = function
  | X86 -> i32
  | X8664 -> i64

(* exp from big int *)
let bt i t = Int(Arithmetic.to_big_int (i, t), t)
let b64 i = bt i r64
let b32 i = bt i r32
let b16 i = bt i r16

let big_int_of_mode = function
  | X86 -> b32
  | X8664 -> b64

(* Get elemt from low opcode bits *)
let lowbits2elemt b =
  match b & 3 with
  | 0 -> reg_8
  | 1 -> reg_16
  | 2 -> reg_32
  | 3 -> reg_64
  | _ -> disfailwith "invalid"

(* converts a register number to the corresponding register variable *)
let bits2genreg = function
  | 0 -> rax
  | 1 -> rcx
  | 2 -> rdx
  | 3 -> rbx
  | 4 -> rsp
  | 5 -> rbp
  | 6 -> rsi
  | 7 -> rdi
  | i when i >= 8 && i <= 15 -> nums.(i-8)
  | _ -> failwith "bits2genreg takes 4 bits"

and reg2bits r =
  let (i,_) = BatList.findi (fun _ x -> x == r) [rax; rcx; rdx; rbx; rsp; rbp; rsi; rdi] in
  i

let bits2segreg = function
  | 0 -> es
  | 1 -> cs
  | 2 -> ss
  | 3 -> ds
  | 4 -> fs
  | 5 -> gs
  | 6 | 7 -> disfailwith "bits2segreg: reserved"
  | _ -> failwith "bits2regseg: invalid"

let bits2segrege b = Var(bits2segreg b)

let bits2ymm b = ymms.(b)

let bits2ymme b = Var(bits2ymm b)

let bits2ymm128e b =
  cast_low r128 (bits2ymme b)

let bits2ymm64e b =
  cast_low r64 (bits2ymme b)

let bits2ymm32e b =
  cast_low r32 (bits2ymme b)

let bits2xmm = bits2ymm128e

let bits2xmm64e = bits2ymm64e

let bits2xmm32e = bits2ymm32e

let ymm0 = ymms.(0)

let bits2reg64e mode b =
  ge mode (bits2genreg b)

let bits2reg32e mode b =
  cast_low r32 (ge mode (bits2genreg b))

let bits2reg16e mode b =
  cast_low r16 (bits2reg32e mode b)

let bits2reg8e mode ?(has_rex=false) b =
  if b < 4 || has_rex then
    cast_low r8 (bits2reg32e mode b)
  else
    cast_high r8 (cast_low r16 (bits2reg32e mode (b land 3)))

let reg2xmm r =
  bits2xmm (reg2bits r)

(* effective addresses for 16-bit addressing *)
let eaddr16 mode =
  let e v = cast_low r16 (ge mode v) in
function
  (* R/M byte *)
  | 0 -> (e rbx) +* (e rsi)
  | 1 -> (e rbx) +* (e rdi)
  | 2 -> (e rbp) +* (e rsi)
  | 3 -> (e rbp) +* (e rdi)
  | 4 -> e rsi
  | 5 -> e rdi
  | 6 -> e rbp
  | 7 -> e rbx
  | _ -> disfailwith "eaddr16 takes only 0-7"

let eaddr16e mode b = eaddr16 mode b

let ah_e mode = bits2reg8e mode 4
let ch_e mode = bits2reg8e mode 5
let dh_e mode = bits2reg8e mode 6
let bh_e mode = bits2reg8e mode 7

module ToIR = struct

(* stmt helpers *)

let move v e =
  Move(v, e, [])

let store_s mode s t a e =
  let mem = gv mode mem in
  match s with
  | None -> move mem (Store(Var mem, a, e, little_endian, t))
  | Some v -> move mem (Store(Var mem, Var v +* a, e, little_endian, t))

let storem mode t a e =
  move mode (Store(Var mode, a, e, little_endian, t))

let op2e_s mode ss has_rex t = function
  | Ovec r when t = r256 -> bits2ymme r
  | Ovec r when t = r128 -> bits2ymm128e r
  | Ovec r when t = r64 -> bits2ymm64e r
  | Ovec r when t = r32 -> bits2ymm32e r
  | Ovec r -> let i = match t with | Reg n -> ": "^(string_of_int n) | _ -> ""
              in disfailwith ("invalid SIMD register size for op2e"^i)
  | Oreg r when t = r64 -> bits2reg64e mode r
  | Oreg r when t = r32 -> bits2reg32e mode r
  | Oreg r when t = r16 -> bits2reg16e mode r
  | Oreg r when t = r8 -> bits2reg8e mode ~has_rex r
  | Oreg r -> unimplemented "unknown register"
  | Oseg r when t = r64 -> cast_unsigned t (bits2segrege r)
  (* There is no 32-bit extension of segment selectors; this is not a bug *)
  | Oseg r when t = r16 -> bits2segrege r
  | Oseg r -> disfailwith "Segment register when t is not r16"
  | Oaddr e -> load_s mode ss t e
  | Oimm i -> Int(Arithmetic.to_big_int (i,t), t)

let assn_s mode s has_rex has_vex t v e =
  (* Assign to some bits of v, starting at bit off, while preserving the other bits *)
  let sub_assn ?(off=0) t v e =
    let concat_exps = ref [] in
    let bits = Typecheck.bits_of_width (Var.typ v) in
    let assnbits = Typecheck.bits_of_width t in

    (* Add the upper preserved bits, if any *)
    let ubh = (bits-1) and ubl = (assnbits+off) in
    if ubh > ubl then concat_exps := extract ubh ubl (Var v)::!concat_exps;

    (* Add e *)
    concat_exps := e::!concat_exps;

    (* Add the lower preserved bits, if any *)
    let lbh = (off-1) and lbl = 0 in
    if lbh > lbl then
      concat_exps := extract lbh lbl (Var v)::!concat_exps;

    let final_e = BatList.reduce (fun big_e e -> Ast_convenience.concat e big_e) !concat_exps in
    move v final_e
  in
  match v, t with
  (* Zero-extend 128-bit assignments to 256-bit ymms. *)
  | Ovec r, Reg (128|64|32) when has_vex ->
    let v = bits2ymm r in
    sub_assn r256 v (cast_unsigned r256 e)
  | Ovec r, Reg (256|128|64|32) ->
    let v = bits2ymm r in
    sub_assn t v e
  | Ovec r, _ -> disfailwith "invalid SIMD register size for assignment"
  (* Zero-extend 32-bit assignments to 64-bit registers. *)
  | Oreg r, Reg 32 when mode = X8664 ->
    let v = gv mode (bits2genreg r) in
    sub_assn r64 v (cast_unsigned r64 e)
  | Oreg r, Reg (64|32|16) ->
    let v = gv mode (bits2genreg r) in
    sub_assn t v e
  | Oreg r, Reg 8 when r < 4 || (mode = X8664 && has_rex) ->
    let v = gv mode (bits2genreg r) in
    sub_assn t v e
  | Oreg r, Reg 8 ->
    let v = gv mode (bits2genreg (r land 3)) in
    sub_assn ~off:8 t v e
  | Oreg _, _ -> unimplemented "assignment to sub registers"
  | Oseg r, _ when t = r16 ->
    let v = bits2segreg r in
    move v e
  | Oseg r, _ -> disfailwith "Can't assign to non 16 bit segment register"
  | Oaddr a, _ -> store_s mode s t a e
  | Oimm _, _ -> disfailwith "disasm_i386: Can't assign to an immediate value"

(* Double width operands, as used by multiplication and division *)
let op_dbl = function
  | Reg 8 -> [r16, o_rax]
  | Reg 16 -> [r16, o_rdx; r16, o_rax]
  | Reg 32 -> [r32, o_rdx; r32, o_rax]
  | Reg 64 -> [r64, o_rdx; r64, o_rax]
  | _ -> disfailwith "op_dbl only defined for Reg 8, 16, 32, and 64"

(* Return an expression for a double-width operand, as used by the div
   instruction. *)
let op2e_dbl_s mode ss has_rex t =
  let cf (ct, o) = op2e_s mode ss has_rex ct o in
  let ol = List.map cf (op_dbl t) in
  BatList.reduce
    (fun bige little -> bige ++* little)
    ol

(* Double width assignments, as used by multiplication *)
let assn_dbl_s mode s has_rex has_vex t e = match op_dbl t with
  | (t,o) :: [] -> [assn_s mode s has_rex has_vex t o e], op2e_s mode s has_rex t o
  | l ->
    let tmp = nt "t" (Reg (Typecheck.bits_of_width t * 2)) in
    let f (stmts, off) (ct, o) =
      let newoff = off + Typecheck.bits_of_width ct in
      assn_s mode s has_rex has_vex ct o (extract (newoff-1) off (Var tmp))::stmts, newoff
    in
    List.rev (fst (List.fold_left f ([move tmp e], 0) (List.rev l))), Var tmp

(* A function for computing the target of jumps. *)
let compute_jump_target mode s has_rex =
  let t = type_of_mode mode in
  function
  | Jabs o -> op2e_s mode s has_rex t o
  | Jrel (na,offset) ->
    let i,t = Arithmetic.binop PLUS (na,t) (offset,t) in
    Int(i,t)
let jump_target = compute_jump_target

let bytes_of_width = Typecheck.bytes_of_width
let bits_of_width = Typecheck.bits_of_width

let string_incr mode t v =
  let i = int_of_mode mode in
  if t = r8 then
    move v (Var v +* df_to_offset mode df_e)
  else
    move v (Var v +* (df_to_offset mode df_e ** i (bytes_of_width t)))

let rep_wrap ?check_zf ~mode ~addr ~next stmts =
  let bi = big_int_of_mode mode in
  let endstmt = match check_zf with
    | None -> Jmp(bi addr, [])
    | Some x when x = repz ->
      CJmp(zf_e, bi addr, bi next, [])
    | Some x when x = repnz ->
      CJmp(zf_e, bi next, bi addr, [])
    | _ -> failwith "invalid value for ?check_zf"
  in
  let rcx = gv mode rcx in
  let rcx_e = Var rcx in
  cjmp (rcx_e ==* bi (bi0)) (bi next)
  @ stmts
  @ move rcx (rcx_e -* bi (bi1))
  :: cjmp (rcx_e ==* bi (bi0)) (bi next)
  @ [endstmt]

let reta = [StrAttr "ret"]
and calla = [StrAttr "call"]

let compute_sf result = cast_high r1 result
let compute_zf t result = Int(bi0, t) ==* result
let compute_pf t r =
  let acc = nt "acc" t in
  let var_acc = Var acc in
  (* extra parens do not change semantics but do make it pretty print nicer *)
  exp_not (cast_low r1
             (Let(acc, (r >>* it 4 t) ^* r, Let(acc, (var_acc >>* it 2 t) ^* var_acc, (var_acc >>* it 1 t) ^* var_acc))))

let set_sf r = move sf (compute_sf r)
let set_zf t r = move zf (compute_zf t r)
let set_pf t r = move pf (compute_pf t r)

let set_pszf t r =
  [set_pf t r;
   set_sf r;
   set_zf t r]

(* Adjust flag

   AF is set when there is a carry to or borrow from bit 4 (starting
   at 0), when considering unsigned operands. Let X_i denote bit i of
   value X.  Note that in addition, r_4 = c + [(op1_4 + op2_4) mod 2],
   where c is the carry bit from the lower four bits. Since AF = c,
   and we want to know the value of AF, we can rewrite as AF = c = r_4
   - [(op1_4 + op2_4) mod 2]. Noting that addition and subtraction mod
   2 is just xor, we can simplify to AF = r_4 xor op1_4 xor op2_4.
*)

let set_apszf t s1 s2 r =
  let bit4 = it (1 lsl 4) t in
  move af (bit4 ==* (bit4 &* ((r ^* s1) ^* s2)))
  ::set_pszf t r

(* Helper functions to set flags for adding *)
let set_aopszf_add t s1 s2 r =
  move oF (cast_high r1 ((s1 =* s2) &* (s1 ^* r)))
  ::set_apszf t s1 s2 r

let set_flags_add t s1 s2 r =
  move cf (r <* s1)
  ::set_aopszf_add t s1 s2 r

(* Helper functions to set flags for subtracting *)
let set_apszf_sub t s1 s2 r = set_apszf t s1 s2 r

let set_aopszf_sub t s1 s2 r =
  move oF (cast_high r1 ((s1 ^* s2) &* (s1 ^* r)))
  ::set_apszf_sub t s1 s2 r

let set_flags_sub t s1 s2 r =
  move cf (s2 >* s1)
  ::set_aopszf_sub t s1 s2 r


let rec to_ir mode addr next ss pref has_rex has_vex =
  let load = load_s mode ss (* Need to change this if we want seg_ds <> None *)
  and op2e = op2e_s mode ss has_rex
  and op2e_dbl = op2e_dbl_s mode ss has_rex
  and store = store_s mode ss
  and assn = assn_s mode ss has_rex has_vex
  and assn_dbl = assn_dbl_s mode ss has_rex has_vex
  and mi = int_of_mode mode
  (* and mi64 = int64_of_mode mode *) (* unused *)
  and mt = type_of_mode mode
  and fs_base = gv mode fs_base
  and _fs_base_e = ge mode fs_base
  and gs_base = gv mode gs_base
  and _gs_base_e = ge mode gs_base
  and _rbp = gv mode rbp
  and rbp_e = ge mode rbp
  and rsp = gv mode rsp
  and rsp_e = ge mode rsp
  and rsi = gv mode rsi
  and rsi_e = ge mode rsi
  and rdi = gv mode rdi
  and rdi_e = ge mode rdi
  and rax = gv mode rax
  and rax_e = ge mode rax
  and _rbx = gv mode rbx
  and _rbx_e = ge mode rbx
  and rcx = gv mode rcx
  and _rcx_e = ge mode rcx
  and _rdx = gv mode rdx
  and rdx_e = ge mode rdx
  and ah_e = ah_e mode
  and _ch_e = ch_e mode
  and _dh_e = dh_e mode
  and _bh_e = bh_e mode
  in
  function
  | Nop -> []
  | Bswap(t, op) ->
    let e = match t with
      | Reg 32 | Reg 64 ->
        reverse_bytes (op2e t op)
      | _ -> disfailwith "bswap: Expected 32 or 64 bit type"
    in
    [assn t op e]
  | Retn (op, far_ret) when pref = [] || pref = [repz]  || pref = [repnz]->
    let temp = nt "ra" mt in
    let load_stmt = if far_ret
      then (* TODO Mess with segment selectors here *)
        unimplemented "long retn not supported"
      else move temp (load_s mode seg_ss mt rsp_e)
    in
    let rsp_stmts =
      move rsp (rsp_e +* (mi (bytes_of_width mt)))::
        (match op with
        | None -> []
        | Some(t, src) ->
          [move rsp (rsp_e +* (op2e t src))]
      ) in
      load_stmt::
      rsp_stmts@
      [Jmp(Var temp, reta)]
  | Mov(t, dst, src, condition) ->
    let c_src = (match condition with
      | None -> op2e t src
      | Some(c) -> ite t c (op2e t src) (op2e t dst))
    in
    (* Find base by looking at LDT or GDT *)
    let base_e e =
      (* 0 = GDT, 1 = LDT *)
      let ti = extract 3 3 e in
      let base = ite mt ti (ge mode ldt) (ge mode gdt) in
      (* Extract index into table *)
      let entry_size, entry_shift = match mode with
        | X86 -> r64, 6 (* 1<<6 = 64 *)
        | X8664 -> r128, 7 (* 1<<7 = 128 *)
      in
      let index = cast_unsigned mt (extract 15 4 e) <<* mi entry_shift in
      (* Load the table entry *)
      let table_entry = load_s mode None entry_size (base +* index) in
      (* Extract the base *)
      concat_explist
        (BatList.enum
           ((match mode with
           | X86 -> []
           | X8664 -> extract 95 64 table_entry :: [])
            @  extract 63 56 table_entry
            :: extract 39 32 table_entry
            :: extract 31 16 table_entry
            :: []))
    in
    let bs =
      let dst_e = op2e t dst in
      if dst = o_fs && !compute_segment_bases then [move fs_base (base_e dst_e)]
      else if dst = o_gs && !compute_segment_bases then [move gs_base (base_e dst_e)]
      else []
    in
    assn t dst c_src
    :: bs
  | Movs(Reg bits as t) ->
      let stmts =
        store_s mode seg_es t rdi_e (load_s mode seg_es t rsi_e)
        :: string_incr mode t rsi
        :: string_incr mode t rdi
        :: []
      in
      if pref = [] then
        stmts
      else if List.mem repz pref || List.mem repnz pref then
        (* movs has only rep instruction others just considered to be rep *)
        rep_wrap ~mode ~addr ~next stmts
      else
        unimplemented "unsupported prefix for movs"
  | Movzx(t, dst, ts, src) ->
    [assn t dst (cast_unsigned t (op2e ts src))]
  | Movsx(t, dst, ts, src) ->
    [assn t dst (cast_signed t (op2e ts src))]
  | Movdq(ts, s, td, d, align) ->
    let (s, al) = match s with
      | Ovec _ | Oreg _-> op2e ts s, []
      | Oaddr a -> op2e ts s, [a]
      | Oimm _ | Oseg _ -> disfailwith "invalid source operand for movdq"
    in
    let (d, al) = match d with
      (* Behavior is to clear the xmm bits *)
      | Ovec _ -> assn td d (cast_unsigned td s), al
      | Oreg _ -> assn td d s, al
      | Oaddr a -> assn td d s, a::al
      | Oimm _ | Oseg _ -> disfailwith "invalid dest operand for movdq"
    in
    let im = int_of_mode mode in
    let al =
      if align then
        List.map (fun a -> Assert( (a &* im 15) ==* im 0, [])) (al)
      else []
    in
    d::al
  | Movoffset((tdst, dst), offsets) ->
    (* If a vex prefix is present, then exra space is filled with 0.
       Otherwise, the bits are preserved. *)
    let padding hi lo =
      if hi < lo then []
      else if has_vex then [it 0 (Reg (hi - lo + 1))]
      else [extract hi lo (op2e tdst dst)]
    in
    let offsets = List.sort (fun {offdstoffset=o1} {offdstoffset=o2} -> Pervasives.compare o1 o2) offsets in
    let add_exp (elist,nextbit) {offlen; offtyp; offop; offsrcoffset; offdstoffset} =
      extract ((bits_of_width offlen) + offsrcoffset - 1) offsrcoffset (op2e offtyp offop)
      :: padding (offdstoffset - 1) nextbit
      @ elist, offdstoffset + (bits_of_width offlen)
    in
    let elist, nextbit = List.fold_left add_exp ([],0) offsets in
    let elist = padding ((bits_of_width tdst) - 1) nextbit @ elist in
    [assn tdst dst (Ast_convenience.concat_explist (BatList.enum elist))]
  | Punpck(t, et, o, d, s, vs) ->
    let nelem = match t, et with
      | Reg n, Reg n' -> n / n'
      | _ -> disfailwith "invalid"
    in
    assert (nelem mod 2 = 0);
    let nelem_per_src = nelem / 2 in
    let halft = Reg ((Typecheck.bits_of_width t)/2) in
    let castf = match o with
      | High -> cast_high halft
      | Low -> cast_low halft
    in
    let se, de = castf (op2e t s), castf (op2e t d) in
    let st, dt = nt "s" halft, nt "d" halft in
    let mape i =
      BatList.enum [extract_element et (Var st) i; extract_element et (Var dt) i]
    in
    let e = concat_explist (BatEnum.flatten (map mape ((nelem_per_src-1)---0))) in
    let dest = match vs with
      | None -> d
      | Some vdst -> vdst
    in
    [move st se;
     move dt de;
     assn t dest e]
  | Ppackedbinop(t, et, fbop, _, d, s, vs) ->
    let nelem = match t, et with
      | Reg n, Reg n' -> n / n'
      | _ -> disfailwith "invalid"
    in
    let getelement o i =
      (* assumption: immediate operands are repeated for all vector
         elements *)
      match o with
      | Oimm i -> op2e et o
      | _ -> extract_element et (op2e t o) i
    in
    let f i =
      fbop (getelement d i) (getelement s i)
    in
    let e = concat_explist (map f ((nelem-1)---0)) in
    (match vs with
    | None -> [assn t d e]
    | Some vdst -> [assn t vdst e])
  | Pbinop(t, fbop, s, o1, o2, vop) ->
    (match vop with
    | None -> [assn t o1 (fbop (op2e t o1) (op2e t o2))]
    | Some vop -> [assn t o1 (fbop (op2e t vop) (op2e t o2))])
  | Pcmp (t,elet,bop,_,dst,src,vsrc) ->
    let ncmps = (bits_of_width t) / (bits_of_width elet) in
    let elebits = bits_of_width elet in
    let src = match src with
      | Ovec i -> op2e t src
      | Oaddr a -> load t a
      | Oreg _ | Oimm _ | Oseg _ -> disfailwith "invalid"
    in
    let dst, vsrc = match vsrc with
      | None -> dst, dst
      | Some vsrc -> dst, vsrc
    in
    let compare_region i =
      let byte1 = Extract(biconst (i*elebits-1), biconst ((i-1)*elebits), src) in
      let byte2 = Extract(biconst (i*elebits-1), biconst ((i-1)*elebits), op2e t vsrc) in
      let tmp = nt ("t" ^ string_of_int i) elet in
      Var tmp, move tmp (Ite(binop bop byte1 byte2, lt (-1L) elet, lt 0L elet))
    in
    let indices = BatList.init ncmps (fun i -> i + 1) in (* list 1-nbytes *)
      let comparisons = List.map compare_region indices in
      let temps, cmps = List.split comparisons in
      let temps = List.rev temps in
        (* could also be done with shifts *)
      let store_back = List.fold_left (fun acc i -> Concat(acc,i)) (List.hd temps) (List.tl temps) in
      cmps @ [assn t dst store_back]
  | Pmov (t, dstet, srcet, dst, src, ext, _) ->
    let nelem = match t, dstet with
      | Reg n, Reg n' -> n / n'
      | _ -> disfailwith "invalid"
    in
    let getelt op i = extract_element srcet (op2e t op) i in
    let extcast = match ext with
      | CAST_UNSIGNED | CAST_SIGNED -> cast ext dstet
      | _ -> disfailwith "invalid"
    in
    let extend i = extcast (getelt src i) in
    let e = concat_explist (map extend ((nelem - 1)---0)) in
    [assn t dst e]
  | Pmovmskb (t,dst,src) ->
      let nbytes = bytes_of_width t in
      let src = match src with
        | Ovec i -> op2e t src
        | _ -> disfailwith "invalid operand"
      in
      let get_bit i = Extract(biconst (i*8-1), biconst (i*8-1), src) in
      let byte_indices = BatList.init nbytes (fun i -> i + 1) in (* list 1-nbytes *)
      let all_bits = List.map get_bit byte_indices in
      let all_bits = List.rev all_bits in
        (* could also be done with shifts *)
      let padt = Reg(32 - nbytes) in
      let or_together_bits = List.fold_left (fun acc i -> Concat(acc,i)) (it 0 padt) all_bits in
      [assn r32 dst or_together_bits]
  | Palignr (t,dst,src,vsrc,imm) ->
      let dst_e = op2e t dst in
      let src_e = op2e t src in
      let imm = op2e t imm in
      let concat = dst_e ++* src_e in
      let t_concat = Typecheck.infer_ast concat in
      let shift = concat >>* (cast_unsigned t_concat (imm <<* (it 3 t))) in
      let high, low = match t with
        | Reg 256 -> biconst 255, bi0
        | Reg 128 -> biconst 127, bi0
        | Reg 64 -> biconst 63, bi0
        | _ -> disfailwith "impossible: used non 64/128/256-bit operand in palignr"
      in
      let result = Extract (high, low, shift) in
      let im = int_of_mode mode in
      let addresses = List.fold_left (fun acc -> function Oaddr a -> a::acc | _ -> acc) [] [src;dst] in
      List.map (fun addr -> Assert( (addr &* im 15) ==* im 0, [])) addresses
      @ (match vsrc with
        | None -> [assn t dst result]
        | Some vdst -> [assn t vdst result])
  | Pcmpstr(t,xmm1,xmm2m128,imm,imm8cb,pcmpinfo) ->
    (* All bytes and bits are numbered with zero being the least
       significant. This includes strings! *)
    (* NOTE: Strings are backwards, at least when they are in
       registers.  This doesn't seem to be documented in the Intel
       manual.  This means that the NULL byte comes before the
       string. *)
    let xmm1_e = op2e t xmm1 in
    let xmm2m128_e = op2e t xmm2m128 in
    let regm = type_of_mode mode in

    let open Pcmpstr in
    let comment = match imm8cb with
      | {agg=agg;
         ssize=ssize;
         ssign=ssign;
         outselectsig=outselectsig;
         outselectmask=outselectmask} ->
        Comment(Printf.sprintf "Imm8 control byte information.  Aggregation function: %s Element size: %s Element signedness: %s Significance: %s Mask type: %s"
                            (agg_to_string agg)
                            (ssize_to_string ssize)
                            (ssign_to_string ssign)
                            (outselectsig_to_string outselectsig)
                            (outselectmask_to_string outselectmask), [])
    in

    let nelem, nbits, elemt = match imm8cb with
      | {ssize=Bytes} -> 16, 8, Reg 8
      | {ssize=Words} -> 8, 16, Reg 16
    in
    (* Get element index in e *)
    let get_elem = extract_element elemt in
    (* Get from xmm1/xmm2 *)
    let get_xmm1 = get_elem xmm1_e
    and get_xmm2 = get_elem xmm2m128_e
    in

    (* Build expressions that assigns the correct values to the
       is_valid variables using implicit (NULL-based) string
       length. *)
    let build_implicit_valid_xmm_i is_valid_xmm_i get_xmm_i =
      let f acc i =
        (* Previous element is valid *)
        let prev_valid = if i == 0 then exp_true else Var (is_valid_xmm_i (i-1)) in
          (* Current element is valid *)
        let curr_valid = get_xmm_i i <>* it 0 elemt in
        Let(is_valid_xmm_i i, prev_valid &* curr_valid, acc)
      in (fun e -> fold f e (nelem-1---0))
    in

    (* Build expressions that assigns the correct values to the
       is_valid variables using explicit string length. *)
    let build_explicit_valid_xmm_i is_valid_xmm_i sizee =
      (* Max size is nelem *)
      let sizev = nt "sz" regm in
      let sizee = exp_ite (binop LT (it nelem regm) sizee) (it nelem regm) sizee in
      let f acc i =
        (* Current element is valid *)
        let curr_valid = binop LT (it i regm) (Var sizev) in
        Let(is_valid_xmm_i i, curr_valid, acc)
      in (fun e -> Let(sizev, sizee, fold f e (nelem-1---0)))
    in

    (* Get var name indicating whether index in xmm num is a valid
       byte (before NULL byte). *)
    let is_valid =
      let vh = Hashtbl.create (2*nelem) in
      (fun xmmnum index ->
        try Hashtbl.find vh (xmmnum,index)
        with Not_found ->
          let v = nt ("is_valid_xmm"^string_of_int xmmnum^"_ele"^string_of_int index) r1 in
          Hashtbl.add vh (xmmnum,index) v;
          v)
    in
    let is_valid_xmm1 index = is_valid 1 index
    and is_valid_xmm2 index = is_valid 2 index
    in
    let is_valid_xmm1_e index = Var(is_valid_xmm1 index)
    and is_valid_xmm2_e index = Var(is_valid_xmm2 index)
    in

    let build_valid_xmm1,build_valid_xmm2 =
      match pcmpinfo with
      | {len=Implicit} ->
        build_implicit_valid_xmm_i is_valid_xmm1 get_xmm1,
        build_implicit_valid_xmm_i is_valid_xmm2 get_xmm2
      | {len=Explicit} ->
        build_explicit_valid_xmm_i is_valid_xmm1 rax_e,
        build_explicit_valid_xmm_i is_valid_xmm2 rdx_e
    in

    let get_intres1_bit index = match imm8cb with
      | {agg=EqualAny} ->
        (* Is xmm2[index] at xmm1[j]? *)
        let check_char acc j =
          let eq = (get_xmm2 index) ==* (get_xmm1 j) in
          let valid = is_valid_xmm1_e j in
          ite r1 (eq &* valid) exp_true acc
        in
        binop AND (is_valid_xmm2_e index)
          (* Is xmm2[index] included in xmm1[j] for any j? *)
          (fold check_char exp_false (nelem-1---0))
      | {agg=Ranges} ->
        (* Is there an even j such that xmm1[j] <= xmm2[index] <=
           xmm1[j+1]? *)
        let check_char acc j =
          (* XXX: Should this be AND? *)
          let rangevalid = is_valid_xmm1_e (2*j) &* is_valid_xmm1_e (2*j+1) in
          let lte = match imm8cb with
            | {ssign=Unsigned} -> LE
            | {ssign=Signed} -> SLE
          in
          let inrange =
            binop lte (get_xmm1 (2*j)) (get_xmm2 index)
            &* binop lte (get_xmm2 index) (get_xmm1 (2*j+1))
          in
          ite r1 (unop NOT rangevalid) exp_false
            (ite r1 inrange exp_true acc)
        in
        is_valid_xmm2_e index
          (* Is xmm2[index] in the jth range pair? *)
          &* fold check_char exp_false ((nelem/2-1)---0)
      | {agg=EqualEach} ->
        (* Does xmm1[index] = xmm2[index]? *)
        let xmm1_invalid = unop NOT (is_valid_xmm1_e index) in
        let xmm2_invalid = unop NOT (is_valid_xmm2_e index) in
        let bothinvalid = xmm1_invalid &* xmm2_invalid in
        let eitherinvalid = xmm1_invalid |* xmm2_invalid in
        let eq = get_xmm1 index ==* get_xmm2 index in
        (* both invalid -> true
           one invalid -> false
           both valid -> check same byte *)
        ite r1 bothinvalid exp_true
          (ite r1 eitherinvalid exp_false
             (ite r1 eq exp_true exp_false))
      | {agg=EqualOrdered} ->
        (* Does the substring xmm1 occur at xmm2[index]? *)
        let check_char acc j =
          let neq = get_xmm1 j <>* get_xmm2 (index+j) in
          let substrended = unop NOT (is_valid_xmm1_e j) in
          let bigstrended = unop NOT (is_valid_xmm2_e (index+j)) in
          (* substrended => true
             bigstrended => false
             byte diff => false
             byte same => keep going  *)
          ite r1 substrended exp_true
            (ite r1 bigstrended exp_false
               (ite r1 neq exp_false acc))
        in
        (* Is xmm1[j] equal to xmm2[index+j]? *)
        fold check_char exp_true ((nelem-index-1)---0)
    in
    let bits = map get_intres1_bit (nelem-1---0) in
    let res_e = build_valid_xmm1 (build_valid_xmm2 (concat_explist bits)) in
    let int_res_1 = nt "IntRes1" r16 in
    let int_res_2 = nt "IntRes2" r16 in

    let contains_null e =
      fold (fun acc i ->
        ite r1 (get_elem e i ==* it 0 elemt) exp_true acc) exp_false (0--(nelem-1))
    in
    (* For pcmpistri/pcmpestri *)
    let sb e =
      fold (fun acc i ->
        ite regm (exp_true ==* extract i i e)
          (it i regm)
          acc
        ) (it nelem regm)
        (match imm8cb with
        | {outselectsig=LSB} -> (nelem-1)---0
        | {outselectsig=MSB} -> 0--(nelem-1))
    in
    (* For pcmpistrm/pcmpestrm *)
    let mask e =
      match imm8cb with
      | {outselectmask=Bitmask} ->
        cast_unsigned r128 e
      | {outselectmask=Bytemask} ->
        let get_element i =
          cast_unsigned elemt (extract i i e)
        in
        concat_explist (map get_element ((nelem-1)---0))
    in
    comment
    :: move int_res_1 (cast_unsigned r16 res_e)
    :: (match imm8cb with
    | {negintres1=false} ->
      move int_res_2 (Var int_res_1)
    | {negintres1=true; maskintres1=false} ->
      (* int_res_1 is bitwise-notted *)
      move int_res_2 (unop NOT (Var int_res_1))
    | {negintres1=true; maskintres1=true} ->
      (* only the valid elements in xmm2 are bitwise-notted *)
      (* XXX: Right now we duplicate the valid element computations
         when negating the valid elements.  They are also used by the
         aggregation functions.  A better way to implement this might
         be to write the valid element information out as a temporary
         bitvector.  The aggregation functions and this code would
         then extract the relevant bit to see if an element is
         valid. *)
      let validvector =
        let bits = map is_valid_xmm2_e (nelem-1---0) in
        build_valid_xmm2 (cast_unsigned r16 (concat_explist bits))
      in
      move int_res_2 (validvector ^* Var int_res_1))
    :: (match pcmpinfo with
    | {out=Index} -> move rcx (sb (Var int_res_2))
    (* FIXME: ymms should be used instead of xmms here *)
    | {out=Mask} -> move ymm0 (mask (Var int_res_2)))
    :: move cf (Var int_res_2 <>* it 0 r16)
    :: move zf (contains_null xmm2m128_e)
    :: move sf (contains_null xmm1_e)
    :: move oF (extract 0 0 (Var int_res_2))
    :: move af (it 0 r1)
    :: move pf (it 0 r1)
    :: []
  | Pshufd (t, dst, src, vsrc, imm) ->
    let src_e = op2e t src in
    let imm_e = op2e t imm in
    (* XXX: This would be more straight-forward if implemented using
       map, instead of fold *)
    let get_dword ndword =
      let high = 2 * (ndword mod 4) + 1 in
      let low = 2 * (ndword mod 4) in
      let index = cast_unsigned t (extract high low imm_e) in
      (* Use the same pattern for the top half of a ymm register *)
      let index = if t = Reg 256 && ndword > 3 then index +* Int(bi4,t) else index in
      extract_element_symbolic reg_32 src_e index
    in
    let topdword = match t with Reg 128 -> 3 | _ -> 7 in
    let dwords = concat_explist (map get_dword (topdword---0)) in
    (match vsrc with
    | None -> [assn t dst dwords]
    | Some vdst -> [assn t vdst dwords])
  | Pshufb (t, dst, src, vsrc) ->
    let order_e = op2e t src in
    let dst_e = op2e t dst in
    let get_bit i =
      let highbit = extract ((i*8)+7) ((i*8)+7) order_e in
      let index = match t with
        | Reg 64 -> extract ((i*8)+2) ((i*8)+0) order_e (* 3 bits *)
        | Reg 128 | Reg 256 -> extract ((i*8)+3) ((i*8)+0) order_e (* 4 bits *)
        | _ -> disfailwith "invalid size for pshufb"
      in
      let index = cast_unsigned t index in
      let atindex = extract_byte_symbolic dst_e index in
      ite r8 highbit (it 0 r8) atindex
    in
    let n = (Typecheck.bits_of_width t) / 8 in
    let e = concat_explist (map get_bit ((n-1)---0)) in
    (match vsrc with
    | None -> [assn t dst e]
    | Some vdst -> [assn t vdst e])
  | Lea(t, r, a) when pref = [] ->
    (* See Table 3-64 *)
    let a = match Typecheck.infer_ast a, t with
      | Reg addrbits, Reg opbits when addrbits > opbits -> cast_low t a
      | Reg _, Reg _ -> a
      | _ -> failwith "impossible"
    in
    [assn t r a]
  | Call(o1, ra) when pref = [] ->
    (* If o1 is an immediate, we should syntactically have Jump(imm)
       so that the CFG algorithm knows where the jump goes.  Otherwise
       it will point to BB_Indirect.

       Otherwise, we should evaluate the operand before decrementing esp.
       (This really only matters when esp is the base register of a memory
       lookup. *)
    let target = op2e mt o1 in
    (match o1 with
    | Oimm _ ->
      [move rsp (rsp_e -* mi (bytes_of_width mt));
       store_s mode None mt rsp_e (big_int_of_mode mode ra);
       Jmp(target, calla)]
    | _ ->
      let t = nt "target" mt in
      [move t target;
       move rsp (rsp_e -* mi (bytes_of_width mt));
       store_s mode None mt rsp_e (big_int_of_mode mode ra);
       Jmp(Var t, calla)])
  | Jump(o) ->
    [Jmp(jump_target mode ss has_rex o, [])]
  | Jcc(o, c) ->
    cjmp c (jump_target mode ss has_rex o)
  | Setcc(t, o1, c) ->
    [assn t o1 (cast_unsigned t c)]
  | Shift(st, s, dst, shift) ->
    assert (List.mem s [r8; r16; r32; r64]);
    let origCOUNT, origDEST = nt "origCOUNT" s, nt "origDEST" s
    and size = it (bits_of_width s) s
    and s_f = match st with LSHIFT -> (<<*) | RSHIFT -> (>>*)
      | ARSHIFT -> (>>>*) | _ -> disfailwith "invalid shift type"
    and dste = op2e s dst in
    let count_mask = size -* (it 1 s) in
    let count = (op2e s shift) &* count_mask in
    let ifzero = ite r1 (Var origCOUNT ==* (it 0 s))
    and new_of = match st with
      | LSHIFT -> (cast_high r1 dste) ^* cf_e
      | RSHIFT -> cast_high r1 (Var origDEST)
      | ARSHIFT -> exp_false
      | _ -> disfailwith "impossible"
    in
    let unk_of = Unknown("OF undefined after shift", r1) in
    let new_cf =
      (* undefined for SHL and SHR instructions where the count is greater than
         or equal to the size (in bits) of the destination operand *)
      match st with
      | LSHIFT -> cast_low r1 (Var origDEST >>* (size -* Var origCOUNT))
      | RSHIFT | ARSHIFT ->
        cast_high r1 (Var origDEST <<* (size -* Var origCOUNT))
      | _ -> failwith "impossible"
    in
    [move origDEST dste;
     move origCOUNT count;
     assn s dst (s_f dste count);
     move cf (ifzero cf_e new_cf);
     move oF (ifzero of_e (ite r1 (Var origCOUNT ==* (it 1 s)) new_of unk_of));
     move sf (ifzero sf_e (compute_sf dste));
     move zf (ifzero zf_e (compute_zf s dste));
     move pf (ifzero pf_e (compute_pf s dste));
     move af (ifzero af_e (Unknown("AF undefined after shift", r1)))
    ]
  | Shiftd(st, s, dst, fill, count) ->
      let origDEST, origCOUNT = nt "origDEST" s, nt "origCOUNT" s in
      let e_dst = op2e s dst in
      let e_fill = op2e s fill in
      (* Check for 64-bit operand *)
      let size = it (bits_of_width s) s in
      let count_mask = size -* (it 1 s) in
      let e_count = (op2e s count) &* count_mask in
      let new_cf =  match st with
        | LSHIFT -> cast_low r1 (Var origDEST >>* (size -* Var origCOUNT))
        | RSHIFT -> cast_high r1 (Var origDEST <<* (size -* Var origCOUNT))
        | _ -> disfailwith "impossible" in
      let ifzero = ite r1 ((Var origCOUNT) ==* (it 0 s)) in
      let new_of = cast_high r1 (Var origDEST) ^* cast_high r1 e_dst in
      let unk_of =
        Unknown ("OF undefined after shiftd of more then 1 bit", r1) in
      let ret1 = match st with
        | LSHIFT -> e_fill >>* (size -* Var origCOUNT)
        | RSHIFT -> e_fill <<* (size -* Var origCOUNT)
        | _ -> disfailwith "impossible" in
      let ret2 = match st with
        | LSHIFT -> e_dst <<* Var origCOUNT
        | RSHIFT -> e_dst >>* Var origCOUNT
        | _ -> disfailwith "impossible" in
      let result = ret1 |* ret2 in
      (* SWXXX If shift is greater than the operand size, dst and
         flags are undefined *)
      [
        move origDEST e_dst;
        move origCOUNT e_count;
        assn s dst result;
        move cf (ifzero cf_e new_cf);
        (* For a 1-bit shift, the OF flag is set if a sign change occurred;
           otherwise, it is cleared. For shifts greater than 1 bit, the OF flag
           is undefined. *)
        move oF (ifzero of_e (ite r1 ((Var origCOUNT) ==* i32 1) new_of unk_of));
        move sf (ifzero sf_e (compute_sf e_dst));
        move zf (ifzero zf_e (compute_zf s e_dst));
        move pf (ifzero pf_e (compute_pf s e_dst));
        move af (ifzero af_e (Unknown ("AF undefined after shiftd", r1)))
      ]
  | Rotate(rt, s, dst, shift, use_cf) ->
    (* SWXXX implement use_cf *)
    if use_cf then unimplemented "rotate use_vf";
    let origCOUNT = nt "origCOUNT" s in
    let e_dst = op2e s dst in
    let shift_val = match s with
      | Reg 64 -> 63
      | _ -> 31
    in
    let e_shift = op2e s shift &* it shift_val s in
    let size = it (bits_of_width s) s in
    let new_cf = match rt with
      | LSHIFT -> cast_low r1 e_dst
      | RSHIFT -> cast_high r1 e_dst
      | _ -> disfailwith "impossible" in
    let new_of = match rt with
      | LSHIFT -> cf_e ^* cast_high r1 e_dst
      | RSHIFT -> cast_high r1 e_dst ^* cast_high r1 (e_dst <<* it 1 s)
      | _ -> disfailwith "impossible" in
    let unk_of =
      Unknown ("OF undefined after rotate of more then 1 bit", r1) in
    let ifzero = ite r1 (Var origCOUNT ==* it 0 s) in
    let ret1 = match rt with
        | LSHIFT -> e_dst <<* Var origCOUNT
        | RSHIFT -> e_dst >>* Var origCOUNT
        | _ -> disfailwith "impossible" in
    let ret2 = match rt with
        | LSHIFT -> e_dst >>* (size -* Var origCOUNT)
        | RSHIFT -> e_dst <<* (size -* Var origCOUNT)
        | _ -> disfailwith "impossible" in
    let result = ret1 |* ret2 in
    [
      move origCOUNT e_shift;
      assn s dst result;
      (* cf must be set before of *)
      move cf (ifzero cf_e new_cf);
      move oF (ifzero of_e (ite r1 (Var origCOUNT ==* it 1 s) new_of unk_of));
    ]
  | Bt(t, bitoffset, bitbase) ->
      let offset = op2e t bitoffset in
      let value, shift = match bitbase with
        | Oreg i ->
            let reg = op2e t bitbase in
            let shift = offset &* it (bits_of_width t - 1) t in
            reg, shift
        | Oaddr a ->
            let byte = load r8 (a +* (offset >>* (it 3 t))) in
            let shift = (cast_low r8 offset) &* (it 7 r8) in
            byte, shift
        | Ovec _ | Oseg _ | Oimm _ -> disfailwith "Invalid bt operand"
      in
      [
        move cf (cast_low r1 (value >>* shift));
        move oF (Unknown ("OF undefined after bt", r1));
        move sf (Unknown ("SF undefined after bt", r1));
        move af (Unknown ("AF undefined after bt", r1));
        move pf (Unknown ("PF undefined after bt", r1))
      ]
  | Bs(t, dst, src, dir) ->
    let source_is_zero = nt "t" r1 in
    let source_is_zero_v = Var source_is_zero in
    let src_e = op2e t src in
    let bits = bits_of_width t in
    let check_bit bitindex next_value =
      ite t (Extract(biconst bitindex,biconst bitindex,src_e) ==* it 1 r1) (it bitindex t) next_value
    in
    let bitlist = List.of_enum (0 --- (bits-1)) in
    (* We are folding from right to left *)
    let bitlist = match dir with
      | Forward -> (* least significant first *) bitlist
      | Backward -> (* most significant *) List.rev bitlist
    in
    let first_one = List.fold_right check_bit bitlist (Unknown("bs: destination undefined when source is zero", t)) in
    [
      move source_is_zero (src_e ==* it 0 t);
      assn t dst first_one;
      move zf (ite r1 source_is_zero_v (it 1 r1) (it 0 r1));
    ]
    @
      let undef (Var.V(_, n, t) as r) = move r (Unknown ((n^" undefined after bsf"), t)) in
      List.map undef [cf; oF; sf; af; pf]
  | Hlt ->
    [Halt(rax_e, [])]
  | Rdtsc ->
    let undef reg = assn r32 reg (Unknown ("rdtsc", r32)) in
    List.map undef [o_rax; o_rdx]
  | Cpuid ->
    let undef reg = assn r32 reg (Unknown ("cpuid", r32)) in
    List.map undef [o_rax; o_rbx; o_rcx; o_rdx]
  | Xgetbv ->
    let undef reg = assn r32 reg (Unknown ("xgetbv", r32)) in
    List.map undef [o_rax; o_rdx]
  | Stmxcsr (dst) ->
      let dst = match dst with
        | Oaddr addr -> addr
        | _ -> disfailwith "stmxcsr argument cannot be non-memory"
      in
      [
        store r32 dst (Var mxcsr);(*(Unknown ("stmxcsr", r32));*)
      ]
  | Ldmxcsr (src) ->
      let src = match src with
        | Oaddr addr -> addr
        | _ -> disfailwith "ldmxcsr argument cannot be non-memory"
      in
      [
        move mxcsr (load r32 src);
      ]
  | Fnstcw (dst) ->
      let dst = match dst with
        | Oaddr addr -> addr
        | _ -> disfailwith "fnstcw argument cannot be non-memory"
      in
      [
        store r16 dst (Var fpu_ctrl);
      ]
  | Fldcw (src) ->
      let src = match src with
        | Oaddr addr -> addr
        | _ -> disfailwith "fldcw argument cannot be non-memory"
      in
      [
        move fpu_ctrl (load r16 src);
      ]
  | Fld (src) ->
    unimplemented "unsupported FPU register stack"
  | Fst (dst,pop) ->
    unimplemented "unsupported FPU flags"
  | Cmps(Reg bits as t) ->
    let src1 = nt "src1" t and src2 = nt "src2" t and tmpres = nt "tmp" t in
    let stmts =
      move src1 (op2e t (Oaddr rsi_e))
      :: move src2 (op2e_s mode seg_es has_rex t (Oaddr rdi_e))
      :: move tmpres (Var src1 -* Var src2)
      :: string_incr mode t rsi
      :: string_incr mode t rdi
      :: set_flags_sub t (Var src1) (Var src2) (Var tmpres)
    in
    if pref = [] then
      stmts
    else if pref = [repz] || pref = [repnz] then
      rep_wrap ~mode ~check_zf:(List.hd pref) ~addr ~next stmts
    else
      unimplemented "unsupported flags in cmps"
  | Lods(Reg bits as t) ->
    let stmts =
      assn t o_rax (op2e t (Oaddr rsi_e))
      :: string_incr mode t rsi
      :: []
    in
    if pref = [] then
      stmts
    else if pref = [repz] || pref = [repnz] then
      rep_wrap ~mode ~check_zf:(List.hd pref) ~addr ~next stmts
    else unimplemented "unsupported flags in lods"
  | Scas(Reg bits as t) ->
    let src1 = nt "src1" t and src2 = nt "src2" t and tmpres = nt "tmp" t in
    let stmts =
      move src1 (cast_low t (Var rax))
      :: move src2 (op2e_s mode seg_es has_rex t (Oaddr rdi_e))
      :: move tmpres (Var src1 -* Var src2)
      :: string_incr mode t rdi
      :: set_flags_sub t (Var src1) (Var src2) (Var tmpres)
    in
    if pref = [] then
      stmts
    else if pref = [repz] || pref = [repnz] then
      rep_wrap ~mode ~check_zf:(List.hd pref) ~addr ~next stmts
    else
      unimplemented "unsupported flags in scas"
  | Stos(Reg bits as t) ->
    let stmts = [store_s mode seg_es t rdi_e (op2e t o_rax);
                 string_incr mode t rdi]
    in
    if pref = [] then
      stmts
    else if pref = [repz] then
      rep_wrap ~mode ~addr ~next stmts
    else
      unimplemented "unsupported prefix for stos"
  | Push(t, o) ->
    let tmp = nt "t" t in (* only really needed when o involves esp *)
    move tmp (op2e t o)
    :: move rsp (rsp_e -* mi (bytes_of_width t))
    :: store_s mode seg_ss t rsp_e (Var tmp) (* FIXME: can ss be overridden? *)
    :: []
  | Pop(t, o) ->
    (* From the manual:

       "The POP ESP instruction increments the stack pointer (ESP)
       before data at the old top of stack is written into the
       destination"

       So, effectively there is no incrementation.
    *)
    assn t o (load_s mode seg_ss t rsp_e)
    :: if o = o_rsp then []
      else [move rsp (rsp_e +* mi (bytes_of_width t))]
  | Pushf(t) ->
    (* Note that we currently treat these fields as unknowns, but the
       manual says: When copying the entire EFLAGS register to the
       stack, the VM and RF flags (bits 16 and 17) are not copied;
       instead, the values for these flags are cleared in the EFLAGS
       image stored on the stack. *)
    let flags_e = match t with
      | Reg 16 -> flags_e
      | Reg 32 -> eflags_e
      | Reg 64 -> rflags_e
      | _ -> failwith "impossible"
    in
    move rsp (rsp_e -* mi (bytes_of_width t))
    :: store_s mode seg_ss t rsp_e flags_e
    :: []
  | Popf t ->
    let assnsf = match t with
      | Reg 16 -> assns_flags_to_bap
      | Reg 32 -> assns_eflags_to_bap
      | Reg 64 -> assns_rflags_to_bap
      | _ -> failwith "impossible"
    in
    let tmp = nt "t" t in
    let extractlist =
      BatList.of_enum (map
        (fun i ->
          extract i i (Var tmp))
        (((bits_of_width t)-1)---0))
    in
    move tmp (load_s mode seg_ss t rsp_e)
    :: move rsp (rsp_e +* mi (bytes_of_width t))
    :: List.flatten (List.map2 (fun f e -> f e) assnsf extractlist)
  | Popcnt(t, s, d) ->
    let width = bits_of_width t in
    let bits = op2e t s in
    let bitvector = Array.to_list (Array.init width (fun i -> ite t (extract i i bits) (it 1 t) (it 0 t))) in
    let count = BatList.reduce (+*) bitvector in
    set_zf t bits
    :: assn t d count
    :: List.map (fun r -> move r (it 0 r1)) [cf; oF; sf; af; pf]
  | Sahf ->
    let assnsf = assns_lflags_to_bap in
    let tah = nt "AH" r8 in
    let extractlist =
      BatList.of_enum (map
        (fun i ->
          extract i i (Var tah))
        (7---0))
    in
    move tah ah_e
    :: List.flatten (List.map2 (fun f e -> f e) assnsf extractlist)
  | Lahf ->
    let o_ah = Oreg 4 in
    [assn r8 o_ah lflags_e]
  | Add(t, o1, o2) ->
    let tmp = nt "t1" t and tmp2 = nt "t2" t in
    move tmp (op2e t o1)
    :: move tmp2 (op2e t o2)
    :: assn t o1 (op2e t o1 +* Var tmp2)
    :: let s1 = Var tmp and s2 = Var tmp2 and r = op2e t o1 in
       set_flags_add t s1 s2 r
  | Adc(t, o1, o2) ->
    let orig1 = nt "orig1" t and orig2 = nt "orig2" t in
    let bits = bits_of_width t in
    let t' = Reg (bits + 1) in
    let c = cast_unsigned t' in
    (* Literally compute the addition with an extra bit and see
       what the value is for CF *)
    let s1 = Var orig1 and s2 = Var orig2 and r = op2e t o1 in
    let bige = c s1 +* c s2 +* c (cast_unsigned t cf_e) in
    move orig1 (op2e t o1)
    :: move orig2 (op2e t o2)
    :: assn t o1 (s1 +* s2 +* cast_unsigned t cf_e)
    :: move cf (extract bits bits bige)
    :: set_aopszf_add t s1 s2 r
  | Inc(t, o) (* o = o + 1 *) ->
    let tmp = nt "t" t in
    move tmp (op2e t o)
    :: assn t o (op2e t o +* it 1 t)
    :: set_aopszf_add t (Var tmp) (it 1 t) (op2e t o)
  | Dec(t, o) (* o = o - 1 *) ->
    let tmp = nt "t" t in
    move tmp (op2e t o)
    :: assn t o (op2e t o -* it 1 t)
    :: set_aopszf_sub t (Var tmp) (it 1 t) (op2e t o) (* CF is maintained *)
  | Sub(t, o1, o2) (* o1 = o1 - o2 *) ->
    let oldo1 = nt "t" t in
    move oldo1 (op2e t o1)
    :: assn t o1 (op2e t o1 -* op2e t o2)
    :: set_flags_sub t (Var oldo1) (op2e t o2) (op2e t o1)
  | Sbb(t, o1, o2) ->
    let tmp_s = nt "ts" t in
    let tmp_d = nt "td" t in
    let orig_s = Var tmp_s in
    let orig_d = Var tmp_d in
    let sube = orig_s +* cast_unsigned t cf_e in
    let d = op2e t o1 in
    let s1 = op2e t o2 in
    move tmp_s s1
    :: move tmp_d d
    :: assn t o1 (orig_d -* sube)
    :: move oF (cast_high r1 ((orig_s ^* orig_d) &* (orig_d ^* d)))
    (* When src = 0xffffffff and cf=1, the processor sets CF=1.

       Note that we compute dest = dest - (0xffffffff + 1) = 0, so the
       subtraction does not overflow.

       So, I am guessing that CF is set if the subtraction overflows
       or the addition overflows.

       Maybe we should implement this by doing the actual computation,
       like we do for adc.
    *)
               (* sub overflow | add overflow *)
    :: move cf ((sube >* orig_d) |* (sube <* orig_s))
    :: set_apszf t orig_s orig_d d
  | Cmp(t, o1, o2) ->
    let tmp = nt "t" t in
    move tmp (op2e t o1 -* op2e t o2)
    :: set_flags_sub t (op2e t o1) (op2e t o2) (Var tmp)
  | Cmpxchg(t, src, dst) ->
    let eax_e = op2e t o_rax in
    let dst_e = op2e t dst in
    let src_e = op2e t src in
    let tmp = nt "t" t in
    move tmp (eax_e -* dst_e)
    :: set_flags_sub t eax_e dst_e (Var tmp)
    @ assn t dst (ite t zf_e src_e dst_e)
    :: assn t o_rax (ite t zf_e eax_e dst_e)
    :: []
  | Cmpxchg8b o -> (* only 32bit case *)
    let accumulator = Concat((op2e r32 o_rdx),(op2e r32 o_rax)) in
    let dst_e = op2e r64 o in
    let src_e = Concat((op2e r32 o_rcx),(op2e r32 o_rbx)) in
    let dst_low_e = Extract(biconst 63, biconst 32, dst_e) in
    let dst_hi_e = Extract(biconst 31, bi0, dst_e) in
    let eax_e = op2e r32 o_rax in
    let edx_e = op2e r32 o_rdx in
    let equal = nt "t" r1 in
    let equal_v = Var equal in
    [
      move equal (accumulator ==* dst_e);
      move zf equal_v;
      assn r64 o (ite r64 equal_v src_e dst_e);
      assn r32 o_rax (ite r32 equal_v eax_e dst_low_e);
      assn r32 o_rdx (ite r32 equal_v edx_e dst_hi_e)
    ]
  | Xadd(t, dst, src) ->
    let tmp = nt "t" t in
    move tmp (op2e t dst +* op2e t src)
    :: assn t src (op2e t dst)
    :: assn t dst (Var tmp)
    :: let s = Var tmp and src = op2e t src and dst = op2e t dst in
       set_flags_add t s src dst
  | Xchg(t, src, dst) ->
    let tmp = nt "t" t in
    [
      move tmp (op2e t src);
      assn t src (op2e t dst);
      assn t dst (Var tmp);
    ]
  | And(t, o1, o2) ->
    assn t o1 (op2e t o1 &* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after and", r1))
    :: set_pszf t (op2e t o1)
  | Or(t, o1, o2) ->
    assn t o1 (op2e t o1 |* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after or", r1))
    :: set_pszf t (op2e t o1)
  | Xor(t, o1, o2) when o1 = o2->
    assn t o1 (Int(bi0,t))
    :: move af (Unknown("AF is undefined after xor", r1))
    :: List.map (fun v -> move v exp_true) [zf; pf]
    @  List.map (fun v -> move v exp_false) [oF; cf; sf]
  | Xor(t, o1, o2) ->
    assn t o1 (op2e t o1 ^* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after xor", r1))
    :: set_pszf t (op2e t o1)
  | Test(t, o1, o2) ->
    let tmp = nt "t" t in
    move tmp (op2e t o1 &* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after and", r1))
    :: set_pszf t (Var tmp)
  | Ptest(t, o1, o2) ->
    let tmp1 = nt "t1" t in
    let tmp2 = nt "t2" t in
    move tmp1 (op2e t o2 &* op2e t o1)
    :: move tmp2 (op2e t o2 &* (exp_not (op2e t o1)))
    :: move af exp_false
    :: move oF exp_false
    :: move pf exp_false
    :: move sf exp_false
    :: move zf ((Var tmp1) ==* (Int(bi0, t)))
    :: [move cf ((Var tmp2) ==* (Int(bi0, t)))]
  | Not(t, o) ->
    [assn t o (exp_not (op2e t o))]
  | Neg(t, o) ->
    let tmp = nt "t" t in
    let min_int =
      Ast_convenience.binop LSHIFT (it 1 t) (it ((bits_of_width t)-1) t)
    in
    move tmp (op2e t o)
    ::assn t o (it 0 t -* op2e t o)
    ::move cf (ite r1 (Var tmp ==* it 0 t) (it 0 r1) (it 1 r1))
    ::move oF (ite r1 (Var tmp ==* min_int) (it 1 r1) (it 0 r1))
    ::set_apszf_sub t (Var tmp) (it 0 t) (op2e t o)
  | Mul (t, src) ->
    (* Mul always multiplies EAX by src and stores the result in EDX:EAX
       starting from the "right hand side" based on the type t of src *)

    (* The OF and CF flags are set to 0 if the upper half of the result is 0;
       otherwise, they are set to 1 *)
    let new_t = Reg ((bits_of_width t)*2) in
    let assnstmts, assne = assn_dbl t ((cast_unsigned new_t (op2e t o_rax)) ** (cast_unsigned new_t (op2e t src)))
    in
    let flag =
      let highbit = bits_of_width new_t - 1 in
      let lowbit = bits_of_width new_t / 2 in
      extract highbit lowbit assne <>* it 0 t
    in
    assnstmts
      @
      [
        move oF flag;
        move cf flag;
        move sf (Unknown("SF is undefined after Mul", r1));
        move zf (Unknown("ZF is undefined after Mul", r1));
        move af (Unknown("AF is undefined after Mul", r1));
        move pf (Unknown("PF is undefined after Mul", r1))
      ]
  | Imul (t, (oneopform, dst), src1, src2) ->
    let new_t = Reg ((bits_of_width t)*2) in
    let mul_stmts =
      (match oneopform with
      | true ->
        (* For one operand form, use assn_double *)
        let assnstmts, assne = assn_dbl t ((cast_signed new_t (op2e t src1)) ** (cast_signed new_t (op2e t src2))) in
        let flag =
          (* Intel checks if EAX == EDX:EAX.  Instead of doing this, we are just
             going to check if the upper bits are != 0 *)
          let highbit = bits_of_width new_t - 1 in
          let lowbit = bits_of_width new_t / 2 in
          extract highbit lowbit assne <>* it 0 t
        in
        assnstmts @
          [move oF flag;
           move cf flag]
      | false ->
        (* Two and three operand forms *)
        let tmp = nt "t" new_t in
        (* Flag is set when the result is truncated *)
        let flag = (Var tmp <>* cast_signed new_t (op2e t dst)) in
        [(move tmp ((cast_signed new_t (op2e t src1)) ** (cast_signed new_t (op2e t src2))));
         (assn t dst (cast_low t (Var tmp)));
         move oF flag;
         move cf flag]
      )
    in
    mul_stmts@[
      move pf (Unknown("PF is undefined after imul", r1));
      move sf (Unknown("SF is undefined after imul", r1));
      move zf (Unknown("ZF is undefined after imul", r1));
      move af (Unknown("AF is undefined after imul", r1));
    ]
  | Div(t, src) ->
    let dt = Reg (bits_of_width t * 2) in
    let dividend = op2e_dbl t in
    let divisor = cast_unsigned dt (op2e t src) in
    let tdiv = nt "div" dt in
    let trem = nt "rem" dt in
    let assne = cast_low t (Var trem) ++* cast_low t (Var tdiv) in
    Assert(divisor <>* it 0 dt, [StrAttr "#DE"])
    :: move tdiv (dividend /* divisor)
    :: move trem (dividend %* divisor)
    (* Overflow is indicated with the #DE (divide error) exception
       rather than with the CF flag. *)
    :: [Assert(cast_high t (Var tdiv) ==* it 0 t, [StrAttr "#DE"])]
    @ fst (assn_dbl t assne)
    @ (let undef (Var.V(_, n, t) as r) =
         move r (Unknown ((n^" undefined after div"), t))
       in
       List.map undef [cf; oF; sf; zf; af; pf])
  | Idiv(t, src) ->
    let dt = Reg (bits_of_width t * 2) in
    let dividend = op2e_dbl t in
    let divisor = cast_signed dt (op2e t src) in
    let tdiv = nt "div" dt in
    let trem = nt "rem" dt in
    let assne = cast_low t (Var trem) ++* cast_low t (Var tdiv) in
    Assert(divisor <>* it 0 dt, [StrAttr "#DE"])
    :: move tdiv (dividend $/* divisor)
    :: [move trem (dividend $%* divisor)]
    (* Overflow is indicated with the #DE (divide error) exception
       rather than with the CF flag. *)
    (* SWXXX For signed division make sure quotient is between smallest and
       largest values.  For type t, this would be -2^(t/2) to (2^(t/2) - 1). *)
    (* :: [Assert(cast_high t (Var tdiv) ==* it 0 t, [StrAttr "#DE"])] *)
    @ fst (assn_dbl t assne)
    @ (let undef (Var.V(_, n, t) as r) =
         move r (Unknown ((n^" undefined after div"), t)) in
       List.map undef [cf; oF; sf; zf; af; pf])
  | Cld ->
    [Move(df, exp_false, [])]
  | Leave t when pref = [] -> (* #UD if Lock prefix is used *)
    Move(rsp, rbp_e, [])
    ::to_ir mode addr next ss pref has_rex has_vex (Pop(t, o_rbp))
  | Interrupt3 ->
    [Special("int3", Some {Var.defs = []; Var.uses = []}, [])]
  | Interrupt(Oimm i) ->
    [Special(Printf.sprintf "int 0x%s" (Util.big_int_to_hex i), None, [])]
  | Sysenter | Syscall ->
    [Special("syscall", None, [])]
  (* Match everything exhaustively *)
  | Leave _ ->  unimplemented "to_ir: Leave"
  | Call _ ->  unimplemented "to_ir: Call"
  | Lea _ ->  unimplemented "to_ir: Lea"
  | Movs _ ->  unimplemented "to_ir: Movs"
  | Cmps _ ->  unimplemented "to_ir: Cmps"
  | Lods _ ->  unimplemented "to_ir: Lods"
  | Scas _ ->  unimplemented "to_ir: Scas"
  | Stos _ ->  unimplemented "to_ir: Stos"
  | Retn _ ->  unimplemented "to_ir: Retn"
  | Interrupt _ ->  unimplemented "to_ir: Interrupt"

let add_labels ?(asm) a ir =
  let attr = match asm with None -> [] | Some s -> [Asm(s)] in
  Label(Addr a, attr)
  ::Label(Name(Printf.sprintf "pc_0x%s" (Util.big_int_to_hex a)),[])
  ::ir

end (* ToIR *)


(* extract the condition to jump on from the opcode bits
for 70 to 7f and 0f 80 to 8f *)
let cc_to_exp i =
  let cc = match i & 0xe with
    | 0x0 -> of_e
    | 0x2 -> cf_e
    | 0x4 -> zf_e
    | 0x6 -> cf_e |* zf_e
    | 0x8 -> sf_e
    | 0xa -> pf_e
    | 0xc -> sf_e ^* of_e
    | 0xe -> zf_e |* (sf_e ^* of_e)
    | _ -> disfailwith "impossible condition code"
  in
  if (i & 1) = 0 then cc else exp_not cc

let parse_instr mode g addr =
  let s = succ_big_int in
  let bm = big_int_of_mode mode in
  let im = int_of_mode mode in
  let tm = type_of_mode mode in
  let get_prefix c =
    let i = Char.code c in
    match i with
    | 0xf0 | 0xf2 | 0xf3 | 0x2e | 0x36 | 0x3e | 0x26 | 0x64 | 0x65
    | 0x66 | 0x67 -> Some i
    | _ -> None
  in
  let get_prefixes a =
    let rex a = match mode with
      | X86 -> None, a
      | X8664 ->
        let i = Char.code (g a) in
        if i >= 0x40 && i <= 0x4f
        then Some i, s a
        else None, a
    in
    let rec f l a =
      match get_prefix (g a) with
      | Some p -> f (p::l) (s a)
      | None -> (l, a)
    in
    (* Legacy prefixes *)
    let leg, a = f [] a in
    (* Add rex *)
    let rex, a = rex a in
    rex, leg, a
  in
  let parse_rex i =
    {
      rex_w = i land 0x8 = 0x8;
      rex_r = i land 0x4 = 0x4;
      rex_x = i land 0x2 = 0x2;
      rex_b = i land 0x1 = 0x1;
    }
  in
  let get_vex a =
    if mode = X86 then None, a else
    match Char.code (g a) with
    (* 3-byte prefix *)
    | 0xc4 | 0x8f ->
      let a = (s a) in
      let b1, a = Char.code (g a), (s a) in
      let b2, a = Char.code (g a), (s a) in
      Some {
        vex_nr = b1 land 0x80 = 0x80;
        vex_nx = b1 land 0x40 = 0x40;
        vex_nb = b1 land 0x20 = 0x20;
        vex_map_select = b1 land 0x1f;
        vex_we = b2 land 0x80 = 0x80;
        vex_v = (b2 land 0x78) >> 3;
        vex_l = b2 land 0x4 = 0x4;
        vex_pp = b2 land 0x3;
      }, a
    (* 2-byte prefix *)
    | 0xc5 ->
      let a = (s a) in
      let b1, a = Char.code (g a), (s a) in
      Some {
        vex_nr = b1 land 0x80 = 0x80;
        vex_nx = true;
        vex_nb = true;
        vex_map_select = 1;
        vex_we = false;
        vex_v = (b1 land 0x78) >> 3;
        vex_l = b1 land 0x4 = 0x4;
        vex_pp = b1 land 0x3;
      }, a
    | _ -> None, a
  in

(*  let int2prefix ?(jmp=false) = function
    | 0xf0 -> Some Lock
    | 0xf2 -> Some Repnz
    | 0xf3 -> Some Repz
    | 0x2e when jmp-> Some Hint_bnt
    | 0x3e when jmp-> Some Hint_bt
    | 0x2e -> Some(Override CS)
    | 0x36 -> Some(Override SS)
    | 0x3e -> Some(Override DS)
    | 0x26 -> Some(Override ES)
    | 0x64 -> Some(Override FS)
    | 0x65 -> Some(Override GS)
    | 0x66 -> Some Op_size
    | 0x0f -> Some Mandatory_0f
    | 0x67 -> Some Address_size
    | _ -> None
  in*)
  let parse_int nbits a =
    let r a n = (bi (Char.code (g (a +% (big_int_of_int n))))) <<% (8*n) in
    let nbytes = nbits/8 in
    let bytes = map (fun n -> r a n) (0 -- (nbytes-1)) in
    let i = reduce or_big_int bytes in
    (i, a +% (big_int_of_int nbytes))
  in
  let parse_int8 = parse_int 8 in
  let parse_int16 = parse_int 16 in
  let parse_int32 = parse_int 32 in
  let parse_int64 = parse_int 64 in

  let to_signed i t = Arithmetic.to_sbig_int (i, t) in
  let parse_sint nbits a =
    let (i, na) = parse_int nbits a in
    (to_signed i (Reg nbits), na)
  in
  let parse_sint8 = parse_sint 8 in
  let parse_sint16 = parse_sint 16 in
  let parse_sint32 = parse_sint 32 in
  let parse_sint64 = parse_sint 64 in

  let parse_disp8 = parse_sint8
  and parse_disp16 = parse_sint16
  and parse_disp32 = parse_sint32
  and parse_disp64 = parse_sint64
  in
  let parse_disp:(Type.typ -> big_int -> big_int * big_int) = function
    | Reg 8 ->  parse_disp8
    | Reg 16 -> parse_disp16
    | Reg 32 -> parse_disp32
    | Reg 64 -> parse_disp64
    | _ -> disfailwith "unsupported displacement size"
  in
  let parse_imm8cb b =
    let b = Big_int_Z.int64_of_big_int b in
    let open Pcmpstr in
    let (&) = Int64.logand in
    let ssize = if (b & 1L) = 0L then Bytes else Words in
    let ssign = if (b & 2L) = 0L then Unsigned else Signed in
    let agg = match b & 12L with
      | 0L -> EqualAny
      | 4L -> Ranges
      | 8L -> EqualEach
      | 12L -> EqualOrdered
      | _ -> failwith "impossible"
    in
    let negintres1 = if (b & 16L) = 0L then false else true in
    let maskintres1 = if (b & 32L) = 0L then false else true in
    let outselectsig = if (b & 64L) = 0L then LSB else MSB in
    let outselectmask = sig_to_mask outselectsig in
    if (b & 128L) <> 0L then wprintf "Most significant bit of Imm8 control byte should be set to 0";

    {ssize=ssize; ssign=ssign; agg=agg; negintres1=negintres1; maskintres1=maskintres1; outselectsig=outselectsig; outselectmask=outselectmask}

  in
  let parse_sib rex modb a =
    (* ISR 2.1.5 Table 2-3 *)
    let e b = (if b then 1 else 0) << 3 in (* tuareg >> *)
    let rex_b, rex_x = match rex with
      | Some {rex_b; rex_x} -> rex_b, rex_x
      | None -> false, false
    in
    let b = Char.code (g a) in
    let bits2rege = match mode with
      | X86 -> bits2reg32e
      | X8664 -> bits2reg64e
    in
    let ss = b >> 6 and idx = ((b>>3) & 7) lor (e rex_x) in
    let base, na =
      match (b & 7, modb & 7) with (* base register, MOD *)
      | 5, 0 -> let (i,na) = parse_disp32 (s a) in (bm i, na)
      | _, 0 | _, 1 | _, 2 -> (bits2rege mode ((b & 7) lor (e rex_b)), s a)
      | _ -> disfailwith (Printf.sprintf "impossible opcode: sib b=%02x" b)
    in
    if idx = 4 then (base, na) else
      let idx = bits2rege mode idx in
      if ss = 0 then (base +* idx, na)
      else (base +* (idx <<* im ss), na)
  in
  (* Parse mod/rm bits helper function *)
  let parse_modrmbits a =
    let b = Char.code (g a)
    and na = s a in
    let r = (b>>3) & 7
    and m = b >> 6
    and rm = b & 7 in
    (b, r, m, rm, na)
  in
  (* Parse mod/rm bits, but also apply bits from REX prefix *)
  let parse_modrmbits64 rex a =
    let e b = (if b then 1 else 0) << 3 in (* tuareg >> *)
    let b, r, modb, rm, na = parse_modrmbits a in
    let rex_r, rex_x, rex_b = match rex with
      | Some {rex_r; rex_x; rex_b} -> rex_r, rex_x, rex_b
      | None -> false, false, false
    in
    let r = e rex_r lor r in
    let rm = e rex_b lor rm in
    b, r, modb, rm, na
  in
  (* Parse mod/rm operands for 16-bit addressing mode *)
  let parse_modrm16int a b r modb rm na =
    (* ISR 2.1.5 Table 2-1 *)
    match modb & 7 with (* MOD *)
    | 0 -> (match rm & 7 with
      | 6 -> let (disp, na) = parse_disp16 na in (r, Oaddr(bm disp), na)
      | n when n < 8 -> (r, Oaddr(cast_unsigned tm (eaddr16 mode rm)), na)
      | _ -> disfailwith "Impossible"
    )
    | 1 | 2 ->
      let (base, na) = eaddr16 mode rm, na in
      let (disp, na) =
        if (modb & 7) = 1 then parse_disp8 na else (*2*) parse_disp16 na
      in
      (r, Oaddr(cast_unsigned tm (base +* b16 disp)), na)
    | 3 -> (r, Oreg rm, na)
    | _ -> disfailwith "Impossible"
  in
  (* External interface to get mod/rm operands for 16-bit addressing
     mode *)
  let parse_modrm16ext rex _iaoffset a =
    let b, r, modb, rm, na = parse_modrmbits64 rex a in
    parse_modrm16int a b r modb rm na
  in
  (* Same as parse_modrm16ext, but casts r as a register operand. *)
  let parse_modrm16 rex _iaoffset a =
    let (r, rm, na) = parse_modrm16ext rex _iaoffset a in
    (Oreg r, rm, na)
  in
  (* Same as parse_modrm16ext, but casts r as a segment selector
     operand. *)
  let parse_modrm16seg rex _iaoffset a =
    let (r, rm, na) = parse_modrm16ext rex _iaoffset a in
    (Oseg r, rm, na)
  in
  (* parse_modrm3264int uses the rex prefix to extended registers, but
     vex prefixes can also specify these bits.  This function converts
     from the vex prefix to the canonical rex form used by
     parse_modrm3264int. *)
  let convert_vex rex vex = match rex, vex with
    | Some _, Some _ -> failwith "both rex and vex specified"
    | Some _, None -> rex (* already in rex form *)
    | None, Some {vex_nr; vex_nx; vex_nb; vex_we} ->
      Some {
        rex_r=not vex_nr;
        rex_x=not vex_nx;
        rex_b=not vex_nb;
        rex_w=vex_we;
      }
    | None, None -> None
  in
  (* Parse mod/rm operands for 32 and 64-bit addressing modes. *)
  let parse_modrm3264int rex at ia a b r modb rm naoffset na =
    (* ISR 2.1.5 Table 2-2 *)
    let bm = big_int_of_mode mode in
    let bits2rege = match at with
      | Reg 32 -> (fun b -> cast_unsigned (type_of_mode mode) (bits2reg32e mode b))
      | Reg 64 -> bits2reg64e mode
      | _ -> failwith "parse_modrm3264int: invalid address type"
    in
    match modb & 7 with (* MOD *)
    | 0 -> (match rm & 7 with (* rm & 7 ignores the extended rex bit *)
      | 4 -> let (sib, na) = parse_sib rex modb na in (r, Oaddr sib, na)
      | 5 ->
        (* See Table 2-7. RIP-Relative Addressing.

           In 32-bit mode, this is a displacement.  In 64-bit mode,
           this is RIP + displacement
        *)
        (match mode with
        | X86 ->
          let (disp, na) = parse_disp32 na in (r, Oaddr(b32 disp), na)
        | X8664 ->
          let immoff = match naoffset with
            | Some (Reg nbits) -> i64 (nbits / 8)
            | _ -> i64 0
          in
          let (disp, na) = parse_disp32 na in (r, Oaddr(b64 disp +* b64 na +* immoff), na))
      | n -> (r, Oaddr(bits2rege rm), na)
    )
    | 1 | 2 ->
      let (base, na) =
        (* rm & 7 ignores the extended rex bit *)
        if 4 = (rm & 7) then parse_sib rex modb na else (bits2rege rm, na) in
      let (disp, na) =
        if modb = 1 then parse_disp8 na else (*2*) parse_disp32 na in
      (r, Oaddr(base +* bm disp), na)
    | 3 -> (r, Oreg rm, na)
    | _ -> disfailwith "Impossible"
  in
  (* External interface to get mod/rm operands for 32 and 64-bit
     addressing modes. *)
  let parse_modrm3264ext rex vex at ia immoff a =
    let rex = convert_vex rex vex in
    let b, r, modb, rm, na = parse_modrmbits64 rex a in
    parse_modrm3264int rex at ia a b r modb rm immoff na
  in
  (* Same as parse_modrm3264ext, but casts r as a register. *)
  let parse_modrm3264 rex vex at ia immoff a =
    let r, rm, na = parse_modrm3264ext rex vex at ia immoff a in
    (Oreg r, rm, na)
  in
  (* Same as parse_modrm3264ext, but casts r as a segment selector. *)
  let parse_modrm3264seg rex vex at ia iaoffset a =
    let (r, rm, na) = parse_modrm3264ext rex vex at ia iaoffset a in
    (* Ignore bits from REX prefix for segment selectors *)
    (Oseg (r & 7), rm, na)
  in
  (* Get the extra vvvv operand from the VEX prefix *)
  let get_vex_opr vex = match vex with
    | None -> None
    | Some {vex_v} -> Some (Ovec ((lnot vex_v) land 0xf))
  in
  (* Force an operand to be a vector register if it's a register operand. *)
  let tovec op = match op with
    | Oreg r -> Ovec r
    | _ -> op
  in
  let toreg op = match op with
    | Ovec r -> Oreg r
    | _ -> op
  in
  (* Same as parse_modrm3264ext, but casts r, rm, and the optional vex
     operand as vector registers. *)
  let parse_modrm3264_vec rex vex at ia iaoffset a =
    let r, rm, na = parse_modrm3264 rex vex at ia iaoffset a in
    (tovec r, tovec rm, get_vex_opr vex, na)
  in
  (* Parse 8-bits as unsigned integer *)
  let parse_imm8 a = (* not sign extended *)
    let (i, na) = parse_int8 a in
    (Oimm i, na)
  and parse_simm8 a = (* sign extended *)
    let (i, na) = parse_sint8 a in
    (Oimm i, na)
  and parse_imm16 a =
    let (i, na) = parse_int16 a in
    (Oimm i, na)
  and parse_simm16 a =
    let (i, na) = parse_sint16 a in
    (Oimm i, na)
  and parse_imm32 a =
    let (i, na) = parse_int32 a in
    (Oimm i, na)
  and parse_simm32 a =
    let (i, na) = parse_sint32 a in
    (Oimm i, na)
  and parse_imm64 a =
    let (i, na) = parse_int64 a in
    (Oimm i, na)
  and parse_simm64 a =
    let (i, na) = parse_sint64 a in
    (Oimm i, na)
  in
  let parse_immz t a = match t with
    | Reg 8 -> parse_imm8 a
    | Reg 16 -> parse_imm16 a
    | Reg 32 -> parse_imm32 a
    | Reg 64 -> parse_imm64 a
    | Reg n -> disfailwith ("parse_immz unsupported size: "^(string_of_int n))
    | _ -> disfailwith "parse_immz unsupported size"
  in
  let parse_simm t a = match t with
    | Reg 8 -> parse_simm8 a
    | Reg 16 -> parse_simm16 a
    | Reg 32 -> parse_simm32 a
    | Reg 64 -> parse_simm64 a
    | Reg n -> disfailwith ("parse_simm unsupported size: "^(string_of_int n))
    | _ -> disfailwith "parse_simm unsupported size"
  in
  let parse_immv = parse_immz in
  let parse_immb = parse_imm8 in
  let parse_immw = parse_imm16 in
  (* let parse_immd = parse_imm32 in *)
  let _parse_simmb = parse_simm8 in
  let _parse_simmw = parse_simm16 in
  let _parse_simmd = parse_simm32 in
  (* sign extend op of type ot to size *)
  let sign_ext ot op size = (match op with
    | Oimm d ->
      let (v,_) =
        Arithmetic.cast CAST_SIGNED (d, ot) size
      in
      Oimm v
    | _ -> disfailwith "sign_ext only handles Oimm"
  ) in
  (* This is for opcodes like e9 *)
  let expanded_jump_type opsize = match mode with
    | X86 -> opsize
    | X8664 -> r32
  in
  let get_opcode pref ({rex; vex; rm_extend; addrsize} as prefix) a =
    let parse_disp_addr, parse_modrm_addr, parse_modrmseg_addr, parse_modrmext_addr = match addrsize with
      | Reg 16 -> parse_disp16, parse_modrm16 rex, parse_modrm16seg rex, parse_modrm16ext rex
      | Reg 32 -> parse_disp32, parse_modrm3264 rex vex addrsize a, parse_modrm3264seg rex vex addrsize a, parse_modrm3264ext rex vex addrsize a
      | Reg 64 -> parse_disp64, parse_modrm3264 rex vex addrsize a, parse_modrm3264seg rex vex addrsize a, parse_modrm3264ext rex vex addrsize a
      | t -> failwith "Bad address type"
    in
    let parse_modrm_vec = parse_modrm3264_vec rex vex addrsize a in
    let mi = int_of_mode mode in
    let _mi64 = int64_of_mode mode in
    let mbi = big_int_of_mode mode in
    let mt = type_of_mode mode in
    (* A VEX prefix always implies the first byte of 0x0f *)
    let b1, na = if vex <> None then 0x0f, a else Char.code (g a), s a in
    match b1 with (* Table A-2 *)
        (*** 00 to 3d are near the end ***)
    | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 ->
      (Inc(prefix.opsize, Oreg(rm_extend lor (b1 & 7))), na)
    | 0x48 | 0x49 | 0x4a | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f ->
      (Dec(prefix.opsize, Oreg(rm_extend lor (b1 & 7))), na)
    | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 ->
      (Push(prefix.bopsize, Oreg(rm_extend lor (b1 & 7))), na)
    | 0x58 | 0x59 | 0x5a | 0x5b | 0x5c | 0x5d | 0x5e | 0x5f ->
      (Pop(prefix.bopsize, Oreg(rm_extend lor (b1 & 7))), na)
    | 0x63 when mode = X8664 ->
      let (r, rm, na) = parse_modrm_addr None na in
      (Movsx(prefix.opsize, r, r32, rm), na)
    | 0x68 | 0x6a  ->
      let (o, na) =
        (* SWXXX Sign extend these? *)
        if b1=0x68 then parse_immz prefix.opsize na else parse_immb na
      in
      let size = match mode with
        | X86 -> prefix.opsize
        | X8664 -> r64
      in
      (Push(size, o), na)
    | 0x69 | 0x6b ->
      let it =
        if b1 = 0x6b
        then r8
        else if prefix.opsize = r16 then r16
        else r32
      in
      let (r, rm, na) = parse_modrm_addr (Some it) na in
      let (o, na) = parse_simm it na in
      (Imul(prefix.opsize, (false,r), rm, (sign_ext it o prefix.opsize)), na)
    | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78 | 0x79
    | 0x7a | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f ->
      let (i,na) = parse_disp8 na in
      (Jcc(Jabs(Oimm(i +% na)), cc_to_exp b1), na)
    | 0x80 | 0x81 | 0x82 | 0x83 ->
      let it = match b1 with
        | 0x81 -> if prefix.opsize = r64 then r32 else prefix.opsize
        | _ -> r8
      in
      let (r, rm, na) = parse_modrmext_addr (Some it) na in
      let (o, na) = parse_immz it na in
      let (o2, na) = ((sign_ext it o prefix.opsize), na) in
      let opsize = if b1 land 1 = 0 then r8 else prefix.opsize in
      (match r with (* Grp 1 *)
      | 0 -> (Add(opsize, rm, o2), na)
      | 1 -> (Or(opsize, rm, o2), na)
      | 2 -> (Adc(opsize, rm, o2), na)
      | 3 -> (Sbb(opsize, rm, o2), na)
      | 4 -> (And(opsize, rm, o2), na)
      | 5 -> (Sub(opsize, rm, o2), na)
      | 6 -> (Xor(opsize, rm, o2), na)
      | 7 -> (Cmp(opsize, rm, o2), na)
      | _ -> disfailwith
        (Printf.sprintf "impossible Grp 1 opcode: %02x/%d" b1 r)
      )
    | 0x84
    | 0x85 -> let (r, rm, na) = parse_modrm_addr None na in
              let o = if b1 = 0x84 then r8 else prefix.opsize in
              (Test(o, rm, r), na)
    | 0x87 -> let (r, rm, na) = parse_modrm_addr None na in
              (Xchg(prefix.opsize, r, rm), na)
    | 0x88 -> let (r, rm, na) = parse_modrm_addr None na in
              (Mov(r8, rm, r, None), na)
    | 0x89 -> let (r, rm, na) = parse_modrm_addr None na in
              (Mov(prefix.opsize, rm, r, None), na)
    | 0x8a -> let (r, rm, na) = parse_modrm_addr None na in
              (Mov(r8, r, rm, None), na)
    | 0x8b -> let (r, rm, na) = parse_modrm_addr None na in
              (Mov(prefix.opsize, r, rm, None), na)
    | 0x8c -> let (r, rm, na) = parse_modrmseg_addr None na in
              let extend = if prefix.opsize = r64 then r64 else r16 in
              (Mov(extend, rm, r, None), na)
    | 0x8d -> let (r, rm, na) = parse_modrm_addr None na in
              (match rm with
              | Oaddr a -> (Lea(prefix.opsize, r, a), na)
              | _ -> disfailwith "invalid lea (must be address)")
    | 0x8e -> let (r, rm, na) = parse_modrmseg_addr None na in
              (Mov(r16, r, rm, None), na)
    | 0x90 -> (Nop, na)
    | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 ->
      let reg = Oreg (rm_extend lor (b1 & 7)) in
      (Xchg(prefix.opsize, o_rax, reg), na)
    | 0x98 -> let srct = match prefix.opsize with
              | Reg 16 -> r8
              | Reg 32 -> r16
              | Reg 64 -> r32
              | _ -> disfailwith "invalid opsize for CBW/CWDE/CWQE"
              in
              (Movsx(prefix.opsize, o_rax, srct, o_rax), na)
    | 0x9c -> (Pushf(prefix.bopsize), na)
    (* Intel says that popfq needs to have a REX.W prefix, but gas
       and clang both insist that no prefix is needed! *)
    | 0x9d -> (Popf(prefix.bopsize), na)
    | 0x9e -> (Sahf, na)
    | 0x9f -> (Lahf, na)
    | 0xa0 | 0xa1 ->
      let t = if b1 = 0xa0 then reg_8 else prefix.opsize in
      let (addr, na) = parse_disp_addr na in
      (Mov(t, o_rax, Oaddr(mbi addr), None), na)
    | 0xa2 | 0xa3 ->
      let t = if b1 = 0xa2 then r8 else prefix.opsize in
      let (addr, na) = parse_disp_addr na in
      (Mov(t, Oaddr(mbi addr), o_rax, None), na)
    | 0xa4 -> (Movs r8, na)
    | 0xa5 -> (Movs prefix.opsize, na)
    | 0xa6 -> (Cmps r8, na)
    | 0xa7 -> (Cmps prefix.opsize, na)
    | 0xac -> (Lods r8, na)
    | 0xad -> (Lods prefix.opsize, na)
    | 0xae -> (Scas r8, na)
    | 0xaf -> (Scas prefix.opsize, na)
    | 0xa8 -> let (i, na) = parse_imm8 na in
              (Test(r8, o_rax, i), na)
    | 0xa9 -> let it = if prefix.opsize = r64 then r32 else prefix.opsize in
              let (i,na) = parse_immz it na in
              (Test(prefix.opsize, o_rax, sign_ext it i prefix.opsize), na)
    | 0xaa -> (Stos r8, na)
    | 0xab -> (Stos prefix.opsize, na)
    | 0xb0 | 0xb1 | 0xb2 | 0xb3 | 0xb4 | 0xb5 | 0xb6
    | 0xb7 -> let (i, na) = parse_imm8 na in
              (Mov(r8, Oreg(rm_extend lor (b1 & 7)), i, None), na)
    | 0xb8 | 0xb9 | 0xba | 0xbb | 0xbc | 0xbd | 0xbe
    | 0xbf -> let (i, na) = parse_immv prefix.opsize na in
              (Mov(prefix.opsize, Oreg(rm_extend lor (b1 & 7)), i, None), na)
    | 0xc2 | 0xc3 (* Near ret *)
    | 0xca | 0xcb (* Far ret *) ->
      let far_ret = if (b1 = 0xc2 || b1 = 0xc3) then false else true in
      if (b1 = 0xc3 || b1 = 0xcb) then (Retn(None, far_ret), na)
      else let (imm,na) = parse_immw na in
           (Retn(Some(mt, imm), far_ret), na)
    | 0xc6
    | 0xc7 -> let t = if b1 = 0xc6 then r8 else prefix.opsize in
              let it = match b1 with
                | 0xc6 -> r8
                | 0xc7 when prefix.opsize_override -> r16
                | 0xc7 -> r32
                | _ -> failwith "impossible"
              in
              let (e, rm, na) = parse_modrmext_addr (Some it) na in
              let (i,na) = parse_immz it na in
              (match e with (* Grp 11 *)
              | 0 -> (Mov(t, rm, sign_ext it i t, None), na)
              | _ -> disfailwith (Printf.sprintf "Invalid opcode: %02x/%d" b1 e)
              )
    | 0xc9 -> (Leave (type_of_mode mode), na)
    | 0xcc -> (Interrupt3, na)
    | 0xcd -> let (i,na) = parse_imm8 na in
              (Interrupt(i), na)

    (* 0xd8-0xdf can be followed by a secondary opcode, OR a modrm
       byte. But the secondary opcode is only used when the modrm
       byte does not specify a memory address. *)
    | 0xd8 | 0xd9 | 0xda | 0xdb | 0xdc | 0xdd | 0xde | 0xdf ->
      let b2, _ = parse_int8 na in
      let (r, rm, na) = parse_modrmext_addr None na in
      (match r, rm with
      | 2, Oaddr _ ->
        (match b1 with
        | 0xd9 | 0xdd -> (Fst(rm, false), na)
        | _ ->
          unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
        )
      | 3, Oaddr _ ->
        (match b1 with
        | 0xd9 | 0xdd -> (Fst(rm, true), na)
        | _ ->
          unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
        )
      | 5, Oaddr _ ->
        (match b1 with
        | 0xd9 -> (Fldcw rm, na)
        | 0xdb -> (Fld rm, na)
        | _ ->
          unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
        )
      | 7, Oaddr _ ->
        (match b1 with
        | 0xd9 -> (Fnstcw rm, na)
        | 0xdb -> (Fst(rm, true), na)
        | _ ->
          unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
        )
      | _, Oaddr _ ->
        unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
      | _, _ ->
        unimplemented (Printf.sprintf "unsupported opcode: %02x %s" b1 (Util.big_int_to_hex ~pad:2 b2))
      )

    | 0xe8 -> let t = expanded_jump_type prefix.opsize in
              let (i,na) = parse_disp t na in
              (Call(Oimm(i +% na), na), na)
    | 0xe9 -> let t = expanded_jump_type prefix.opsize in
              let (i,na) = parse_disp t na in
              (Jump(Jabs(Oimm(i +% na))), na)
    | 0xeb -> let (i,na) = parse_disp8 na in
              (Jump(Jabs(Oimm(i +% na))), na)
    | 0xc0 | 0xc1
    | 0xd0 | 0xd1 | 0xd2
    | 0xd3 -> let immoff = if (b1 & 0xfe) = 0xc0 then Some r8 else None in
              let (r, rm, na) = parse_modrmext_addr immoff na in
              let opsize = if (b1 & 1) = 0 then r8 else prefix.opsize in
              let (amt, na) = match b1 & 0xfe with
                | 0xc0 -> parse_imm8 na
                | 0xd0 -> (Oimm bi1, na)
                | 0xd2 -> (o_rcx, na)
                | _ ->
                  disfailwith (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
              in
              (match r with (* Grp 2 *)
              | 0 -> (Rotate(LSHIFT, opsize, rm, amt, false),na)
              | 1 -> (Rotate(RSHIFT, opsize, rm, amt, false),na)
                (* SWXXX Implement these *)
              | 2 -> unimplemented
                (* (Rotate(LSHIFT, opsize, rm, amt, true),na) *)
                (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
              | 3 -> unimplemented
                (* (Rotate(RSHIFT, opsize, rm, amt, true),na) *)
                (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
              | 4 -> (Shift(LSHIFT, opsize, rm, amt), na)
              | 5 -> (Shift(RSHIFT, opsize, rm, amt), na)
              | 7 -> (Shift(ARSHIFT, opsize, rm, amt), na)
              | _ -> disfailwith
                (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
              )
    | 0xe3 ->
      let rcx_e = ge mode rcx in
      let (i,na) = parse_disp8 na in
      (Jcc(Jrel(na, i), rcx_e ==* mi 0), na)
    | 0xf4 -> (Hlt, na)
    | 0xf6
    | 0xf7 -> let t = if b1 = 0xf6 then r8 else prefix.opsize in
              let it = if t = r64 then r32 else t in
              let (r, rm, na) = parse_modrmext_addr (Some it) na in
              (match r with (* Grp 3 *)
               | 0 ->
                 let (imm, na) = parse_immz it na in
                 (Test(t, rm, sign_ext it imm t), na)
               | 2 -> (Not(t, rm), na)
               | 3 -> (Neg(t, rm), na)
               | 4 ->
                 (match b1 with
                 | 0xf6 -> (Mul(t, rm), na)
                 | 0xf7 -> (Mul(t, rm), na)
                 | _ -> disfailwith
                   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
                 )
               | 5 ->
                 (match b1 with
                 | 0xf6 -> (Imul(t, (true,o_rax), o_rax, rm), na)
                 | 0xf7 -> (Imul(t, (true,o_rdx), o_rax, rm), na)
                 | _ -> disfailwith
                   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
                 )
               | 6 ->
                 (match b1 with
                 | 0xf6 -> (Div(r8, rm) , na)
                 | 0xf7 -> (Div(t, rm), na)
                 | _ -> disfailwith
                   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
                 )
               | 7 ->
                 (match b1 with
                 | 0xf6 -> (Idiv(r8, rm) , na)
                 | 0xf7 -> (Idiv(t, rm), na)
                 | _ -> disfailwith
                   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
                 )
               | _ ->
                 disfailwith (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
              )
    | 0xfc -> (Cld, na)
    | 0xfe -> let (r, rm, na) = parse_modrmext_addr None na in
              (match r with (* Grp 4 *)
                | 0 -> (Inc(r8, rm), na)
                | 1 -> (Dec(r8, rm), na)
                | _ -> disfailwith
                  (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
              )
    | 0xff -> let (r, rm, na) = parse_modrmext_addr None na in
              (match r with (* Grp 5 *)
                | 0 -> (Inc(prefix.opsize, rm), na)
                | 1 -> (Dec(prefix.opsize, rm), na)
                | 2 -> (Call(rm, na), na)
                | 3 -> unimplemented (* callf *)
                  (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
                | 4 -> (Jump (Jabs rm), na)
                | 5 -> unimplemented (* jmpf *)
                  (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
                | 6 -> let size = match mode with
                         | X86 -> prefix.opsize
                         | X8664 -> r64
                       in
                       (Push(size, rm), na)
                | _ -> disfailwith
                  (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
              )
    (*** 00 to 3e ***)
    | b1 when b1 < 0x3e && (b1 & 7) < 6 ->
      (
        let ins a = match b1 >> 3 with
          | 0 -> Add a
          | 1 -> Or a
          | 2 -> Adc a
          | 3 -> Sbb a
          | 4 -> And a
          | 5 -> Sub a
          | 6 -> Xor a
          | 7 -> Cmp a
          | _ -> disfailwith (Printf.sprintf "impossible opcode: %02x" b1)
        in
        let t = if (b1 & 1) = 0  then r8 else prefix.opsize in
        (* handle sign extended immediate cases *)
        let it = if t = r64 then r32 else t in
        let (o1, o2, na) = match b1 & 7 with
          | 0 | 1 -> let r, rm, na = parse_modrm_addr None na in
                     (rm, r, na)
          | 2 | 3 -> let r, rm, na = parse_modrm_addr None na in
                     (r, rm, na)
          | 4 -> let i, na = parse_immb na in
                 (o_rax, i, na)
          | 5 -> let i, na = parse_immz it na in
                 (o_rax, sign_ext it i t, na)
          | _ -> disfailwith (Printf.sprintf "impossible opcode: %02x" b1)
        in
        (ins(t, o1, o2), na)
      )
    (* Two byte opcodes *)
    | 0x0f -> (
      (* Add in the second implied vex prefix *)
      let b2, na = match vex with
        | Some {vex_map_select=2} -> 0x38, na
        | Some {vex_map_select=3} -> 0x3a, na
        | Some {vex_map_select=1} | None -> Char.code (g na), s na
        | Some {vex_map_select} -> disfailwith (Printf.sprintf "reserved mmmmmm vex value: %d" vex_map_select)
      in
      match b2 with (* Table A-3 *)
      | 0x01 ->
        let b3, nna = Char.code (g na), (s na) in
        (match b3 with
          | 0xd0 -> (Xgetbv, nna)
          | _ -> disfailwith (Printf.sprintf "unsupported opcode %02x %02x %02x" b1 b2 b3))
      | 0x05 when mode = X8664 -> (Syscall, na)
      | 0x1f ->
        (* Even though we don't use the operand to nop, we need to
           parse it to get the next address *)
        let _, _, na = parse_modrm_addr None na in
        (Nop, na)
      | 0x10 | 0x11 when (prefix.repeat || prefix.nrepeat) -> (* MOVSS, MOVSD *)
        let r, rm, rv, na = parse_modrm_vec None na in
        let t = if prefix.repeat then r32 else r64 in
        let d, s, td = if b2 = 0x10 then r, rm, r128 else rm, r, t in
        (match rm, rv with
          | Ovec _, Some rv ->
            let nt = Typecheck.bits_of_width t in
            (Movoffset((r128, d),
                       {offlen=Reg (128 - nt); offtyp=r128; offop=rv; offsrcoffset=nt; offdstoffset=nt}
                       :: {offlen=t; offtyp=r128; offop=s; offsrcoffset=0; offdstoffset=0} :: []), na)
          | Ovec _, None ->
            (Movdq(t, s, t, d, false), na)
          | Oaddr _, _ ->
            (Movdq(t, s, td, d, false), na)
          | _ -> disfailwith "impossible")
      | 0x12 | 0x13 | 0x16 | 0x17 -> (* MOVLPS, MOVLPD, MOVHPS, MOVHPD, MOVHLPS, MOVHLPD, MOVLHPS, MOVLHPD *)
        let r, rm, rv, na = parse_modrm_vec None na in
        let tdst, dst, telt, tsrc1, src1, off_src1, off_dst1, src2 =
          match b2 with
          | (0x12 | 0x16) when rv <> None ->
            let offs1, offs2, offd1, offd2 = match b2, rm with
              | 0x12, Ovec _ -> 64, 64, 64, 0
              | 0x12, _ -> 64, 0, 64, 0
              | 0x16, _ -> 0, 0, 0, 64
              | _ -> disfailwith "impossible"
            in
            let rv = match rv with
              | Some r -> r
              | None -> disfailwith "impossible"
            in
            let src2 = [{offlen=r64; offtyp=r128; offop=rm; offsrcoffset=offs2; offdstoffset=offd2}] in
            r128, r, r64, r128, rv, offs1, offd1, src2
          | 0x12 | 0x13 | 0x16 | 0x17 ->
            let offset = match b2 with
              | 0x12 | 0x13 -> 0
              | 0x16 | 0x17 -> 64
              | _ -> disfailwith "impossible"
            in
            let s, d, offs, offd = match b2, rm with
              | 0x12, Ovec _ -> rm, r, 64, 0
              | (0x12 | 0x16), _ -> rm, r, 0, offset
              | (0x13 | 0x17), _ -> r, rm, offset, 0
              | _ -> disfailwith "impossible"
            in
            let ts = match s with Ovec _ -> r128 | _ -> r64 in
            r128, d, r64, ts, s, offs, offd, []
          | _ -> disfailwith "impossible"
        in
        (Movoffset((tdst, dst),
                   [{offlen=telt; offtyp=tsrc1; offop=src1; offsrcoffset=off_src1; offdstoffset=off_dst1}]@src2), na)
      | 0x10 | 0x11 | 0x28 | 0x29 | 0x6e | 0x7e | 0x6f | 0x7f | 0xd6 ->
      (* REGULAR MOVDQ *)
        let r, rm, _, na = parse_modrm_vec None na in
        let src, dst, tsrc, tdst, align = match b2 with
          | 0x10 | 0x11 | 0x28 | 0x29 -> (* MOVUPS, MOVUPD, MOVAPS, MOVAPD *)
            let s, d = match b2 with
              | 0x10 | 0x28 -> rm, r
              | 0x11 | 0x29 -> r, rm
              | _ -> disfailwith "impossible"
            in
            let align = match b2 with
              | 0x10 | 0x11 -> false
              | 0x28 | 0x29 -> true
              | _ -> disfailwith "impossible"
            in
            let t = if prefix.mopsize = r256 then r256 else r128 in
            s, d, t, t, align
          | 0x6e | 0x7e -> (* MOVD, MOVQ *)
            let t = if prefix.opsize = r64 then r64 else r32 in
            let s, d, ts, td = match b2 with
              | 0x6e -> toreg rm, r, t, r128
              | 0x7e when prefix.repeat -> rm, r, r64, r128
              | 0x7e -> r, toreg rm, t, t
              | _ -> disfailwith "impossible"
            in
            s, d, ts, td, false
          | 0x6f | 0x7f -> (* MOVQ, MOVDQA, MOVDQU *)
            let s, d = match b2 with
              | 0x6f -> rm, r
              | 0x7f -> r, rm
              | _ -> disfailwith "impossible"
            in
            let size = if prefix.repeat && prefix.vex = None then r128 else prefix.mopsize in
            let align = if prefix.opsize_override then true else false in
            s, d, size, size, align
          | 0xd6 -> (* MOVQ *)
            r, rm, r64, r64, false
          | _ -> unimplemented
            (Printf.sprintf "mov opcode case missing: %02x" b2)
        in
        (Movdq(tsrc, src, tdst, dst, align), na)
      | 0x31 -> (Rdtsc, na)
      | 0x34 -> (Sysenter, na)
      | 0x38 ->
        (* Three byte opcodes *)
        let b3 = Char.code (g na) and na = s na in
        (match b3 with
        | 0x00 ->
          let d, s, rv, na = parse_modrm_vec None na in
          (Pshufb(prefix.mopsize, d, s, rv), na)
        | 0x17 when prefix.opsize_override ->
          let d, s, _, na = parse_modrm_vec None na in
          (Ptest(prefix.mopsize, d, s), na)
        | 0x29 when prefix.opsize_override ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Pcmp(prefix.mopsize, reg_64, EQ, "pcmpeq", r, rm, rv), na)
        | 0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25
        | 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 when prefix.opsize_override ->
          (* pmovsx and pmovzx *)
          let r, rm, _, na = parse_modrm_vec None na in
          (* determine sign/zero extension *)
          let ext, name = match (b3 & 0xf0) with
            | 0x20 -> CAST_SIGNED, "pmovsx"
            | 0x30 -> CAST_UNSIGNED, "pmovzx"
            | _ -> disfailwith "impossible"
          in
          (* determine dest/src element size *)
          let dstet, srcet, fullname = match (b3 & 0x0f) with
            | 0x00 -> r16, r8, name ^ "bw"
            | 0x01 -> r32, r8, name ^ "bd"
            | 0x02 -> r64, r8, name ^ "bq"
            | 0x03 -> r32, r16, name ^ "wd"
            | 0x04 -> r64, r16, name ^ "wq"
            | 0x05 -> r64, r32, name ^ "dq"
            | _ -> disfailwith "impossible"
          in
          (Pmov(prefix.mopsize, dstet, srcet, r, rm, ext, fullname), na)
        | 0x37 when prefix.opsize_override ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Pcmp(prefix.mopsize, reg_64, SLT, "pcmpgt", r, rm, rv), na)
        | 0x38 | 0x39 when prefix.opsize_override ->
          let r, rm, rv, na = parse_modrm_vec None na in
          let et = match b3 with
            | 0x38 -> reg_8 | 0x39 -> reg_32
            | _ -> disfailwith "invalid"
          in
          (Ppackedbinop(prefix.mopsize, et, Ast_convenience.min_symbolic ~signed:true, "pmins", r, rm, rv), na)
        | 0x3a | 0x3b when prefix.opsize_override ->
          let r, rm, rv, na = parse_modrm_vec None na in
          let et = match b3 with
            | 0x3a -> reg_16 | 0x3b -> reg_32
            | _ -> disfailwith "invalid"
          in
          (Ppackedbinop(prefix.mopsize, et, Ast_convenience.min_symbolic ~signed:false, "pminu", r, rm, rv), na)
        | _ -> disfailwith (Printf.sprintf "opcode unsupported: 0f 38 %02x" b3))
      | 0x3a ->
        let b3 = Char.code (g na) and na = s na in
        (match b3 with
        | 0x0f ->
          let r, rm, rv, na = parse_modrm_vec (Some r8) na in
          let i, na = parse_imm8 na in
          (Palignr(prefix.mopsize, r, rm, rv, i), na)
        | 0x60 | 0x61 | 0x62 | 0x63 ->
          let r, rm, _, na = parse_modrm_vec (Some r8) na in
          let i, na = parse_imm8 na in
          (match i with

          | Oimm imm ->
            let open Pcmpstr in
            let imm8cb = parse_imm8cb imm in
            let pcmp = {out=if b3 land 0x1 = 0x1 then Index else Mask;
                        len=if b3 land 0x2 = 0x2 then Implicit else Explicit} in
            (Pcmpstr(prefix.mopsize, r, rm, i, imm8cb, pcmp), na)
          | _ ->  unimplemented "unsupported non-imm op for pcmpistri")
        | b4 ->  unimplemented
          (Printf.sprintf "unsupported opcode %02x %02x %02x" b1 b2 b3)
        )
      (* conditional moves *)
      | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49
      | 0x4a | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f ->
        let (r, rm, na) = parse_modrm_addr None na in
        (Mov(prefix.opsize, r, rm, Some(cc_to_exp b2)), na)
      | 0x57 ->
        let r, rm, rv, na = parse_modrm_vec None na in
        let t = if prefix.mopsize = r256 then r256 else r128 in
        (Ppackedbinop(t, prefix.opsize, binop XOR, "xorp", r, rm, rv), na)
      | 0x60 | 0x61 | 0x62 | 0x68 | 0x69 | 0x6a ->
        let order = match b2 with
          | 0x60 | 0x61 | 0x62 -> Low
          | 0x68 | 0x69 | 0x70 -> High
          | _ -> disfailwith "impossible"
        in
        let elemt = match b2 with
          | 0x60 | 0x68 -> reg_8
          | 0x61 | 0x69 -> reg_16
          | 0x62 | 0x6a -> reg_32
          | _ -> disfailwith "impossible"
        in
        let r, rm, rv, na = parse_modrm_vec None na in
        (Punpck(prefix.mopsize, elemt, order, r, rm, rv), na)
      | 0x6c | 0x6d when prefix.opsize_override ->
        let order = match b2 with
          | 0x6c -> Low
          | 0x6d -> High
          | _ -> disfailwith "impossible"
        in
        let elemt = reg_64 in
        let r, rm, rv, na = parse_modrm_vec None na in
        (Punpck(prefix.mopsize, elemt, order, r, rm, rv), na)
      | 0x64 | 0x65 | 0x66 | 0x74 | 0x75 | 0x76  as o ->
        let r, rm, rv, na = parse_modrm_vec None na in
        let elet = match o & 0x6 with | 0x4 -> r8 | 0x5 -> r16 | 0x6 -> r32 | _ ->
          disfailwith "impossible" in
        let bop, bstr = match o & 0x70 with | 0x70 -> EQ, "pcmpeq" | 0x60 -> SLT, "pcmpgt"
          | _ -> disfailwith "impossible" in
        (Pcmp(prefix.mopsize, elet, bop, bstr, r, rm, rv), na)
      | 0x70 when prefix.opsize = r16 ->
        let r, rm, rv, na = parse_modrm_vec (Some r8) na in
        let i, na = parse_imm8 na in
        (Pshufd(prefix.mopsize, r, rm, rv, i), na)
      | 0x71 | 0x72 | 0x73 ->
        let t = prefix.mopsize in
        let r, rm, rv, na = parse_modrm_vec (Some r8) na in
        let i, na = parse_imm8 na in
        let open BatInt64.Infix in
            let fbop, str, et, i = match b2, r, i with
              | _, Ovec 2, _ -> binop RSHIFT, "psrl", lowbits2elemt b2, i
              | _, Ovec 6, _ -> binop LSHIFT, "psll", lowbits2elemt b2, i
              | _, Ovec 4, _ -> binop ARSHIFT, "psra", lowbits2elemt b2, i
          (* The shift amount of next two elements are multipled by eight *)
              | 0x73, Ovec 3, Oimm i when prefix.opsize_override -> binop RSHIFT, "psrldq", t, Oimm (i *% bi8)
              | 0x73, Ovec 7, Oimm i when prefix.opsize_override -> binop LSHIFT, "pslldq", t, Oimm (i *% bi8)
              | _, Oreg i, _ -> disfailwith (Printf.sprintf "invalid psrl/psll encoding b2=%#x r=%#x" b2 i)
              | _ -> disfailwith "impossible"
            in
            (Ppackedbinop(t, et, fbop, str, rm, i, rv), na)
      | 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 | 0x89
      | 0x8a | 0x8b | 0x8c | 0x8d | 0x8e | 0x8f ->
        let t = expanded_jump_type prefix.opsize in
        let (i,na) = parse_disp t na in
        (Jcc(Jabs(Oimm(i +% na)), cc_to_exp b2), na)
      (* add other opcodes for setcc here *)
      | 0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 | 0x99
      | 0x9a | 0x9b | 0x9c | 0x9d | 0x9e | 0x9f ->
        let r, rm, na = parse_modrm_addr None na in
        (* unclear what happens otherwise *)
        assert (prefix.opsize = r32);
        (Setcc(r8, rm, cc_to_exp b2), na)
      | 0xa2 -> (Cpuid, na)
      | 0xa3 | 0xba ->
        let it = if b2 = 0xba then Some r8 else None in
        let (r, rm, na) = parse_modrm_addr it na in
        let r, na = if b2 = 0xba then parse_imm8 na else r, na in
        (Bt(prefix.opsize, r, rm), na)
      | 0xa4 ->
        (* shld *)
        let (r, rm, na) = parse_modrm_addr (Some r8) na in
        let (i, na) = parse_imm8 na in
        (Shiftd(LSHIFT, prefix.opsize, rm, r, i), na)
      | 0xa5 ->
        (* shld *)
        let (r, rm, na) = parse_modrm_addr None na in
        (Shiftd(LSHIFT, prefix.opsize, rm, r, o_rcx), na)
      | 0xac ->
        (* shrd *)
        let (r, rm, na) = parse_modrm_addr (Some r8) na in
        let (i, na) = parse_imm8 na in
        (Shiftd(RSHIFT, prefix.opsize, rm, r, i), na)
      | 0xad ->
        (* shrd *)
        let (r, rm, na) = parse_modrm_addr None na in
        (Shiftd(RSHIFT, prefix.opsize, rm, r, o_rcx), na)
      | 0xae ->
        let (r, rm, na) = parse_modrmext_addr None na in
        (match r with
        | 2 -> (Ldmxcsr rm, na) (* ldmxcsr *)
        | 3 -> (Stmxcsr rm, na) (* stmxcsr *)
        | _ -> unimplemented
          (Printf.sprintf "unsupported opcode: %02x %02x/%d" b1 b2 r)
        )
      | 0xaf ->
        let (r, rm, na) = parse_modrm_addr None na in
        (Imul(prefix.opsize, (false,r), r, rm), na)
      | 0xb1 ->
        let r, rm, na = parse_modrm_addr None na in
        (Cmpxchg (prefix.opsize, r, rm), na)
      | 0xb6
      | 0xb7 -> let st = if b2 = 0xb6 then r8 else r16 in
                let r, rm, na = parse_modrm_addr None na in
                (Movzx(prefix.opsize, r, st, rm), na)
      | 0xb8 when prefix.repeat ->
        let r, rm, na = parse_modrm_addr None na in
        (Popcnt (prefix.opsize, rm, r), na)
      | 0xbc | 0xbd ->
        let dir = match b2 with | 0xbc -> Forward | 0xbd -> Backward | _ -> failwith "impossible" in
        let r, rm, na = parse_modrm_addr None na in
        (Bs (prefix.opsize, r, rm, dir), na)
      | 0xbe
      | 0xbf -> let st = if b2 = 0xbe then r8 else r16 in
                let r, rm, na = parse_modrm_addr None na in
                (Movsx(prefix.opsize, r, st, rm), na)
      | 0xc1 ->
        let r, rm, na = parse_modrm_addr None na in
        (Xadd(prefix.opsize, r, rm), na)
      | 0xc7 ->
        let r, rm, na = parse_modrmext_addr None na in
        (match r with
        | 1 -> (Cmpxchg8b(rm), na)
        | _ -> unimplemented
          (Printf.sprintf "unsupported opcode: %02x %02x/%d" b1 b2 r)
        )
      | 0xc8 | 0xc9 | 0xca | 0xcb | 0xcc | 0xcd | 0xce | 0xcf ->
        (Bswap(prefix.opsize, Oreg(rm_extend lor (b2 & 7))), na)
      | 0xd1 | 0xd2 | 0xd3 | 0xe1 | 0xe2 | 0xf1 | 0xf2 | 0xf3 ->
        let t = prefix.mopsize in
        let r, rm, rv, na = parse_modrm_vec None na in
        let et = lowbits2elemt b2 in
        let fbop, str = match b2 & 0xf0 with
          | 0xd0 -> binop RSHIFT, "psrl"
          | 0xe0 -> binop ARSHIFT, "psra"
          | 0xf0 -> binop LSHIFT, "psll"
          | _ -> disfailwith "invalid"
        in
        (Ppackedbinop(t, et, fbop, str, r, rm, rv), na)
      | 0xda ->
        let r, rm, rv, na = parse_modrm_vec None na in
        (Ppackedbinop(prefix.mopsize, reg_8, Ast_convenience.min_symbolic ~signed:false, "pminub", r, rm, rv), na)
      | 0xdb ->
        let r, rm, rv, na = parse_modrm_vec None na in
        (Pbinop(prefix.mopsize, binop AND, "pand", r, rm, rv), na)
      | 0xd7 ->
        let r, rm, na = parse_modrm_addr None na in
        let r, rm = r, tovec rm in
        (Pmovmskb(prefix.mopsize, r, rm), na)
      | 0xde ->
        let r, rm, rv, na = parse_modrm_vec None na in
        (Ppackedbinop(prefix.mopsize, reg_8, Ast_convenience.max_symbolic ~signed:false, "pmaxub", r, rm, rv), na)
      | 0xdf ->
        let r, rm, rv, na = parse_modrm_vec None na in
        let andn x y = binop AND (unop NOT x) y in
        (Pbinop(prefix.mopsize, andn, "pandn", r, rm, rv), na)
      | 0xe0 | 0xe3 ->
        (* pavg *)
        let r, rm, rv, na = parse_modrm_vec None na in
        (* determine whether we're using bytes or words *)
        let et = match b2 & 0x0f with
          | 0x00 -> r8
          | 0x03 -> r16
          | _ -> disfailwith "invalid"
        in
        let one = it 1 et in
        let average x y = ((x +* y) +* one) >>* one in
        (Ppackedbinop(prefix.mopsize, et, average, "pavg", r, rm, rv), na)
      | 0xea ->
        let r, rm, rv, na = parse_modrm_vec None na in
        (Ppackedbinop(prefix.mopsize, reg_16, Ast_convenience.min_symbolic ~signed:true, "pmins", r, rm, rv), na)
      | 0xeb ->
        let r, rm, rv, na = parse_modrm_vec None na in
        (Pbinop(prefix.mopsize, binop OR, "por", r, rm, rv), na)
      | 0xef ->
        let r, rm, rv, na = parse_modrm_vec None na in
        (Pbinop(prefix.mopsize, binop XOR, "pxor", r, rm, rv), na)
      | 0xf0 ->
        let r, rm, _, na = parse_modrm_vec None na in
        let t = if prefix.mopsize = r256 then r256 else r128 in
        (Movdq(t, rm, t, r, false), na)
      | 0xf8 | 0xf9 | 0xfa | 0xfb ->
        let r, rm, rv, na = parse_modrm_vec None na in
        let eltsize = match b2 & 7 with
          | 0 -> r8
          | 1 -> r16
          | 2 -> r32
          | 3 -> r64
          | _ -> disfailwith "impossible"
        in
        (Ppackedbinop(prefix.mopsize, eltsize, binop MINUS, "psub", r, rm, rv), na)
      | _ -> unimplemented
        (Printf.sprintf "unsupported opcode: %02x %02x" b1 b2)
    )
    | n -> unimplemented (Printf.sprintf "unsupported single opcode: %02x" n)

  in
  let rex, pref, a = get_prefixes addr in
  let vex, a = get_vex a in
  (* Append VEX implied mandatory prefixes *)
  let pref = match vex with
    | Some {vex_pp=1} -> 0x66 :: pref
    | Some {vex_pp=2} -> 0xf3 :: pref
    | Some {vex_pp=3} -> 0xf2 :: pref
    | _ -> pref
  in
  let rex = BatOption.map parse_rex rex in
  (* Opsize for regular instructions, MMX/SSE2 instructions

     The opsize override makes regular operands smaller, but MMX
     operands larger.  *)
  let modesize = type_of_mode mode in
  let opsize, bopsize, mopsize =
    if List.mem pref_opsize pref then r16,r16,r128 else r32,modesize,r64
  in
  let opsize = match rex with
    | Some {rex_w=true} -> r64 (* See Table 3-4: Effective Operand-
                                  and Address-Size Attributes in 64-Bit Mode *)
    | Some {rex_w=false} | None -> opsize
  in
  let opsize = match vex with
    | Some {vex_we=true} -> r64
    | _ -> opsize
  in
  let mopsize = match vex with
    | Some {vex_l=false} -> r128
    | Some {vex_l=true} -> r256
    | None -> mopsize
  in
  let addrsize = match mode with
    | X86 -> if List.mem pref_addrsize pref then r16 else r32
    | X8664 -> if List.mem pref_addrsize pref then r32 else r64
  in
  let r_extend, rm_extend, sib_extend =
    let e b = (if b then 1 else 0) << 3 in (* tuareg >> *)
    match rex with
    | Some {rex_r; rex_b; rex_x} ->
      e rex_r, e rex_b, e rex_x
    | None ->
      (match vex with
      | Some {vex_nr; vex_nb; vex_nx;} -> e (not vex_nr), e (not vex_nb), e (not vex_nx)
      | None -> 0, 0, 0)
  in

  let prefix =
    {
      addrsize;
      opsize;
      bopsize;
      mopsize;
      repeat = List.mem repz pref;
      nrepeat = List.mem repnz pref;
      addrsize_override = List.mem pref_addrsize pref;
      opsize_override = List.mem pref_opsize pref;
      rex;
      vex;
      r_extend;
      rm_extend;
      sib_extend;
    }
  in
  let op, a = get_opcode pref prefix a in
  (pref, prefix, op, a)

let parse_prefixes mode pref op =
  (* FIXME: how to deal with conflicting prefixes? *)
  let rec f s r = function
    | [] -> (BatOption.map (gv mode) s, List.rev r)
    | 0x2e::p -> f seg_cs r p
    | 0x36::p -> f seg_ss r p
    | 0x3e::p -> f seg_ds r p
    | 0x26::p -> f seg_es r p
    | 0x64::p -> f seg_fs r p
    | 0x65::p -> f seg_gs r p
    | 0xf0::p -> f s r p (* discard lock prefix *)
    | 0x66::p -> f s r p
    | p::ps -> f s (p::r) ps
  in
  f None [] pref

let disasm_instr mode g addr =
  let (pref, prefix, op, na) = parse_instr mode g addr in
  let has_rex = prefix.rex <> None in
  let has_vex = prefix.vex <> None in
  let (ss, pref) =  parse_prefixes mode pref op in
  let ir = ToIR.to_ir mode addr na ss pref has_rex has_vex op in
  (ToIR.add_labels addr ir, na)
