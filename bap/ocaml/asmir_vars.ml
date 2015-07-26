(** Asmir variables *)

open Type

let x86_regs = Disasm_i386.regs_x86
let x64_regs = Disasm_i386.regs_x86_64
let full_regs = Disasm_i386.regs_full

let x86_mem = Disasm_i386.R32.mem
let x64_mem = Disasm_i386.R64.mem

module X86 = struct
  module R32 = Disasm_i386.R32
  module R64 = Disasm_i386.R64
end

let mem_of_type = function
  | Type.Reg 32 -> x86_mem
  | Type.Reg 64 -> x64_mem
  | t -> failwith (Printf.sprintf "Unable to find memory for address type %s" (Pp.typ_to_string t))

let arm_regs =
  List.map (fun n -> Var.newvar n (Reg 32))
    [ "R0";
      "R1";
      "R2";
      "R3";
      "R4";
      "R5";
      "R6";
      "R7";
      "R8";
      "R9";
      "R10";
      "R11";
      "R12";
      "R13";
      "R14";
      "R15T";
      "CC";
      "CC_OP";
      "CC_DEP1";
      "CC_DEP2";
      "CC_NDEP";
    ]

let subregs =
  [
    (* x86 subregisters *)
    ("R_AL_32", ("R_EAX_32", 0, Reg 32));
    ("R_BL_32", ("R_EBX_32", 0, Reg 32));
    ("R_CL_32", ("R_ECX_32", 0, Reg 32));
    ("R_DL_32", ("R_EDX_32", 0, Reg 32));

    ("R_AH_32", ("R_EAX_32", 8, Reg 32));
    ("R_BH_32", ("R_EBX_32", 8, Reg 32));
    ("R_CH_32", ("R_ECX_32", 8, Reg 32));
    ("R_DH_32", ("R_EDX_32", 8, Reg 32));

    ("R_DI_32", ("R_EDI_32", 0, Reg 32));
    ("R_SI_32", ("R_ESI_32", 0, Reg 32));
    ("R_BP_32", ("R_EBP_32", 0, Reg 32));
    ("R_SP_32", ("R_ESP_32", 0, Reg 32));
    ("R_AX_32", ("R_EAX_32", 0, Reg 32));

    ("R_BX_32", ("R_EBX_32", 0, Reg 32));
    ("R_CX_32", ("R_ECX_32", 0, Reg 32));
    ("R_DX_32", ("R_EDX_32", 0, Reg 32));
    ("R_BP_32", ("R_EBP_32", 0, Reg 32));
    ("R_SI_32", ("R_ESI_32", 0, Reg 32));
    ("R_DI_32", ("R_EDI_32", 0, Reg 32));
    ("R_SP_32", ("R_ESP_32", 0, Reg 32));

    (* x64 subregisters *)
    ("R_AL_64", ("R_RAX", 0, Reg 64));
    ("R_BL_64", ("R_RBX", 0, Reg 64));
    ("R_CL_64", ("R_RCX", 0, Reg 64));
    ("R_DL_64", ("R_RDX", 0, Reg 64));

    ("R_AH_64", ("R_RAX", 8, Reg 64));
    ("R_BH_64", ("R_RBX", 8, Reg 64));
    ("R_CH_64", ("R_RCX", 8, Reg 64));
    ("R_DH_64", ("R_RDX", 8, Reg 64));

    ("R_DIL", ("R_RDI", 0, Reg 64));
    ("R_SIL", ("R_RSI", 0, Reg 64));
    ("R_BPL", ("R_RBP", 0, Reg 64));
    ("R_SPL", ("R_RSP", 0, Reg 64));

    ("R_DI_64", ("R_RDI", 0, Reg 64));
    ("R_SI_64", ("R_RSI", 0, Reg 64));
    ("R_BP_64", ("R_RBP", 0, Reg 64));
    ("R_SP_64", ("R_RSP", 0, Reg 64));

    ("R_AX_64", ("R_RAX", 0, Reg 64));
    ("R_BX_64", ("R_RBX", 0, Reg 64));
    ("R_CX_64", ("R_RCX", 0, Reg 64));
    ("R_DX_64", ("R_RDX", 0, Reg 64));
    ("R_BP_64", ("R_RBP", 0, Reg 64));
    ("R_SI_64", ("R_RSI", 0, Reg 64));
    ("R_DI_64", ("R_RDI", 0, Reg 64));
    ("R_SP_64", ("R_RSP", 0, Reg 64));

    ("R_EAX_64", ("R_RAX", 0, Reg 64));
    ("R_EBX_64", ("R_RBX", 0, Reg 64));
    ("R_ECX_64", ("R_RCX", 0, Reg 64));
    ("R_EDX_64", ("R_RDX", 0, Reg 64));
    ("R_EBP_64", ("R_RBP", 0, Reg 64));
    ("R_ESI_64", ("R_RSI", 0, Reg 64));
    ("R_EDI_64", ("R_RDI", 0, Reg 64));
    ("R_ESP_64", ("R_RSP", 0, Reg 64));

  ] @ Array.to_list (Array.init 16 (fun i -> (Printf.sprintf "R_XMM%d" i, (Printf.sprintf "R_YMM%d" i, 0, Reg 256))))
    @ Array.to_list (Array.init 16 (fun i -> (Printf.sprintf "R_MM%d" i, (Printf.sprintf "R_YMM%d" i, 0, Reg 256))))
    @ Array.to_list (Array.init 8 (fun i -> (Printf.sprintf "R_R%dL" (i+8), (Printf.sprintf "R_R%d" (i+8), 0, Reg 64))))
    @ Array.to_list (Array.init 8 (fun i -> (Printf.sprintf "R_R%dW" (i+8), (Printf.sprintf "R_R%d" (i+8), 0, Reg 64))))
    @ Array.to_list (Array.init 8 (fun i -> (Printf.sprintf "R_R%dD" (i+8), (Printf.sprintf "R_R%d" (i+8), 0, Reg 64))))

let x86_all_regs = x86_mem :: x86_regs @ arm_regs
let x64_all_regs = x64_mem :: x64_regs
let multiarch_all_regs = x64_mem :: x86_mem :: full_regs
