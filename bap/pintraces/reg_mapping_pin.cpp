#include "reg_mapping_pin.h"

string pin_register_name(uint32_t id)
{

switch (id) {
    //case REG_INVALID_ :
    //case REG_NONE :
    //case REG_FIRST:

    // immediate operand
    //case REG_IMM8 :
    //case REG_IMM_BASE:
    //case REG_IMM:
    //case REG_IMM32:
    //case REG_IMM_LAST:

    // memory operand
    //case REG_MEM:
    //case REG_MEM_BASE:
    //case REG_MEM_OFF8:
    //case REG_MEM_OFF32:
    //case REG_MEM_LAST:

    // memory-address offset operand
    //case REG_OFF8:
    //case REG_OFF_BASE:
    //case REG_OFF:
    //case REG_OFF32:
    //case REG_OFF_LAST:

    //case REG_MODX:

    // base for all kinds of registers (application: machine: pin)
    //case REG_RBASE:

    // Machine registers are individual real registers on the machine
    //case REG_MACHINE_BASE:

    // Application registers are registers used in the application binary
    // Application registers include all machine registers. In addition:
    // they include some aggregrate registers that can be accessed by
    // the application in a single instruction
    // Essentially: application registers = individual machine registers + aggregrate registers

    //case REG_APPLICATION_BASE:

    /* !@ todo: should save scratch mmx and fp registers */
    // The machine registers that form a context. These are the registers
    // that need to be saved in a context switch.
    //case REG_PHYSICAL_CONTEXT_BEGIN:

    //case REG_GR_BASE:
#if defined(TARGET_IA32E)
    // Context registers in the Intel(R) 64 architecture
    // We don't need to have cases for G* registers, since they're defined
    // as equal to their equivalents (R* for 64 bit, E* for 32 bit) in the enum.
    case REG_RDI: return string("R_RDI");
    case REG_RSI: return string("R_RSI");
    case REG_RBP: return string("R_RBP");
    case REG_RSP: return string("R_RSP");
    case REG_RBX: return string("R_RBX");
    case REG_RDX: return string("R_RDX");
    case REG_RCX: return string("R_RCX");
    case REG_RAX: return string("R_RAX");
    case REG_R8: return string("R_R8");
    case REG_R9: return string("R_R9");
    case REG_R10: return string("R_R10");
    case REG_R11: return string("R_R11");
    case REG_R12: return string("R_R12");
    case REG_R13: return string("R_R13");
    case REG_R14: return string("R_R14");
    case REG_R15: return string("R_R15");

    case REG_RFLAGS: return string("R_RFLAGS");
    case REG_RIP: return string("R_RIP");
#else
    // Context registers in the IA-32 architecture
    // Given _32 suffix to differentiate from subregs in 64-bit mode.
    case REG_EDI:  return string("R_EDI_32");
    //case REG_GDI:
    //case REG_EDI:
    case REG_ESI:  return string("R_ESI_32");
    //case REG_GSI:
    //case REG_ESI:
    case REG_EBP:  return string("R_EBP_32");
    //case REG_GBP:
    //case REG_EBP:
    case REG_ESP:  return string("R_ESP_32");
    //case REG_STACK_PTR:
    //case REG_ESP:
    case REG_EBX:  return string("R_EBX_32");
    //case REG_GBX:
    //case REG_EBX:
    case REG_EDX:  return string("R_EDX_32");
    //case REG_GDX:
    //case REG_EDX:
    case REG_ECX:  return string("R_ECX_32");
    //case REG_GCX:
    //case REG_ECX:
    case REG_EAX: return string("R_EAX_32");
    //case REG_GAX:
    //case REG_EAX:
    //case REG_GR_LAST:
    //case REG_EAX:

    case REG_EFLAGS:  return string("R_EFLAGS");
    //case REG_GFLAGS:
    //case REG_EFLAGS:

    /*
     * These two are equal in the header so there's
     * a case conflict when both are defined.
     */
    case REG_EIP: return string("R_EIP");
    // case REG_INST_PTR:  return string("R_EIP");
#endif

    //case REG_PHYSICAL_CONTEXT_END:
    //case REG_INST_PTR:

    //case REG_SP:
    //case REG_FLAGS:
    //case REG_IP:

#if defined(TARGET_IA32E)
    // partial registers common to both the IA-32 and Intel(R) 64 architectures.
    // Here given special names so we can differentiate between subregisters
    // of 32-bit and 64-bit registers.
    case REG_AL:  return string("R_AL_64");
    case REG_AH:  return string("R_AH_64");
    case REG_AX:  return string("R_AX_64");

    case REG_CL:  return string("R_CL_64");
    case REG_CH:  return string("R_CH_64");
    case REG_CX:  return string("R_CX_64");

    case REG_DL:  return string("R_DL_64");
    case REG_DH:  return string("R_DH_64");
    case REG_DX:  return string("R_DX_64");

    case REG_BL:  return string("R_BL_64");
    case REG_BH:  return string("R_BH_64");
    case REG_BX:  return string("R_BX_64");

    case REG_BP:  return string("R_BP_64");
    case REG_SI:  return string("R_SI_64");
    case REG_DI:  return string("R_DI_64");

    // partial registers in the Intel(R) 64 architecture
    case REG_EDI: return string("R_EDI_64");
    case REG_DIL: return string("R_DIL");
    case REG_ESI: return string("R_ESI_64");
    case REG_SIL: return string("R_SIL");
    case REG_EBP: return string("R_EBP_64");
    case REG_BPL: return string("R_BPL");
    case REG_ESP: return string("R_ESP_64");
    case REG_SPL: return string("R_SPL");
    case REG_EBX: return string("R_EBX_64");
    case REG_EDX: return string("R_EDX_64");
    case REG_ECX: return string("R_ECX_64");
    case REG_EAX: return string("R_EAX_64");

    case REG_EFLAGS: return string("R_EFLAGS");
    case REG_EIP: return string("R_EIP");

    case REG_R8B: return string("R_R8B");
    case REG_R8W: return string("R_R8W");
    case REG_R8D: return string("R_R8D");
    case REG_R9B: return string("R_R9B");
    case REG_R9W: return string("R_R9W");
    case REG_R9D: return string("R_R9D");
    case REG_R10B: return string("R_R10B");
    case REG_R10W: return string("R_R10W");
    case REG_R10D: return string("R_R10D");
    case REG_R11B: return string("R_R11B");
    case REG_R11W: return string("R_R11W");
    case REG_R11D: return string("R_R11D");
    case REG_R12B: return string("R_R12B");
    case REG_R12W: return string("R_R12W");
    case REG_R12D: return string("R_R12D");
    case REG_R13B: return string("R_R13B");
    case REG_R13W: return string("R_R13W");
    case REG_R13D: return string("R_R13D");
    case REG_R14B: return string("R_R14B");
    case REG_R14W: return string("R_R14W");
    case REG_R14D: return string("R_R14D");
    case REG_R15B: return string("R_R15B");
    case REG_R15W: return string("R_R15W");
    case REG_R15D: return string("R_R15D");
#else
    // partial registers common to both the IA-32 and Intel(R) 64 architectures.
    // Given 32-bit suffix to differentiate from 64-bit subregisters.
    case REG_AL:  return string("R_AL_32");
    case REG_AH:  return string("R_AH_32");
    case REG_AX:  return string("R_AX_32");

    case REG_CL:  return string("R_CL_32");
    case REG_CH:  return string("R_CH_32");
    case REG_CX:  return string("R_CX_32");

    case REG_DL:  return string("R_DL_32");
    case REG_DH:  return string("R_DH_32");
    case REG_DX:  return string("R_DX_32");

    case REG_BL:  return string("R_BL_32");
    case REG_BH:  return string("R_BH_32");
    case REG_BX:  return string("R_BX_32");

    case REG_BP:  return string("R_BP_32");
    case REG_SI:  return string("R_SI_32");
    case REG_DI:  return string("R_DI_32");
#endif

/*
    case REG_MM_BASE:
    case REG_MM0 = case REG_MM_BASE:
    case REG_MM1:
    case REG_MM2:
    case REG_MM3:
    case REG_MM4:
    case REG_MM5:
    case REG_MM6:
    case REG_MM7:
    case REG_MM_LAST = case REG_MM7:

    case REG_EMM_BASE:
    case REG_EMM0 = case REG_EMM_BASE:
    case REG_EMM1:
    case REG_EMM2:
    case REG_EMM3:
    case REG_EMM4:
    case REG_EMM5:
    case REG_EMM6:
    case REG_EMM7:
    case REG_EMM_LAST = case REG_EMM7:

    case REG_MXT:
*/
    case REG_XMM0:
      return string("R_XMM0");
    case REG_XMM1:
      return string("R_XMM1");
    case REG_XMM2:
      return string("R_XMM2");
    case REG_XMM3:
      return string("R_XMM3");
    case REG_XMM4:
      return string("R_XMM4");
    case REG_XMM5:
      return string("R_XMM5");
    case REG_XMM6:
      return string("R_XMM6");
    case REG_XMM7:
      return string("R_XMM7");

#if defined(TARGET_IA32E)
    // additional xmm registers in the Intel(R) 64 architecture
    case REG_XMM8:
      return string("R_XMM8");
    case REG_XMM9:
      return string("R_XMM9");
    case REG_XMM10:
      return string("R_XMM10");
    case REG_XMM11:
      return string("R_XMM11");
    case REG_XMM12:
      return string("R_XMM12");
    case REG_XMM13:
      return string("R_XMM13");
    case REG_XMM14:
      return string("R_XMM14");
    case REG_XMM15:
      return string("R_XMM15");
#endif

    case REG_YMM0:
      return string("R_YMM0");
    case REG_YMM1:
      return string("R_YMM1");
    case REG_YMM2:
      return string("R_YMM2");
    case REG_YMM3:
      return string("R_YMM3");
    case REG_YMM4:
      return string("R_YMM4");
    case REG_YMM5:
      return string("R_YMM5");
    case REG_YMM6:
      return string("R_YMM6");
    case REG_YMM7:
      return string("R_YMM7");


#if defined(TARGET_IA32E)
    // additional ymm registers in the Intel(R) 64 architecture
    case REG_YMM8:
      return string("R_YMM8");
    case REG_YMM9:
      return string("R_YMM9");
    case REG_YMM10:
      return string("R_YMM10");
    case REG_YMM11:
      return string("R_YMM11");
    case REG_YMM12:
      return string("R_YMM12");
    case REG_YMM13:
      return string("R_YMM13");
    case REG_YMM14:
      return string("R_YMM14");
    case REG_YMM15:
      return string("R_YMM15");

#endif

    case REG_SEG_CS: return string("R_CS");
    case REG_SEG_SS: return string("R_SS");
    case REG_SEG_DS: return string("R_DS");
    case REG_SEG_ES: return string("R_ES");
    case REG_SEG_FS: return string("R_FS");
    case REG_SEG_GS: return string("R_GS");

    case REG_MXCSR: return string("R_MXCSR");
        /*
    case REG_DR_BASE:
    case REG_DR0 = case REG_DR_BASE:
    case REG_DR1:
    case REG_DR2:
    case REG_DR3:
    case REG_DR4:
    case REG_DR5:
    case REG_DR6:
    case REG_DR7:
    case REG_DR_LAST = case REG_DR7:

    case REG_CR_BASE:
    case REG_CR0 = case REG_CR_BASE:
    case REG_CR1:
    case REG_CR2:
    case REG_CR3:
    case REG_CR4:
    case REG_CR_LAST = case REG_CR4:

    case REG_TSSR:

    case REG_LDTR:
*/
/*
 --- Not clear if following are needed
    case REG_ESR_BASE:
    case REG_ESR_LIMIT:

    case REG_CSR_BASE:
    case REG_CSR_LIMIT:

    case REG_SSR_BASE:
    case REG_SSR_LIMIT:

    case REG_DSR_BASE:
    case REG_DSR_LIMIT:

    case REG_FSR_BASE:
    case REG_FSR_LIMIT:

    case REG_GSR_BASE:
    case REG_GSR_LIMIT:

    case REG_TSSR_BASE:
    case REG_TSSR_LIMIT:

    case REG_LDTR_BASE:
    case REG_LDTR_LIMIT:

    case REG_GDTR_BASE:
    case REG_GDTR_LIMIT:

    case REG_IDTR_BASE:
    case REG_IDTR_LIMIT:
*/
/*
    case REG_TR_BASE:
    case REG_TR = case REG_TR_BASE:
    case REG_TR3:
    case REG_TR4:
    case REG_TR5:
    case REG_TR6:
    case REG_TR7:
    case REG_TR_LAST = case REG_TR7:

    case REG_FPST_BASE:
    case REG_FP_BASE = case REG_FPST_BASE:
    case REG_FPCW = case REG_FP_BASE:
    case REG_FPSW:
    case REG_FPTAG:
    case REG_FPIP_OFF:
    case REG_FPIP_SEL:
    case REG_FPOPCODE:
    case REG_FPDP_OFF:
    case REG_FPDP_SEL:
    case REG_FP_LAST = case REG_FPDP_SEL:

    case REG_ST_BASE:
    */
 case REG_ST0: return string("R_ST0");
 case REG_ST1: return string("R_ST1");
 case REG_ST2: return string("R_ST2");
 case REG_ST3: return string("R_ST3");
 case REG_ST4: return string("R_ST4");
 case REG_ST5: return string("R_ST5");
 case REG_ST6: return string("R_ST6");
 case REG_ST7: return string("R_ST7");
        /*
#if !defined(TARGET_DOXYGEN)
    case REG_ST_LAST = case REG_ST7:
    case REG_FPST_LAST = case REG_ST_LAST:
    case REG_MACHINE_LAST = case REG_FPST_LAST:

    case REG_STATUS_FLAGS:
    case REG_DF_FLAG: return string("R_DFLAG");
    case REG_AGGcase REGATE_BASE:
    case REG_FPST_ALL = case REG_AGGcase REGATE_BASE:
    case REG_AGGcase REGATE_LAST = case REG_FPST_ALL:

    case REG_APPLICATION_LAST = case REG_AGGcase REGATE_LAST:

    case REG_PIN_BASE:
    case REG_PIN_GR_BASE = case REG_PIN_BASE:

    // ia32-specific Pin gr regs
    case REG_PIN_EDI = case REG_PIN_GR_BASE:
#if defined(TARGET_IA32)
    case REG_PIN_GDI = case REG_PIN_EDI:                  // PIN_GDI == PIN_EDI on 32 bit: PIN_RDI on 64 bit.
#endif
    case REG_PIN_ESI:
    case REG_PIN_EBP:
    case REG_PIN_ESP:
#if defined (TARGET_IA32)
    case REG_PIN_STACK_PTR = case REG_PIN_ESP:
#endif
    case REG_PIN_EBX:
    case REG_PIN_EDX:
#if defined(TARGET_IA32)
    case REG_PIN_GDX = case REG_PIN_EDX:
#endif
    case REG_PIN_ECX:
#if defined(TARGET_IA32)
    case REG_PIN_GCX = case REG_PIN_ECX:                  // PIN_GCX == PIN_ECX on 32 bit: PIN_RCX on 64 bit.
#endif
    case REG_PIN_EAX:
#if defined(TARGET_IA32)
    case REG_PIN_GAX = case REG_PIN_EAX:                  // PIN_GAX == PIN_EAX on 32 bit: PIN_RAX on 64 bit.
#endif
    case REG_PIN_AL:
    case REG_PIN_AH:
    case REG_PIN_AX:
    case REG_PIN_CL:
    case REG_PIN_CH:
    case REG_PIN_CX:
    case REG_PIN_DL:
    case REG_PIN_DH:
    case REG_PIN_DX:
    case REG_PIN_BL:
    case REG_PIN_BH:
    case REG_PIN_BX:
    case REG_PIN_BP:
    case REG_PIN_SI:
    case REG_PIN_DI:
    case REG_PIN_SP:

#if defined(TARGET_IA32E)
    // Intel(R) 64 architecture specific pin gr regs
    case REG_PIN_RDI:
    case REG_PIN_GDI = case REG_PIN_RDI:
    case REG_PIN_RSI:
    case REG_PIN_RBP:
    case REG_PIN_RSP:

    case REG_PIN_STACK_PTR = case REG_PIN_RSP:

    case REG_PIN_RBX:
    case REG_PIN_RDX:
    case REG_PIN_GDX = case REG_PIN_RDX:
    case REG_PIN_RCX:
    case REG_PIN_GCX = case REG_PIN_RCX:
    case REG_PIN_RAX:
    case REG_PIN_GAX = case REG_PIN_RAX:
    case REG_PIN_R8:
    case REG_PIN_R9:
    case REG_PIN_R10:
    case REG_PIN_R11:
    case REG_PIN_R12:
    case REG_PIN_R13:
    case REG_PIN_R14:
    case REG_PIN_R15:

    case REG_PIN_DIL:
    case REG_PIN_SIL:
    case REG_PIN_BPL:
    case REG_PIN_SPL:

    case REG_PIN_R8B:
    case REG_PIN_R8W:
    case REG_PIN_R8D:

    case REG_PIN_R9B:
    case REG_PIN_R9W:
    case REG_PIN_R9D:

    case REG_PIN_R10B:
    case REG_PIN_R10W:
    case REG_PIN_R10D:

    case REG_PIN_R11B:
    case REG_PIN_R11W:
    case REG_PIN_R11D:

    case REG_PIN_R12B:
    case REG_PIN_R12W:
    case REG_PIN_R12D:

    case REG_PIN_R13B:
    case REG_PIN_R13W:
    case REG_PIN_R13D:

    case REG_PIN_R14B:
    case REG_PIN_R14W:
    case REG_PIN_R14D:

    case REG_PIN_R15B:
    case REG_PIN_R15W:
    case REG_PIN_R15D:
#endif

    // Every thread is assigned an index so we can implement tls
    case REG_THREAD_ID:

    case REG_SEG_GS_VAL:  // virtual reg holding actual value of gs
    case REG_SEG_FS_VAL:  // virtual reg holding actual value of fs

    // ISA-independent gr regs
    case REG_PIN_INDIRcase REG:  // virtual reg holding indirect jmp target value
    case REG_PIN_IPRELADDR: // virtual reg holding ip-rel address value
    case REG_PIN_SYSENTER_RESUMEADDR: // virtual reg holding the resume address from sysenter

    // ISA-independent gr regs holding temporary values
    case REG_PIN_T_BASE:
    case REG_PIN_T0 = case REG_PIN_T_BASE:
    case REG_PIN_T1:
    case REG_PIN_T2:
    case REG_PIN_T3:
    case REG_PIN_T0L:    // lower 8 bits of temporary register
    case REG_PIN_T1L:
    case REG_PIN_T2L:
    case REG_PIN_T3L:
    case REG_PIN_T0W:    // lower 16 bits of temporary register
    case REG_PIN_T1W:
    case REG_PIN_T2W:
    case REG_PIN_T3W:
    case REG_PIN_T0D:    // lower 32 bits of temporary register
    case REG_PIN_T1D:
    case REG_PIN_T2D:
    case REG_PIN_T3D:
    case REG_PIN_T_LAST = case REG_PIN_T3D:

#endif

    // Virtual registers reg holding memory addresses pointed by GS/FS registers
    // These registers are visible for tool writers
    */
#if defined(TARGET_IA32E)
    case REG_SEG_GS_BASE: return string("R_GS_BASE_64"); ///< Base address for GS segment
    case REG_SEG_FS_BASE: return string("R_FS_BASE_64");///< Base address for FS segment
#else
    case REG_SEG_GS_BASE: return string("R_GS_BASE_32"); ///< Base address for GS segment
    case REG_SEG_FS_BASE: return string("R_FS_BASE_32");///< Base address for FS segment
#endif
    /*
    // ISA-independent Pin virtual regs needed for instrumentation
    // These are pin registers visible to the pintool writers.
    case REG_INST_BASE:
    case REG_INST_SCRATCH_BASE = case REG_INST_BASE:  ///< First available scratch register
    case REG_INST_G0 = case REG_INST_SCRATCH_BASE:    ///< Scratch register used in pintools
    case REG_INST_G1:                            ///< Scratch register used in pintools
    case REG_INST_G2:                            ///< Scratch register used in pintools
    case REG_INST_G3:                            ///< Scratch register used in pintools
    case REG_INST_G4:                            ///< Scratch register used in pintools
    case REG_INST_G5:                            ///< Scratch register used in pintools
    case REG_INST_G6:                            ///< Scratch register used in pintools
    case REG_INST_G7:                            ///< Scratch register used in pintools
    case REG_INST_G8:                            ///< Scratch register used in pintools
    case REG_INST_G9:                            ///< Scratch register used in pintools

    case REG_INST_TOOL_FIRST = case REG_INST_G0:
    case REG_INST_TOOL_LAST = case REG_INST_G9:

    case REG_BUF_BASE0:
    case REG_BUF_BASE1:
    case REG_BUF_BASE2:
    case REG_BUF_BASE3:
    case REG_BUF_BASE4:
    case REG_BUF_BASE5:
    case REG_BUF_BASE6:
    case REG_BUF_BASE7:
    case REG_BUF_BASE8:
    case REG_BUF_BASE9:
    case REG_BUF_LAST = case REG_BUF_BASE9:

    case REG_BUF_END0:
    case REG_BUF_END1:
    case REG_BUF_END2:
    case REG_BUF_END3:
    case REG_BUF_END4:
    case REG_BUF_END5:
    case REG_BUF_END6:
    case REG_BUF_END7:
    case REG_BUF_END8:
    case REG_BUF_END9:
    case REG_BUF_ENDLAST = case REG_BUF_END9:

    case REG_INST_SCRATCH_LAST = case REG_BUF_ENDLAST:

#if !defined(TARGET_DOXYGEN)
    case REG_INST_COND:     // for conditional instrumentation.
    case REG_INST_LAST = case REG_INST_COND:

    // Used for memory rewriting: these are not live outside the region
    // but cannot use general purpose scratch registers: because they're
    // used during instrumentation generation: rather than region generation.
    case REG_INST_T0:
    case REG_INST_T0L:
    case REG_INST_T0W:
    case REG_INST_T0D:
    case REG_INST_T1:
    case REG_INST_T2:
    case REG_INST_T3:

    // Used to preserve the predicate value around repped string ops
    case REG_INST_PRESERVED_PREDICATE:

    // Used when the AC flag needs to be cleared before analysis routine
    case REG_FLAGS_BEFORE_AC_CLEARING:

    // Virtual regs used by Pin inside instrumentation bridges.
    // Unlike case REG_INST_BASE to case REG_INST_LAST: these registers are
    // NOT visible to  Pin clients.
    case REG_PIN_BRIDGE_ORIG_SP:    // hold the stack ptr value before the bridge
    case REG_PIN_BRIDGE_APP_IP: // hold the application (not code cache) IP to resume
    case REG_PIN_BRIDGE_SP_BEFORE_ALIGN: // hold the stack ptr value before the stack alignment
    case REG_PIN_BRIDGE_MARSHALLING_FRAME: // hold the address of the marshalled reference registers
    case REG_PIN_BRIDGE_CONTEXT_FRAME: // hold the address of the context frame
    case REG_PIN_BRIDGE_CONTEXT_ORIG_SP: // hold the sp at which the context was pushed

    case REG_PIN_SPILLPTR:  // ptr to the pin spill area
    case REG_PIN_GR_LAST = case REG_PIN_SPILLPTR:

    // case REG_PIN_FLAGS is x86-specific: but since it is not a gr: we put it out of
    // case REG_PIN_GR_BASE and case REG_PIN_GR_LAST

    case REG_PIN_STATUS_FLAGS:
    case REG_PIN_DF_FLAG:

    case REG_PIN_FLAGS:

    case REG_PIN_XMM_BASE:
    case REG_PIN_XMM0 = case REG_PIN_XMM_BASE:
    case REG_PIN_XMM1:
    case REG_PIN_XMM2:
    case REG_PIN_XMM3:
    case REG_PIN_XMM4:
    case REG_PIN_XMM5:
    case REG_PIN_XMM6:
    case REG_PIN_XMM7:
    case REG_PIN_XMM8:
    case REG_PIN_XMM9:
    case REG_PIN_XMM10:
    case REG_PIN_XMM11:
    case REG_PIN_XMM12:
    case REG_PIN_XMM13:
    case REG_PIN_XMM14:
    case REG_PIN_XMM15:

    case REG_PIN_YMM_BASE:
    case REG_PIN_YMM0 = case REG_PIN_YMM_BASE:
    case REG_PIN_YMM1:
    case REG_PIN_YMM2:
    case REG_PIN_YMM3:
    case REG_PIN_YMM4:
    case REG_PIN_YMM5:
    case REG_PIN_YMM6:
    case REG_PIN_YMM7:
    case REG_PIN_YMM8:
    case REG_PIN_YMM9:
    case REG_PIN_YMM10:
    case REG_PIN_YMM11:
    case REG_PIN_YMM12:
    case REG_PIN_YMM13:
    case REG_PIN_YMM14:
    case REG_PIN_YMM15:
    case REG_PIN_LAST = case REG_PIN_YMM15:
#endif
    case REG_LAST
*/

    default:
      return "Unknown";

}


}
