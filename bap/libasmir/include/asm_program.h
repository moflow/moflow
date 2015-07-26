#ifndef _ASM_PROGRAM_H
#define _ASM_PROGRAM_H


#ifdef __cplusplus
extern "C" 
{
#endif

// from binutils
#include "config.h"
#include <bfd.h>
#include <dis-asm.h>

#ifdef __cplusplus
}
#endif
  
#define MAX_INSN_BYTES 15
  
typedef struct section
{
  bfd_byte *data;
  bfd_size_type datasize;
  
  bfd_vma start_addr; // first byte in the segment
  bfd_vma end_addr; // first byte after the segment

  asection *section;
  int is_code;

  struct section *next;
} section_t;

typedef struct asm_program {
  char *name;
  bfd *abfd;
  struct disassemble_info disasm_info;
  section_t *segs; // linked list of segments
} asm_program_t;


typedef struct memory_cell_data memory_cell_data_t;

#ifdef __cplusplus
#include <vector>
using namespace std;
typedef struct vector<memory_cell_data_t *> memory_data_t;

extern "C"
{
#else
typedef struct memory_data memory_data_t;
#endif

#ifdef __cplusplus
}
extern "C"
{
#endif


extern asm_program_t *asmir_open_file(const char *filename, bfd_vma addr, const char *target);
extern asm_program_t* asmir_new_asmp_for_arch(enum bfd_architecture arch, unsigned long mach);
extern asm_program_t* asmir_trace_asmp_for_arch(enum bfd_architecture arch);
extern void asmir_close(asm_program_t *p);
extern bfd_byte *asmir_get_ptr_to_instr(asm_program_t *prog, bfd_vma addr);
extern int asmir_get_instr_length(asm_program_t *prog, bfd_vma addr);

extern bfd_vma asmir_get_base_address(asm_program_t *prog);
  
// helper for disassembling bytes in traces
extern void set_trace_bytes(void *bytes, size_t len, bfd_vma addr);
  
// dissassemble an instruction and return the asm string
extern char* asmir_string_of_insn(asm_program_t *prog, bfd_vma inst);
extern enum bfd_architecture asmir_get_asmp_arch(asm_program_t *prog);
extern unsigned long asmir_get_asmp_mach(asm_program_t *Program);


// Argh, these functions are documented at
// http://sourceware.org/binutils/docs/bfd/Opening-and-Closing.html
// but don't seem to be in the header files...
extern void *bfd_alloc (bfd *abfd, bfd_size_type wanted);
extern void *bfd_alloc2 (bfd *abfd, bfd_size_type nmemb, bfd_size_type size);


#ifdef __cplusplus
}
#endif


// pulled from disasm.h
#ifndef bfd_get_section_size_before_reloc
#define bfd_get_section_size_before_reloc(x) bfd_get_section_size(x)
#endif



#endif
