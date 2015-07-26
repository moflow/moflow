/** Given a REG, return a trace type (or VT_NONE for failure) */

#include <iostream>
#include "pin.H"
#include "pin_misc.h"

uint32_t pintrace::GetTypeOfReg(REG r) {
  if (REG_is_gr8(r)) return VT_REG8;
  if (REG_is_gr16(r)) return VT_REG16;
  if (REG_is_gr32(r)) return VT_REG32;
  if (REG_is_gr64(r)) return VT_REG64;

  string s = REG_StringShort(r);

  if (s == "eip" || s == "eflags") {
    // No problem for these
    return VT_REG32;
  } else if (s == "rip" || s == "rflags") {
    // Likewise for the 64-bit registers
    return VT_REG64;
  }

  // Otherwise, print a warning...

  std::cerr << "Warning: Unknown register size of register " << REG_StringShort(r) << std::endl;
  return VT_NONE;
}

bool pintrace::valid_regmem_type(pintrace::RegMem_t rm) {
    return (pintrace::NONE <= rm.type && rm.type <= pintrace::MEM);
}
