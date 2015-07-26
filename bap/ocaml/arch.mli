(** Supported BAP architectures *)

type arch =
  | X86_32
  | X86_64

val arch_to_string : arch -> string

val arch_of_string : string -> arch

val type_of_arch : arch -> Type.typ

val bits_of_arch : arch -> int

val bytes_of_arch : arch -> int

val mode_of_arch : arch -> Disasm_i386.mode

val mem_of_arch : arch -> Var.t

val sp_of_arch : arch -> Var.t

val conv_of_arch : arch -> Var.defuse
