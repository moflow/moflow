addr 0x0 @asm "add    %eax,%ebx"
label pc_0x0
T_t1:u32 = R_EBX_32:u32
T_t2:u32 = R_EAX_32:u32
R_EBX_32:u32 = R_EBX_32:u32 + T_t2:u32
R_CF:bool = R_EBX_32:u32 < T_t1:u32
R_OF:bool = high:bool((T_t1:u32 ^ ~T_t2:u32) & (T_t1:u32 ^ R_EBX_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EBX_32:u32 ^ T_t1:u32 ^ T_t2:u32))
R_PF:bool =
  ~low:bool(let T_acc:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc:u32 := T_acc:u32 >> 2:u32 ^ T_acc:u32 in
            T_acc:u32 >> 1:u32 ^ T_acc:u32)
R_SF:bool = high:bool(R_EBX_32:u32)
R_ZF:bool = 0:u32 == R_EBX_32:u32
addr 0x2 @asm "shl    %cl,%ebx"
label pc_0x2
T_origDEST:u32 = R_EBX_32:u32
T_origCOUNT:u32 = R_ECX_32:u32 & 0x1f:u32
R_EBX_32:u32 = R_EBX_32:u32 << (R_ECX_32:u32 & 0x1f:u32)
R_CF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_CF:bool else
  low:bool(T_origDEST:u32 >> 0x20:u32 - T_origCOUNT:u32)
R_OF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_OF:bool else
  if T_origCOUNT:u32 == 1:u32 then high:bool(R_EBX_32:u32) ^ R_CF:bool else
  unknown "OF undefined after shift":bool
R_SF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_SF:bool else high:bool(R_EBX_32:u32)
R_ZF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_ZF:bool else 0:u32 == R_EBX_32:u32
R_PF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_PF:bool else
  ~low:bool(let T_acc_113:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_113:u32 := T_acc_113:u32 >> 2:u32 ^ T_acc_113:u32 in
            T_acc_113:u32 >> 1:u32 ^ T_acc_113:u32)
R_AF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_AF:bool else
  unknown "AF undefined after shift":bool
addr 0x4 @asm "jb     0x0000000000000007"
label pc_0x4
cjmp R_CF:bool, 7:u32, "nocjmp0"
label nocjmp0
addr 0x6 @asm "inc    %eax"
label pc_0x6
T_t:u32 = R_EAX_32:u32
R_EAX_32:u32 = R_EAX_32:u32 + 1:u32
R_OF:bool = high:bool((T_t:u32 ^ -2:u32) & (T_t:u32 ^ R_EAX_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX_32:u32 ^ T_t:u32 ^ 1:u32))
R_PF:bool =
  ~low:bool(let T_acc_115:u32 := R_EAX_32:u32 >> 4:u32 ^ R_EAX_32:u32 in
            let T_acc_115:u32 := T_acc_115:u32 >> 2:u32 ^ T_acc_115:u32 in
            T_acc_115:u32 >> 1:u32 ^ T_acc_115:u32)
R_SF:bool = high:bool(R_EAX_32:u32)
R_ZF:bool = 0:u32 == R_EAX_32:u32
addr 0x7 @asm "inc    %ebx"
label pc_0x7
T_t_116:u32 = R_EBX_32:u32
R_EBX_32:u32 = R_EBX_32:u32 + 1:u32
R_OF:bool = high:bool((T_t_116:u32 ^ -2:u32) & (T_t_116:u32 ^ R_EBX_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EBX_32:u32 ^ T_t_116:u32 ^ 1:u32))
R_PF:bool =
  ~low:bool(let T_acc_117:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_117:u32 := T_acc_117:u32 >> 2:u32 ^ T_acc_117:u32 in
            T_acc_117:u32 >> 1:u32 ^ T_acc_117:u32)
R_SF:bool = high:bool(R_EBX_32:u32)
R_ZF:bool = 0:u32 == R_EBX_32:u32


