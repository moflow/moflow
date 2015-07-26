addr 0x0 @asm "add    $0x2,%eax"
label pc_0x0
T_t1:u32 = R_EAX_32:u32
T_t2:u32 = 2:u32
R_EAX_32:u32 = R_EAX_32:u32 + T_t2:u32
R_CF:bool = R_EAX_32:u32 < T_t1:u32
R_OF:bool = high:bool((T_t1:u32 ^ ~T_t2:u32) & (T_t1:u32 ^ R_EAX_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX_32:u32 ^ T_t1:u32 ^ T_t2:u32))
R_PF:bool =
  ~low:bool(let T_acc:u32 := R_EAX_32:u32 >> 4:u32 ^ R_EAX_32:u32 in
            let T_acc:u32 := T_acc:u32 >> 2:u32 ^ T_acc:u32 in
            T_acc:u32 >> 1:u32 ^ T_acc:u32)
R_SF:bool = high:bool(R_EAX_32:u32)
R_ZF:bool = 0:u32 == R_EAX_32:u32


