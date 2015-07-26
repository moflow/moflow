addr 0x8048294 @asm "push   %ebx"
label pc_0x8048294
T_t:u32 = R_EBX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t:u32
addr 0x8048295 @asm "sub    $0x8,%esp"
label pc_0x8048295
T_t_109:u32 = R_ESP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 8:u32
R_CF:bool = T_t_109:u32 < 8:u32
R_OF:bool = high:bool((T_t_109:u32 ^ 8:u32) & (T_t_109:u32 ^ R_ESP_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t_109:u32 ^ 8:u32))
R_PF:bool =
  ~low:bool(let T_acc:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc:u32 := T_acc:u32 >> 2:u32 ^ T_acc:u32 in
            T_acc:u32 >> 1:u32 ^ T_acc:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x8048298 @asm "call   0x000000000804829d"
label pc_0x8048298
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x804829d:u32
jmp 0x804829d:u32 @str "call"
addr 0x804829d @asm "pop    %ebx"
label pc_0x804829d
R_EBX_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x804829e @asm "add    $0x1d57,%ebx"
label pc_0x804829e
T_t1:u32 = R_EBX_32:u32
T_t2:u32 = 0x1d57:u32
R_EBX_32:u32 = R_EBX_32:u32 + T_t2:u32
R_CF:bool = R_EBX_32:u32 < T_t1:u32
R_OF:bool = high:bool((T_t1:u32 ^ ~T_t2:u32) & (T_t1:u32 ^ R_EBX_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EBX_32:u32 ^ T_t1:u32 ^ T_t2:u32))
R_PF:bool =
  ~low:bool(let T_acc_113:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_113:u32 := T_acc_113:u32 >> 2:u32 ^ T_acc_113:u32 in
            T_acc_113:u32 >> 1:u32 ^ T_acc_113:u32)
R_SF:bool = high:bool(R_EBX_32:u32)
R_ZF:bool = 0:u32 == R_EBX_32:u32
addr 0x80482a4 @asm "mov    -0x4(%ebx),%eax"
label pc_0x80482a4
R_EAX_32:u32 = mem32:u32?u8[R_EBX_32:u32 + -4:u32, e_little]:u32
addr 0x80482aa @asm "test   %eax,%eax"
label pc_0x80482aa
T_t_114:u32 = R_EAX_32:u32 & R_EAX_32:u32
R_OF:bool = false
R_CF:bool = false
R_AF:bool = unknown "AF is undefined after and":bool
R_PF:bool =
  ~low:bool(let T_acc_115:u32 := T_t_114:u32 >> 4:u32 ^ T_t_114:u32 in
            let T_acc_115:u32 := T_acc_115:u32 >> 2:u32 ^ T_acc_115:u32 in
            T_acc_115:u32 >> 1:u32 ^ T_acc_115:u32)
R_SF:bool = high:bool(T_t_114:u32)
R_ZF:bool = 0:u32 == T_t_114:u32
addr 0x80482ac @asm "je     0x00000000080482b3"
label pc_0x80482ac
cjmp R_ZF:bool, 0x80482b3:u32, "nocjmp0"
label nocjmp0
addr 0x80482ae @asm "call   0x00000000080482e0"
label pc_0x80482ae
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x80482b3:u32
jmp 0x80482e0:u32 @str "call"
addr 0x80482b3 @asm "call   0x0000000008048390"
label pc_0x80482b3
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x80482b8:u32
jmp 0x8048390:u32 @str "call"
addr 0x80482b8 @asm "call   0x0000000008048460"
label pc_0x80482b8
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x80482bd:u32
jmp 0x8048460:u32 @str "call"
addr 0x80482bd @asm "add    $0x8,%esp"
label pc_0x80482bd
T_t1_116:u32 = R_ESP_32:u32
T_t2_117:u32 = 8:u32
R_ESP_32:u32 = R_ESP_32:u32 + T_t2_117:u32
R_CF:bool = R_ESP_32:u32 < T_t1_116:u32
R_OF:bool =
  high:bool((T_t1_116:u32 ^ ~T_t2_117:u32) & (T_t1_116:u32 ^ R_ESP_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t1_116:u32 ^ T_t2_117:u32))
R_PF:bool =
  ~low:bool(let T_acc_118:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_118:u32 := T_acc_118:u32 >> 2:u32 ^ T_acc_118:u32 in
            T_acc_118:u32 >> 1:u32 ^ T_acc_118:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x80482c0 @asm "pop    %ebx"
label pc_0x80482c0
R_EBX_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x80482c1 @asm "ret    "
label pc_0x80482c1
T_ra:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra:u32 @str "ret"
addr 0x80482d0 @asm "pushl  0x8049ff8"
label pc_0x80482d0
T_t_120:u32 = mem32:u32?u8[0x8049ff8:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_120:u32
addr 0x80482d6 @asm "jmp    *0x8049ffc"
label pc_0x80482d6
jmp mem32:u32?u8[0x8049ffc:u32, e_little]:u32
addr 0x80482dc @asm "add    %al,(%eax)"
label pc_0x80482dc
T_t1_121:u8 = mem32:u32?u8[R_EAX_32:u32, e_little]:u8
T_t2_122:u8 = low:u8(R_EAX_32:u32)
mem32:u32?u8 =
  mem32:u32?u8 with [R_EAX_32:u32, e_little]:u8 =
  mem32:u32?u8[R_EAX_32:u32, e_little]:u8 + T_t2_122:u8
R_CF:bool = mem32:u32?u8[R_EAX_32:u32, e_little]:u8 < T_t1_121:u8
R_OF:bool =
  high:bool((T_t1_121:u8 ^ ~T_t2_122:u8) &
            (T_t1_121:u8 ^ mem32:u32?u8[R_EAX_32:u32, e_little]:u8))
R_AF:bool =
  0x10:u8 ==
  (0x10:u8 &
  (mem32:u32?u8[R_EAX_32:u32, e_little]:u8 ^ T_t1_121:u8 ^ T_t2_122:u8))
R_PF:bool =
  ~low:bool(let T_acc_123:u8 :=
                                 mem32:u32?u8[R_EAX_32:u32, e_little]:u8 >>
                                 4:u8 ^
                                 mem32:u32?u8[R_EAX_32:u32, e_little]:u8 in
            let T_acc_123:u8 := T_acc_123:u8 >> 2:u8 ^ T_acc_123:u8 in
            T_acc_123:u8 >> 1:u8 ^ T_acc_123:u8)
R_SF:bool = high:bool(mem32:u32?u8[R_EAX_32:u32, e_little]:u8)
R_ZF:bool = 0:u8 == mem32:u32?u8[R_EAX_32:u32, e_little]:u8
addr 0x80482de @asm "add    %al,(%eax)"
label pc_0x80482de
T_t1_124:u8 = mem32:u32?u8[R_EAX_32:u32, e_little]:u8
T_t2_125:u8 = low:u8(R_EAX_32:u32)
mem32:u32?u8 =
  mem32:u32?u8 with [R_EAX_32:u32, e_little]:u8 =
  mem32:u32?u8[R_EAX_32:u32, e_little]:u8 + T_t2_125:u8
R_CF:bool = mem32:u32?u8[R_EAX_32:u32, e_little]:u8 < T_t1_124:u8
R_OF:bool =
  high:bool((T_t1_124:u8 ^ ~T_t2_125:u8) &
            (T_t1_124:u8 ^ mem32:u32?u8[R_EAX_32:u32, e_little]:u8))
R_AF:bool =
  0x10:u8 ==
  (0x10:u8 &
  (mem32:u32?u8[R_EAX_32:u32, e_little]:u8 ^ T_t1_124:u8 ^ T_t2_125:u8))
R_PF:bool =
  ~low:bool(let T_acc_126:u8 :=
                                 mem32:u32?u8[R_EAX_32:u32, e_little]:u8 >>
                                 4:u8 ^
                                 mem32:u32?u8[R_EAX_32:u32, e_little]:u8 in
            let T_acc_126:u8 := T_acc_126:u8 >> 2:u8 ^ T_acc_126:u8 in
            T_acc_126:u8 >> 1:u8 ^ T_acc_126:u8)
R_SF:bool = high:bool(mem32:u32?u8[R_EAX_32:u32, e_little]:u8)
R_ZF:bool = 0:u8 == mem32:u32?u8[R_EAX_32:u32, e_little]:u8
addr 0x80482e0 @asm "jmp    *0x804a000"
label pc_0x80482e0
jmp mem32:u32?u8[0x804a000:u32, e_little]:u32
addr 0x80482e6 @asm "push   $0x0"
label pc_0x80482e6
T_t_127:u32 = 0:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_127:u32
addr 0x80482eb @asm "jmp    0x00000000080482d0"
label pc_0x80482eb
jmp 0x80482d0:u32
addr 0x80482f0 @asm "jmp    *0x804a004"
label pc_0x80482f0
jmp mem32:u32?u8[0x804a004:u32, e_little]:u32
addr 0x80482f6 @asm "push   $0x8"
label pc_0x80482f6
T_t_128:u32 = 8:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_128:u32
addr 0x80482fb @asm "jmp    0x00000000080482d0"
label pc_0x80482fb
jmp 0x80482d0:u32
addr 0x8048300 @asm "xor    %ebp,%ebp"
label pc_0x8048300
R_EBP_32:u32 = 0:u32
R_AF:bool = unknown "AF is undefined after xor":bool
R_ZF:bool = true
R_PF:bool = true
R_OF:bool = false
R_CF:bool = false
R_SF:bool = false
addr 0x8048302 @asm "pop    %esi"
label pc_0x8048302
R_ESI_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x8048303 @asm "mov    %esp,%ecx"
label pc_0x8048303
R_ECX_32:u32 = R_ESP_32:u32
addr 0x8048305 @asm "and    $0xfffffff0,%esp"
label pc_0x8048305
R_ESP_32:u32 = R_ESP_32:u32 & 0xfffffff0:u32
R_OF:bool = false
R_CF:bool = false
R_AF:bool = unknown "AF is undefined after and":bool
R_PF:bool =
  ~low:bool(let T_acc_129:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_129:u32 := T_acc_129:u32 >> 2:u32 ^ T_acc_129:u32 in
            T_acc_129:u32 >> 1:u32 ^ T_acc_129:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x8048308 @asm "push   %eax"
label pc_0x8048308
T_t_130:u32 = R_EAX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_130:u32
addr 0x8048309 @asm "push   %esp"
label pc_0x8048309
T_t_131:u32 = R_ESP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_131:u32
addr 0x804830a @asm "push   %edx"
label pc_0x804830a
T_t_132:u32 = R_EDX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_132:u32
addr 0x804830b @asm "push   $0x8048450"
label pc_0x804830b
T_t_133:u32 = 0x8048450:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_133:u32
addr 0x8048310 @asm "push   $0x80483e0"
label pc_0x8048310
T_t_134:u32 = 0x80483e0:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_134:u32
addr 0x8048315 @asm "push   %ecx"
label pc_0x8048315
T_t_135:u32 = R_ECX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_135:u32
addr 0x8048316 @asm "push   %esi"
label pc_0x8048316
T_t_136:u32 = R_ESI_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_136:u32
addr 0x8048317 @asm "push   $0x80483cb"
label pc_0x8048317
T_t_137:u32 = 0x80483cb:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_137:u32
addr 0x804831c @asm "call   0x00000000080482f0"
label pc_0x804831c
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x8048321:u32
jmp 0x80482f0:u32 @str "call"
addr 0x8048321 @asm "hlt    "
label pc_0x8048321
halt R_EAX_32:u32
addr 0x8048322 @asm "nop"
label pc_0x8048322
addr 0x8048323 @asm "nop"
label pc_0x8048323
addr 0x8048324 @asm "nop"
label pc_0x8048324
addr 0x8048325 @asm "nop"
label pc_0x8048325
addr 0x8048326 @asm "nop"
label pc_0x8048326
addr 0x8048327 @asm "nop"
label pc_0x8048327
addr 0x8048328 @asm "nop"
label pc_0x8048328
addr 0x8048329 @asm "nop"
label pc_0x8048329
addr 0x804832a @asm "nop"
label pc_0x804832a
addr 0x804832b @asm "nop"
label pc_0x804832b
addr 0x804832c @asm "nop"
label pc_0x804832c
addr 0x804832d @asm "nop"
label pc_0x804832d
addr 0x804832e @asm "nop"
label pc_0x804832e
addr 0x804832f @asm "nop"
label pc_0x804832f
addr 0x8048330 @asm "push   %ebp"
label pc_0x8048330
T_t_138:u32 = R_EBP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_138:u32
addr 0x8048331 @asm "mov    %esp,%ebp"
label pc_0x8048331
R_EBP_32:u32 = R_ESP_32:u32
addr 0x8048333 @asm "push   %ebx"
label pc_0x8048333
T_t_139:u32 = R_EBX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_139:u32
addr 0x8048334 @asm "sub    $0x4,%esp"
label pc_0x8048334
T_t_140:u32 = R_ESP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
R_CF:bool = T_t_140:u32 < 4:u32
R_OF:bool = high:bool((T_t_140:u32 ^ 4:u32) & (T_t_140:u32 ^ R_ESP_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t_140:u32 ^ 4:u32))
R_PF:bool =
  ~low:bool(let T_acc_141:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_141:u32 := T_acc_141:u32 >> 2:u32 ^ T_acc_141:u32 in
            T_acc_141:u32 >> 1:u32 ^ T_acc_141:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x8048337 @asm "cmpb   $0x0,0x804a010"
label pc_0x8048337
T_t_142:u8 = mem32:u32?u8[0x804a010:u32, e_little]:u8 - 0:u8
R_CF:bool = mem32:u32?u8[0x804a010:u32, e_little]:u8 < 0:u8
R_OF:bool =
  high:bool((mem32:u32?u8[0x804a010:u32, e_little]:u8 ^ 0:u8) &
            (mem32:u32?u8[0x804a010:u32, e_little]:u8 ^ T_t_142:u8))
R_AF:bool =
  0x10:u8 ==
  (0x10:u8 & (T_t_142:u8 ^ mem32:u32?u8[0x804a010:u32, e_little]:u8 ^ 0:u8))
R_PF:bool =
  ~low:bool(let T_acc_143:u8 := T_t_142:u8 >> 4:u8 ^ T_t_142:u8 in
            let T_acc_143:u8 := T_acc_143:u8 >> 2:u8 ^ T_acc_143:u8 in
            T_acc_143:u8 >> 1:u8 ^ T_acc_143:u8)
R_SF:bool = high:bool(T_t_142:u8)
R_ZF:bool = 0:u8 == T_t_142:u8
addr 0x804833e @asm "jne    0x000000000804837f"
label pc_0x804833e
cjmp ~R_ZF:bool, 0x804837f:u32, "nocjmp1"
label nocjmp1
addr 0x8048340 @asm "mov    0x804a014,%eax"
label pc_0x8048340
R_EAX_32:u32 = mem32:u32?u8[0x804a014:u32, e_little]:u32
addr 0x8048345 @asm "mov    $0x8049f20,%ebx"
label pc_0x8048345
R_EBX_32:u32 = 0x8049f20:u32
addr 0x804834a @asm "sub    $0x8049f1c,%ebx"
label pc_0x804834a
T_t_144:u32 = R_EBX_32:u32
R_EBX_32:u32 = R_EBX_32:u32 - 0x8049f1c:u32
R_CF:bool = T_t_144:u32 < 0x8049f1c:u32
R_OF:bool =
  high:bool((T_t_144:u32 ^ 0x8049f1c:u32) & (T_t_144:u32 ^ R_EBX_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_EBX_32:u32 ^ T_t_144:u32 ^ 0x8049f1c:u32))
R_PF:bool =
  ~low:bool(let T_acc_145:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_145:u32 := T_acc_145:u32 >> 2:u32 ^ T_acc_145:u32 in
            T_acc_145:u32 >> 1:u32 ^ T_acc_145:u32)
R_SF:bool = high:bool(R_EBX_32:u32)
R_ZF:bool = 0:u32 == R_EBX_32:u32
addr 0x8048350 @asm "sar    $0x2,%ebx"
label pc_0x8048350
T_origDEST:u32 = R_EBX_32:u32
T_origCOUNT:u32 = 2:u32
R_EBX_32:u32 = R_EBX_32:u32 $>> 2:u32
R_CF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_CF:bool else
  high:bool(T_origDEST:u32 << 0x20:u32 - T_origCOUNT:u32)
R_OF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_OF:bool else
  if T_origCOUNT:u32 == 1:u32 then false else
  unknown "OF undefined after shift":bool
R_SF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_SF:bool else high:bool(R_EBX_32:u32)
R_ZF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_ZF:bool else 0:u32 == R_EBX_32:u32
R_PF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_PF:bool else
  ~low:bool(let T_acc_148:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_148:u32 := T_acc_148:u32 >> 2:u32 ^ T_acc_148:u32 in
            T_acc_148:u32 >> 1:u32 ^ T_acc_148:u32)
R_AF:bool =
  if T_origCOUNT:u32 == 0:u32 then R_AF:bool else
  unknown "AF undefined after shift":bool
addr 0x8048353 @asm "sub    $0x1,%ebx"
label pc_0x8048353
T_t_149:u32 = R_EBX_32:u32
R_EBX_32:u32 = R_EBX_32:u32 - 1:u32
R_CF:bool = T_t_149:u32 < 1:u32
R_OF:bool = high:bool((T_t_149:u32 ^ 1:u32) & (T_t_149:u32 ^ R_EBX_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EBX_32:u32 ^ T_t_149:u32 ^ 1:u32))
R_PF:bool =
  ~low:bool(let T_acc_150:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_150:u32 := T_acc_150:u32 >> 2:u32 ^ T_acc_150:u32 in
            T_acc_150:u32 >> 1:u32 ^ T_acc_150:u32)
R_SF:bool = high:bool(R_EBX_32:u32)
R_ZF:bool = 0:u32 == R_EBX_32:u32
addr 0x8048356 @asm "cmp    %ebx,%eax"
label pc_0x8048356
T_t_151:u32 = R_EAX_32:u32 - R_EBX_32:u32
R_CF:bool = R_EAX_32:u32 < R_EBX_32:u32
R_OF:bool =
  high:bool((R_EAX_32:u32 ^ R_EBX_32:u32) & (R_EAX_32:u32 ^ T_t_151:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (T_t_151:u32 ^ R_EAX_32:u32 ^ R_EBX_32:u32))
R_PF:bool =
  ~low:bool(let T_acc_152:u32 := T_t_151:u32 >> 4:u32 ^ T_t_151:u32 in
            let T_acc_152:u32 := T_acc_152:u32 >> 2:u32 ^ T_acc_152:u32 in
            T_acc_152:u32 >> 1:u32 ^ T_acc_152:u32)
R_SF:bool = high:bool(T_t_151:u32)
R_ZF:bool = 0:u32 == T_t_151:u32
addr 0x8048358 @asm "jae    0x0000000008048378"
label pc_0x8048358
cjmp ~R_CF:bool, 0x8048378:u32, "nocjmp2"
label nocjmp2
addr 0x804835a @asm "lea    0x0(%esi),%esi"
label pc_0x804835a
R_ESI_32:u32 = R_ESI_32:u32 + 0:u32
addr 0x8048360 @asm "add    $0x1,%eax"
label pc_0x8048360
T_t1_153:u32 = R_EAX_32:u32
T_t2_154:u32 = 1:u32
R_EAX_32:u32 = R_EAX_32:u32 + T_t2_154:u32
R_CF:bool = R_EAX_32:u32 < T_t1_153:u32
R_OF:bool =
  high:bool((T_t1_153:u32 ^ ~T_t2_154:u32) & (T_t1_153:u32 ^ R_EAX_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_EAX_32:u32 ^ T_t1_153:u32 ^ T_t2_154:u32))
R_PF:bool =
  ~low:bool(let T_acc_155:u32 := R_EAX_32:u32 >> 4:u32 ^ R_EAX_32:u32 in
            let T_acc_155:u32 := T_acc_155:u32 >> 2:u32 ^ T_acc_155:u32 in
            T_acc_155:u32 >> 1:u32 ^ T_acc_155:u32)
R_SF:bool = high:bool(R_EAX_32:u32)
R_ZF:bool = 0:u32 == R_EAX_32:u32
addr 0x8048363 @asm "mov    %eax,0x804a014"
label pc_0x8048363
mem32:u32?u8 = mem32:u32?u8 with [0x804a014:u32, e_little]:u32 = R_EAX_32:u32
addr 0x8048368 @asm "call   *0x8049f1c(,%eax,4)"
label pc_0x8048368
T_target:u32 =
  mem32:u32?u8[0x8049f1c:u32 + (R_EAX_32:u32 << 2:u32), e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x804836f:u32
jmp T_target:u32 @str "call"
addr 0x804836f @asm "mov    0x804a014,%eax"
label pc_0x804836f
R_EAX_32:u32 = mem32:u32?u8[0x804a014:u32, e_little]:u32
addr 0x8048374 @asm "cmp    %ebx,%eax"
label pc_0x8048374
T_t_157:u32 = R_EAX_32:u32 - R_EBX_32:u32
R_CF:bool = R_EAX_32:u32 < R_EBX_32:u32
R_OF:bool =
  high:bool((R_EAX_32:u32 ^ R_EBX_32:u32) & (R_EAX_32:u32 ^ T_t_157:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (T_t_157:u32 ^ R_EAX_32:u32 ^ R_EBX_32:u32))
R_PF:bool =
  ~low:bool(let T_acc_158:u32 := T_t_157:u32 >> 4:u32 ^ T_t_157:u32 in
            let T_acc_158:u32 := T_acc_158:u32 >> 2:u32 ^ T_acc_158:u32 in
            T_acc_158:u32 >> 1:u32 ^ T_acc_158:u32)
R_SF:bool = high:bool(T_t_157:u32)
R_ZF:bool = 0:u32 == T_t_157:u32
addr 0x8048376 @asm "jb     0x0000000008048360"
label pc_0x8048376
cjmp R_CF:bool, 0x8048360:u32, "nocjmp3"
label nocjmp3
addr 0x8048378 @asm "movb   $0x1,0x804a010"
label pc_0x8048378
mem32:u32?u8 = mem32:u32?u8 with [0x804a010:u32, e_little]:u8 = 1:u8
addr 0x804837f @asm "add    $0x4,%esp"
label pc_0x804837f
T_t1_159:u32 = R_ESP_32:u32
T_t2_160:u32 = 4:u32
R_ESP_32:u32 = R_ESP_32:u32 + T_t2_160:u32
R_CF:bool = R_ESP_32:u32 < T_t1_159:u32
R_OF:bool =
  high:bool((T_t1_159:u32 ^ ~T_t2_160:u32) & (T_t1_159:u32 ^ R_ESP_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t1_159:u32 ^ T_t2_160:u32))
R_PF:bool =
  ~low:bool(let T_acc_161:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_161:u32 := T_acc_161:u32 >> 2:u32 ^ T_acc_161:u32 in
            T_acc_161:u32 >> 1:u32 ^ T_acc_161:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x8048382 @asm "pop    %ebx"
label pc_0x8048382
R_EBX_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x8048383 @asm "pop    %ebp"
label pc_0x8048383
R_EBP_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x8048384 @asm "ret    "
label pc_0x8048384
T_ra_162:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_162:u32 @str "ret"
addr 0x8048385 @asm "lea    0x0(%esi,%eiz,1),%esi"
label pc_0x8048385
R_ESI_32:u32 = R_ESI_32:u32 + 0:u32
addr 0x8048389 @asm "lea    0x0(%edi,%eiz,1),%edi"
label pc_0x8048389
R_EDI_32:u32 = R_EDI_32:u32 + 0:u32
addr 0x8048390 @asm "push   %ebp"
label pc_0x8048390
T_t_163:u32 = R_EBP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_163:u32
addr 0x8048391 @asm "mov    %esp,%ebp"
label pc_0x8048391
R_EBP_32:u32 = R_ESP_32:u32
addr 0x8048393 @asm "sub    $0x18,%esp"
label pc_0x8048393
T_t_164:u32 = R_ESP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 0x18:u32
R_CF:bool = T_t_164:u32 < 0x18:u32
R_OF:bool =
  high:bool((T_t_164:u32 ^ 0x18:u32) & (T_t_164:u32 ^ R_ESP_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t_164:u32 ^ 0x18:u32))
R_PF:bool =
  ~low:bool(let T_acc_165:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_165:u32 := T_acc_165:u32 >> 2:u32 ^ T_acc_165:u32 in
            T_acc_165:u32 >> 1:u32 ^ T_acc_165:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x8048396 @asm "mov    0x8049f24,%eax"
label pc_0x8048396
R_EAX_32:u32 = mem32:u32?u8[0x8049f24:u32, e_little]:u32
addr 0x804839b @asm "test   %eax,%eax"
label pc_0x804839b
T_t_166:u32 = R_EAX_32:u32 & R_EAX_32:u32
R_OF:bool = false
R_CF:bool = false
R_AF:bool = unknown "AF is undefined after and":bool
R_PF:bool =
  ~low:bool(let T_acc_167:u32 := T_t_166:u32 >> 4:u32 ^ T_t_166:u32 in
            let T_acc_167:u32 := T_acc_167:u32 >> 2:u32 ^ T_acc_167:u32 in
            T_acc_167:u32 >> 1:u32 ^ T_acc_167:u32)
R_SF:bool = high:bool(T_t_166:u32)
R_ZF:bool = 0:u32 == T_t_166:u32
addr 0x804839d @asm "je     0x00000000080483b1"
label pc_0x804839d
cjmp R_ZF:bool, 0x80483b1:u32, "nocjmp4"
label nocjmp4
addr 0x804839f @asm "mov    $0x0,%eax"
label pc_0x804839f
R_EAX_32:u32 = 0:u32
addr 0x80483a4 @asm "test   %eax,%eax"
label pc_0x80483a4
T_t_168:u32 = R_EAX_32:u32 & R_EAX_32:u32
R_OF:bool = false
R_CF:bool = false
R_AF:bool = unknown "AF is undefined after and":bool
R_PF:bool =
  ~low:bool(let T_acc_169:u32 := T_t_168:u32 >> 4:u32 ^ T_t_168:u32 in
            let T_acc_169:u32 := T_acc_169:u32 >> 2:u32 ^ T_acc_169:u32 in
            T_acc_169:u32 >> 1:u32 ^ T_acc_169:u32)
R_SF:bool = high:bool(T_t_168:u32)
R_ZF:bool = 0:u32 == T_t_168:u32
addr 0x80483a6 @asm "je     0x00000000080483b1"
label pc_0x80483a6
cjmp R_ZF:bool, 0x80483b1:u32, "nocjmp5"
label nocjmp5
addr 0x80483a8 @asm "movl   $0x8049f24,(%esp)"
label pc_0x80483a8
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x8049f24:u32
addr 0x80483af @asm "call   *%eax"
label pc_0x80483af
T_target_170:u32 = R_EAX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x80483b1:u32
jmp T_target_170:u32 @str "call"
addr 0x80483b1 @asm "leave  "
label pc_0x80483b1
R_ESP_32:u32 = R_EBP_32:u32
R_EBP_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x80483b2 @asm "ret    "
label pc_0x80483b2
T_ra_171:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_171:u32 @str "ret"
addr 0x80483b3 @asm "nop"
label pc_0x80483b3
addr 0x80483b4 @asm "push   %ebp"
label pc_0x80483b4
T_t_172:u32 = R_EBP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_172:u32
addr 0x80483b5 @asm "mov    %esp,%ebp"
label pc_0x80483b5
R_EBP_32:u32 = R_ESP_32:u32
addr 0x80483b7 @asm "cmpl   $0x2a,0x8(%ebp)"
label pc_0x80483b7
T_t_173:u32 = mem32:u32?u8[R_EBP_32:u32 + 8:u32, e_little]:u32 - 0x2a:u32
R_CF:bool = mem32:u32?u8[R_EBP_32:u32 + 8:u32, e_little]:u32 < 0x2a:u32
R_OF:bool =
  high:bool((mem32:u32?u8[R_EBP_32:u32 + 8:u32, e_little]:u32 ^ 0x2a:u32) &
            (mem32:u32?u8[R_EBP_32:u32 + 8:u32, e_little]:u32 ^ T_t_173:u32))
R_AF:bool =
  0x10:u32 ==
  (0x10:u32 &
  (T_t_173:u32 ^ mem32:u32?u8[R_EBP_32:u32 + 8:u32, e_little]:u32 ^ 0x2a:u32))
R_PF:bool =
  ~low:bool(let T_acc_174:u32 := T_t_173:u32 >> 4:u32 ^ T_t_173:u32 in
            let T_acc_174:u32 := T_acc_174:u32 >> 2:u32 ^ T_acc_174:u32 in
            T_acc_174:u32 >> 1:u32 ^ T_acc_174:u32)
R_SF:bool = high:bool(T_t_173:u32)
R_ZF:bool = 0:u32 == T_t_173:u32
addr 0x80483bb @asm "jne    0x00000000080483c4"
label pc_0x80483bb
cjmp ~R_ZF:bool, 0x80483c4:u32, "nocjmp6"
label nocjmp6
addr 0x80483bd @asm "mov    $0x2a,%eax"
label pc_0x80483bd
R_EAX_32:u32 = 0x2a:u32
addr 0x80483c2 @asm "jmp    0x00000000080483c9"
label pc_0x80483c2
jmp 0x80483c9:u32
addr 0x80483c4 @asm "mov    $0xffffffff,%eax"
label pc_0x80483c4
R_EAX_32:u32 = -1:u32
addr 0x80483c9 @asm "pop    %ebp"
label pc_0x80483c9
R_EBP_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x80483ca @asm "ret    "
label pc_0x80483ca
T_ra_175:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_175:u32 @str "ret"
addr 0x80483cb @asm "push   %ebp"
label pc_0x80483cb
T_t_176:u32 = R_EBP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_176:u32
addr 0x80483cc @asm "mov    %esp,%ebp"
label pc_0x80483cc
R_EBP_32:u32 = R_ESP_32:u32
addr 0x80483ce @asm "sub    $0x4,%esp"
label pc_0x80483ce
T_t_177:u32 = R_ESP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
R_CF:bool = T_t_177:u32 < 4:u32
R_OF:bool = high:bool((T_t_177:u32 ^ 4:u32) & (T_t_177:u32 ^ R_ESP_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t_177:u32 ^ 4:u32))
R_PF:bool =
  ~low:bool(let T_acc_178:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_178:u32 := T_acc_178:u32 >> 2:u32 ^ T_acc_178:u32 in
            T_acc_178:u32 >> 1:u32 ^ T_acc_178:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x80483d1 @asm "movl   $0x2a,(%esp)"
label pc_0x80483d1
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x2a:u32
addr 0x80483d8 @asm "call   0x00000000080483b4"
label pc_0x80483d8
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x80483dd:u32
jmp 0x80483b4:u32 @str "call"
addr 0x80483dd @asm "leave  "
label pc_0x80483dd
R_ESP_32:u32 = R_EBP_32:u32
R_EBP_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x80483de @asm "ret    "
label pc_0x80483de
T_ra_179:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_179:u32 @str "ret"
addr 0x80483df @asm "nop"
label pc_0x80483df
addr 0x80483e0 @asm "push   %ebp"
label pc_0x80483e0
T_t_180:u32 = R_EBP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_180:u32
addr 0x80483e1 @asm "push   %edi"
label pc_0x80483e1
T_t_181:u32 = R_EDI_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_181:u32
addr 0x80483e2 @asm "push   %esi"
label pc_0x80483e2
T_t_182:u32 = R_ESI_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_182:u32
addr 0x80483e3 @asm "push   %ebx"
label pc_0x80483e3
T_t_183:u32 = R_EBX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_183:u32
addr 0x80483e4 @asm "call   0x0000000008048452"
label pc_0x80483e4
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x80483e9:u32
jmp 0x8048452:u32 @str "call"
addr 0x80483e9 @asm "add    $0x1c0b,%ebx"
label pc_0x80483e9
T_t1_184:u32 = R_EBX_32:u32
T_t2_185:u32 = 0x1c0b:u32
R_EBX_32:u32 = R_EBX_32:u32 + T_t2_185:u32
R_CF:bool = R_EBX_32:u32 < T_t1_184:u32
R_OF:bool =
  high:bool((T_t1_184:u32 ^ ~T_t2_185:u32) & (T_t1_184:u32 ^ R_EBX_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_EBX_32:u32 ^ T_t1_184:u32 ^ T_t2_185:u32))
R_PF:bool =
  ~low:bool(let T_acc_186:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_186:u32 := T_acc_186:u32 >> 2:u32 ^ T_acc_186:u32 in
            T_acc_186:u32 >> 1:u32 ^ T_acc_186:u32)
R_SF:bool = high:bool(R_EBX_32:u32)
R_ZF:bool = 0:u32 == R_EBX_32:u32
addr 0x80483ef @asm "sub    $0x1c,%esp"
label pc_0x80483ef
T_t_187:u32 = R_ESP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 0x1c:u32
R_CF:bool = T_t_187:u32 < 0x1c:u32
R_OF:bool =
  high:bool((T_t_187:u32 ^ 0x1c:u32) & (T_t_187:u32 ^ R_ESP_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t_187:u32 ^ 0x1c:u32))
R_PF:bool =
  ~low:bool(let T_acc_188:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_188:u32 := T_acc_188:u32 >> 2:u32 ^ T_acc_188:u32 in
            T_acc_188:u32 >> 1:u32 ^ T_acc_188:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x80483f2 @asm "mov    0x30(%esp),%ebp"
label pc_0x80483f2
R_EBP_32:u32 = mem32:u32?u8[R_ESP_32:u32 + 0x30:u32, e_little]:u32
addr 0x80483f6 @asm "lea    -0xe0(%ebx),%edi"
label pc_0x80483f6
R_EDI_32:u32 = R_EBX_32:u32 + 0xffffff20:u32
addr 0x80483fc @asm "call   0x0000000008048294"
label pc_0x80483fc
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x8048401:u32
jmp 0x8048294:u32 @str "call"
addr 0x8048401 @asm "lea    -0xe0(%ebx),%eax"
label pc_0x8048401
R_EAX_32:u32 = R_EBX_32:u32 + 0xffffff20:u32
addr 0x8048407 @asm "sub    %eax,%edi"
label pc_0x8048407
T_t_189:u32 = R_EDI_32:u32
R_EDI_32:u32 = R_EDI_32:u32 - R_EAX_32:u32
R_CF:bool = T_t_189:u32 < R_EAX_32:u32
R_OF:bool =
  high:bool((T_t_189:u32 ^ R_EAX_32:u32) & (T_t_189:u32 ^ R_EDI_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_EDI_32:u32 ^ T_t_189:u32 ^ R_EAX_32:u32))
R_PF:bool =
  ~low:bool(let T_acc_190:u32 := R_EDI_32:u32 >> 4:u32 ^ R_EDI_32:u32 in
            let T_acc_190:u32 := T_acc_190:u32 >> 2:u32 ^ T_acc_190:u32 in
            T_acc_190:u32 >> 1:u32 ^ T_acc_190:u32)
R_SF:bool = high:bool(R_EDI_32:u32)
R_ZF:bool = 0:u32 == R_EDI_32:u32
addr 0x8048409 @asm "sar    $0x2,%edi"
label pc_0x8048409
T_origDEST_191:u32 = R_EDI_32:u32
T_origCOUNT_192:u32 = 2:u32
R_EDI_32:u32 = R_EDI_32:u32 $>> 2:u32
R_CF:bool =
  if T_origCOUNT_192:u32 == 0:u32 then R_CF:bool else
  high:bool(T_origDEST_191:u32 << 0x20:u32 - T_origCOUNT_192:u32)
R_OF:bool =
  if T_origCOUNT_192:u32 == 0:u32 then R_OF:bool else
  if T_origCOUNT_192:u32 == 1:u32 then false else
  unknown "OF undefined after shift":bool
R_SF:bool =
  if T_origCOUNT_192:u32 == 0:u32 then R_SF:bool else high:bool(R_EDI_32:u32)
R_ZF:bool =
  if T_origCOUNT_192:u32 == 0:u32 then R_ZF:bool else 0:u32 == R_EDI_32:u32
R_PF:bool =
  if T_origCOUNT_192:u32 == 0:u32 then R_PF:bool else
  ~low:bool(let T_acc_193:u32 := R_EDI_32:u32 >> 4:u32 ^ R_EDI_32:u32 in
            let T_acc_193:u32 := T_acc_193:u32 >> 2:u32 ^ T_acc_193:u32 in
            T_acc_193:u32 >> 1:u32 ^ T_acc_193:u32)
R_AF:bool =
  if T_origCOUNT_192:u32 == 0:u32 then R_AF:bool else
  unknown "AF undefined after shift":bool
addr 0x804840c @asm "test   %edi,%edi"
label pc_0x804840c
T_t_194:u32 = R_EDI_32:u32 & R_EDI_32:u32
R_OF:bool = false
R_CF:bool = false
R_AF:bool = unknown "AF is undefined after and":bool
R_PF:bool =
  ~low:bool(let T_acc_195:u32 := T_t_194:u32 >> 4:u32 ^ T_t_194:u32 in
            let T_acc_195:u32 := T_acc_195:u32 >> 2:u32 ^ T_acc_195:u32 in
            T_acc_195:u32 >> 1:u32 ^ T_acc_195:u32)
R_SF:bool = high:bool(T_t_194:u32)
R_ZF:bool = 0:u32 == T_t_194:u32
addr 0x804840e @asm "je     0x0000000008048439"
label pc_0x804840e
cjmp R_ZF:bool, 0x8048439:u32, "nocjmp7"
label nocjmp7
addr 0x8048410 @asm "xor    %esi,%esi"
label pc_0x8048410
R_ESI_32:u32 = 0:u32
R_AF:bool = unknown "AF is undefined after xor":bool
R_ZF:bool = true
R_PF:bool = true
R_OF:bool = false
R_CF:bool = false
R_SF:bool = false
addr 0x8048412 @asm "lea    0x0(%esi),%esi"
label pc_0x8048412
R_ESI_32:u32 = R_ESI_32:u32 + 0:u32
addr 0x8048418 @asm "mov    0x38(%esp),%eax"
label pc_0x8048418
R_EAX_32:u32 = mem32:u32?u8[R_ESP_32:u32 + 0x38:u32, e_little]:u32
addr 0x804841c @asm "mov    %ebp,(%esp)"
label pc_0x804841c
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = R_EBP_32:u32
addr 0x804841f @asm "mov    %eax,0x8(%esp)"
label pc_0x804841f
mem32:u32?u8 =
  mem32:u32?u8 with [R_ESP_32:u32 + 8:u32, e_little]:u32 = R_EAX_32:u32
addr 0x8048423 @asm "mov    0x34(%esp),%eax"
label pc_0x8048423
R_EAX_32:u32 = mem32:u32?u8[R_ESP_32:u32 + 0x34:u32, e_little]:u32
addr 0x8048427 @asm "mov    %eax,0x4(%esp)"
label pc_0x8048427
mem32:u32?u8 =
  mem32:u32?u8 with [R_ESP_32:u32 + 4:u32, e_little]:u32 = R_EAX_32:u32
addr 0x804842b @asm "call   *-0xe0(%ebx,%esi,4)"
label pc_0x804842b
T_target_196:u32 =
  mem32:u32?u8[R_EBX_32:u32 + (R_ESI_32:u32 << 2:u32) + 0xffffff20:u32,
  e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x8048432:u32
jmp T_target_196:u32 @str "call"
addr 0x8048432 @asm "add    $0x1,%esi"
label pc_0x8048432
T_t1_197:u32 = R_ESI_32:u32
T_t2_198:u32 = 1:u32
R_ESI_32:u32 = R_ESI_32:u32 + T_t2_198:u32
R_CF:bool = R_ESI_32:u32 < T_t1_197:u32
R_OF:bool =
  high:bool((T_t1_197:u32 ^ ~T_t2_198:u32) & (T_t1_197:u32 ^ R_ESI_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_ESI_32:u32 ^ T_t1_197:u32 ^ T_t2_198:u32))
R_PF:bool =
  ~low:bool(let T_acc_199:u32 := R_ESI_32:u32 >> 4:u32 ^ R_ESI_32:u32 in
            let T_acc_199:u32 := T_acc_199:u32 >> 2:u32 ^ T_acc_199:u32 in
            T_acc_199:u32 >> 1:u32 ^ T_acc_199:u32)
R_SF:bool = high:bool(R_ESI_32:u32)
R_ZF:bool = 0:u32 == R_ESI_32:u32
addr 0x8048435 @asm "cmp    %edi,%esi"
label pc_0x8048435
T_t_200:u32 = R_ESI_32:u32 - R_EDI_32:u32
R_CF:bool = R_ESI_32:u32 < R_EDI_32:u32
R_OF:bool =
  high:bool((R_ESI_32:u32 ^ R_EDI_32:u32) & (R_ESI_32:u32 ^ T_t_200:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (T_t_200:u32 ^ R_ESI_32:u32 ^ R_EDI_32:u32))
R_PF:bool =
  ~low:bool(let T_acc_201:u32 := T_t_200:u32 >> 4:u32 ^ T_t_200:u32 in
            let T_acc_201:u32 := T_acc_201:u32 >> 2:u32 ^ T_acc_201:u32 in
            T_acc_201:u32 >> 1:u32 ^ T_acc_201:u32)
R_SF:bool = high:bool(T_t_200:u32)
R_ZF:bool = 0:u32 == T_t_200:u32
addr 0x8048437 @asm "jne    0x0000000008048418"
label pc_0x8048437
cjmp ~R_ZF:bool, 0x8048418:u32, "nocjmp8"
label nocjmp8
addr 0x8048439 @asm "add    $0x1c,%esp"
label pc_0x8048439
T_t1_202:u32 = R_ESP_32:u32
T_t2_203:u32 = 0x1c:u32
R_ESP_32:u32 = R_ESP_32:u32 + T_t2_203:u32
R_CF:bool = R_ESP_32:u32 < T_t1_202:u32
R_OF:bool =
  high:bool((T_t1_202:u32 ^ ~T_t2_203:u32) & (T_t1_202:u32 ^ R_ESP_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t1_202:u32 ^ T_t2_203:u32))
R_PF:bool =
  ~low:bool(let T_acc_204:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_204:u32 := T_acc_204:u32 >> 2:u32 ^ T_acc_204:u32 in
            T_acc_204:u32 >> 1:u32 ^ T_acc_204:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x804843c @asm "pop    %ebx"
label pc_0x804843c
R_EBX_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x804843d @asm "pop    %esi"
label pc_0x804843d
R_ESI_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x804843e @asm "pop    %edi"
label pc_0x804843e
R_EDI_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x804843f @asm "pop    %ebp"
label pc_0x804843f
R_EBP_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x8048440 @asm "ret    "
label pc_0x8048440
T_ra_205:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_205:u32 @str "ret"
addr 0x8048441 @asm "jmp    0x0000000008048450"
label pc_0x8048441
jmp 0x8048450:u32
addr 0x8048443 @asm "nop"
label pc_0x8048443
addr 0x8048444 @asm "nop"
label pc_0x8048444
addr 0x8048445 @asm "nop"
label pc_0x8048445
addr 0x8048446 @asm "nop"
label pc_0x8048446
addr 0x8048447 @asm "nop"
label pc_0x8048447
addr 0x8048448 @asm "nop"
label pc_0x8048448
addr 0x8048449 @asm "nop"
label pc_0x8048449
addr 0x804844a @asm "nop"
label pc_0x804844a
addr 0x804844b @asm "nop"
label pc_0x804844b
addr 0x804844c @asm "nop"
label pc_0x804844c
addr 0x804844d @asm "nop"
label pc_0x804844d
addr 0x804844e @asm "nop"
label pc_0x804844e
addr 0x804844f @asm "nop"
label pc_0x804844f
addr 0x8048450 @asm "repz ret "
label pc_0x8048450
T_ra_206:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_206:u32 @str "ret"
addr 0x8048452 @asm "mov    (%esp),%ebx"
label pc_0x8048452
R_EBX_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
addr 0x8048455 @asm "ret    "
label pc_0x8048455
T_ra_207:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_207:u32 @str "ret"
addr 0x8048456 @asm "nop"
label pc_0x8048456
addr 0x8048457 @asm "nop"
label pc_0x8048457
addr 0x8048458 @asm "nop"
label pc_0x8048458
addr 0x8048459 @asm "nop"
label pc_0x8048459
addr 0x804845a @asm "nop"
label pc_0x804845a
addr 0x804845b @asm "nop"
label pc_0x804845b
addr 0x804845c @asm "nop"
label pc_0x804845c
addr 0x804845d @asm "nop"
label pc_0x804845d
addr 0x804845e @asm "nop"
label pc_0x804845e
addr 0x804845f @asm "nop"
label pc_0x804845f
addr 0x8048460 @asm "push   %ebp"
label pc_0x8048460
T_t_208:u32 = R_EBP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_208:u32
addr 0x8048461 @asm "mov    %esp,%ebp"
label pc_0x8048461
R_EBP_32:u32 = R_ESP_32:u32
addr 0x8048463 @asm "push   %ebx"
label pc_0x8048463
T_t_209:u32 = R_EBX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_209:u32
addr 0x8048464 @asm "sub    $0x4,%esp"
label pc_0x8048464
T_t_210:u32 = R_ESP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
R_CF:bool = T_t_210:u32 < 4:u32
R_OF:bool = high:bool((T_t_210:u32 ^ 4:u32) & (T_t_210:u32 ^ R_ESP_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t_210:u32 ^ 4:u32))
R_PF:bool =
  ~low:bool(let T_acc_211:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_211:u32 := T_acc_211:u32 >> 2:u32 ^ T_acc_211:u32 in
            T_acc_211:u32 >> 1:u32 ^ T_acc_211:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x8048467 @asm "mov    0x8049f14,%eax"
label pc_0x8048467
R_EAX_32:u32 = mem32:u32?u8[0x8049f14:u32, e_little]:u32
addr 0x804846c @asm "cmp    $0xffffffff,%eax"
label pc_0x804846c
T_t_212:u32 = R_EAX_32:u32 - -1:u32
R_CF:bool = R_EAX_32:u32 < -1:u32
R_OF:bool = high:bool((R_EAX_32:u32 ^ -1:u32) & (R_EAX_32:u32 ^ T_t_212:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (T_t_212:u32 ^ R_EAX_32:u32 ^ -1:u32))
R_PF:bool =
  ~low:bool(let T_acc_213:u32 := T_t_212:u32 >> 4:u32 ^ T_t_212:u32 in
            let T_acc_213:u32 := T_acc_213:u32 >> 2:u32 ^ T_acc_213:u32 in
            T_acc_213:u32 >> 1:u32 ^ T_acc_213:u32)
R_SF:bool = high:bool(T_t_212:u32)
R_ZF:bool = 0:u32 == T_t_212:u32
addr 0x804846f @asm "je     0x0000000008048484"
label pc_0x804846f
cjmp R_ZF:bool, 0x8048484:u32, "nocjmp9"
label nocjmp9
addr 0x8048471 @asm "mov    $0x8049f14,%ebx"
label pc_0x8048471
R_EBX_32:u32 = 0x8049f14:u32
addr 0x8048476 @asm "xchg   %ax,%ax"
label pc_0x8048476
addr 0x8048478 @asm "sub    $0x4,%ebx"
label pc_0x8048478
T_t_214:u32 = R_EBX_32:u32
R_EBX_32:u32 = R_EBX_32:u32 - 4:u32
R_CF:bool = T_t_214:u32 < 4:u32
R_OF:bool = high:bool((T_t_214:u32 ^ 4:u32) & (T_t_214:u32 ^ R_EBX_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EBX_32:u32 ^ T_t_214:u32 ^ 4:u32))
R_PF:bool =
  ~low:bool(let T_acc_215:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_215:u32 := T_acc_215:u32 >> 2:u32 ^ T_acc_215:u32 in
            T_acc_215:u32 >> 1:u32 ^ T_acc_215:u32)
R_SF:bool = high:bool(R_EBX_32:u32)
R_ZF:bool = 0:u32 == R_EBX_32:u32
addr 0x804847b @asm "call   *%eax"
label pc_0x804847b
T_target_216:u32 = R_EAX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x804847d:u32
jmp T_target_216:u32 @str "call"
addr 0x804847d @asm "mov    (%ebx),%eax"
label pc_0x804847d
R_EAX_32:u32 = mem32:u32?u8[R_EBX_32:u32, e_little]:u32
addr 0x804847f @asm "cmp    $0xffffffff,%eax"
label pc_0x804847f
T_t_217:u32 = R_EAX_32:u32 - -1:u32
R_CF:bool = R_EAX_32:u32 < -1:u32
R_OF:bool = high:bool((R_EAX_32:u32 ^ -1:u32) & (R_EAX_32:u32 ^ T_t_217:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (T_t_217:u32 ^ R_EAX_32:u32 ^ -1:u32))
R_PF:bool =
  ~low:bool(let T_acc_218:u32 := T_t_217:u32 >> 4:u32 ^ T_t_217:u32 in
            let T_acc_218:u32 := T_acc_218:u32 >> 2:u32 ^ T_acc_218:u32 in
            T_acc_218:u32 >> 1:u32 ^ T_acc_218:u32)
R_SF:bool = high:bool(T_t_217:u32)
R_ZF:bool = 0:u32 == T_t_217:u32
addr 0x8048482 @asm "jne    0x0000000008048478"
label pc_0x8048482
cjmp ~R_ZF:bool, 0x8048478:u32, "nocjmp10"
label nocjmp10
addr 0x8048484 @asm "add    $0x4,%esp"
label pc_0x8048484
T_t1_219:u32 = R_ESP_32:u32
T_t2_220:u32 = 4:u32
R_ESP_32:u32 = R_ESP_32:u32 + T_t2_220:u32
R_CF:bool = R_ESP_32:u32 < T_t1_219:u32
R_OF:bool =
  high:bool((T_t1_219:u32 ^ ~T_t2_220:u32) & (T_t1_219:u32 ^ R_ESP_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t1_219:u32 ^ T_t2_220:u32))
R_PF:bool =
  ~low:bool(let T_acc_221:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_221:u32 := T_acc_221:u32 >> 2:u32 ^ T_acc_221:u32 in
            T_acc_221:u32 >> 1:u32 ^ T_acc_221:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x8048487 @asm "pop    %ebx"
label pc_0x8048487
R_EBX_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x8048488 @asm "pop    %ebp"
label pc_0x8048488
R_EBP_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x8048489 @asm "ret    "
label pc_0x8048489
T_ra_222:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_222:u32 @str "ret"
addr 0x804848a @asm "nop"
label pc_0x804848a
addr 0x804848b @asm "nop"
label pc_0x804848b
addr 0x804848c @asm "push   %ebx"
label pc_0x804848c
T_t_223:u32 = R_EBX_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = T_t_223:u32
addr 0x804848d @asm "sub    $0x8,%esp"
label pc_0x804848d
T_t_224:u32 = R_ESP_32:u32
R_ESP_32:u32 = R_ESP_32:u32 - 8:u32
R_CF:bool = T_t_224:u32 < 8:u32
R_OF:bool = high:bool((T_t_224:u32 ^ 8:u32) & (T_t_224:u32 ^ R_ESP_32:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t_224:u32 ^ 8:u32))
R_PF:bool =
  ~low:bool(let T_acc_225:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_225:u32 := T_acc_225:u32 >> 2:u32 ^ T_acc_225:u32 in
            T_acc_225:u32 >> 1:u32 ^ T_acc_225:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x8048490 @asm "call   0x0000000008048495"
label pc_0x8048490
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x8048495:u32
jmp 0x8048495:u32 @str "call"
addr 0x8048495 @asm "pop    %ebx"
label pc_0x8048495
R_EBX_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x8048496 @asm "add    $0x1b5f,%ebx"
label pc_0x8048496
T_t1_226:u32 = R_EBX_32:u32
T_t2_227:u32 = 0x1b5f:u32
R_EBX_32:u32 = R_EBX_32:u32 + T_t2_227:u32
R_CF:bool = R_EBX_32:u32 < T_t1_226:u32
R_OF:bool =
  high:bool((T_t1_226:u32 ^ ~T_t2_227:u32) & (T_t1_226:u32 ^ R_EBX_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_EBX_32:u32 ^ T_t1_226:u32 ^ T_t2_227:u32))
R_PF:bool =
  ~low:bool(let T_acc_228:u32 := R_EBX_32:u32 >> 4:u32 ^ R_EBX_32:u32 in
            let T_acc_228:u32 := T_acc_228:u32 >> 2:u32 ^ T_acc_228:u32 in
            T_acc_228:u32 >> 1:u32 ^ T_acc_228:u32)
R_SF:bool = high:bool(R_EBX_32:u32)
R_ZF:bool = 0:u32 == R_EBX_32:u32
addr 0x804849c @asm "call   0x0000000008048330"
label pc_0x804849c
R_ESP_32:u32 = R_ESP_32:u32 - 4:u32
mem32:u32?u8 = mem32:u32?u8 with [R_ESP_32:u32, e_little]:u32 = 0x80484a1:u32
jmp 0x8048330:u32 @str "call"
addr 0x80484a1 @asm "add    $0x8,%esp"
label pc_0x80484a1
T_t1_229:u32 = R_ESP_32:u32
T_t2_230:u32 = 8:u32
R_ESP_32:u32 = R_ESP_32:u32 + T_t2_230:u32
R_CF:bool = R_ESP_32:u32 < T_t1_229:u32
R_OF:bool =
  high:bool((T_t1_229:u32 ^ ~T_t2_230:u32) & (T_t1_229:u32 ^ R_ESP_32:u32))
R_AF:bool =
  0x10:u32 == (0x10:u32 & (R_ESP_32:u32 ^ T_t1_229:u32 ^ T_t2_230:u32))
R_PF:bool =
  ~low:bool(let T_acc_231:u32 := R_ESP_32:u32 >> 4:u32 ^ R_ESP_32:u32 in
            let T_acc_231:u32 := T_acc_231:u32 >> 2:u32 ^ T_acc_231:u32 in
            T_acc_231:u32 >> 1:u32 ^ T_acc_231:u32)
R_SF:bool = high:bool(R_ESP_32:u32)
R_ZF:bool = 0:u32 == R_ESP_32:u32
addr 0x80484a4 @asm "pop    %ebx"
label pc_0x80484a4
R_EBX_32:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
addr 0x80484a5 @asm "ret    "
label pc_0x80484a5
T_ra_232:u32 = mem32:u32?u8[R_ESP_32:u32, e_little]:u32
R_ESP_32:u32 = R_ESP_32:u32 + 4:u32
jmp T_ra_232:u32 @str "ret"


