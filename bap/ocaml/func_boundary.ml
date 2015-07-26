open Big_int_convenience

module D = Debug.Make(struct let name = "Func_boundary" and default=`NoDebug end)
open D

(* This function checks to see if the sequence of assembly instructions
   at start_addr ends at end_addr. *)
let rec liftable_asm_addr_to_bap p start_addr end_addr =
  if start_addr >% end_addr then false
  else if start_addr ==% end_addr then true
  else
    let (_, next) = Asmir.asm_addr_to_bap p start_addr in
    liftable_asm_addr_to_bap p next end_addr

(* check if str appears inside ir *)
let check_appears ir str =
  let label_asm_startswith_str = function
  | Ast.Label (_, attrs) -> 
    let asm_startswith_str = function
    | Type.Asm s -> let strlen = String.length str in (* faking String.starts_with *)
               (strlen <= String.length s) && (String.sub s 0 strlen = str)
    | _ -> false
    in
    List.exists asm_startswith_str attrs 
  | _ -> false
  in
  List.exists label_asm_startswith_str ir

(* This function checks to see if the instruction is to call
   get_pc_thunk. call get_pc_thunk cannot be function exit, so if this
   function returns true, we need to continue looking back.  Note that
   l is a list of probable get_pc_thunk address. Some binary implements
   get_pc_thunk internally, so we may have a list of address after
   pattern filtering. The one called through instructions should be the
   actual one. We will mark the one and kick out others. This is the
   reason why list l should be mutable. *)
let check_call_get_pc_thunk ir l =
  dprintf "check get_pc_thunk";
  let label_asm_callpcthunk = function
    | Ast.Label (label, attrs) ->
      let asm_callpcthunk = function      
        | Type.Asm s ->
          dprintf "disassemble: %s" s;
          String.length s > 4 && String.sub s 0 4 = "call" 
          &&
            (
              let addr = Scanf.sscanf s "call 0x%Lx" (fun x -> x) in
              let addr = bi64 addr in
              List.exists (fun (a, v) -> if a = addr then (v := true; dprintf "address %s ensured" (~% a)); a = addr) !l )
        | _ -> false
      in List.exists asm_callpcthunk attrs
    | _ -> false
  in List.exists label_asm_callpcthunk ir

(* This function is to generate a list of probable __i686.get_pc_thunk.bx/cx/dx list. *)
let get_pc_thunk_list addr_hex_list =
  let rec filter acc = function 
    | [] -> List.rev acc (* rev is optional *)
    (* __i686.get_pc_thunk.cx: 8b 0c 24 c3: mov (%esp) %ecx, ret*)
    | (addr, 0x8b) :: (_, 0x0c) :: (_, 0x24) :: (_, 0xc3) :: rest
    (* __i686.get_pc_thunk.bx: 8b 1c 24 c3: mov (%esp) %ebx, ret*)
    | (addr, 0x8b) :: (_, 0x1c) :: (_, 0x24) :: (_, 0xc3) :: rest 
    (* __i686.get_pc_thunk.dx: 8b 1c 24 c3: mov (%esp) %edx, ret*)
    | (addr, 0x8b) :: (_, 0x14) :: (_, 0x24) :: (_, 0xc3) :: rest  ->
      dprintf "add %s into get_pc_thunk address list" (~% addr);
      filter ((addr, ref false) :: acc) rest
    | _::rest -> filter acc rest
  in
  filter [] addr_hex_list

(* This function is called externally to output a list of start address. *)   
let start_addresses p =
  if Asmir.get_asmprogram_arch p <> Asmir.arch_i386 then raise (Invalid_argument "Function boundary identification only supported on x86");
  dprintf "get addr_hex_list...";
  let addr_hex_list = 
    let addr_char_list = Asmir.get_exec_mem_contents_list p in
    List.rev (List.rev_map (fun (a, c) -> (a, Char.code c)) addr_char_list) 
  in
  dprintf "find candidate list...";
  let pcthunk_addr_v_list = ref (get_pc_thunk_list addr_hex_list) in
  (* if List.length pcthunk_addr_v_list = 1 then pc := List.hd pcthunk_addr_v_list; *)
  List.iter (fun (x, _) -> dprintf "pc thunk candidate: %s" (~% x)) !pcthunk_addr_v_list;
(* i: the number of bytes that look back,
   e_index: the index of byte that look back,
   last_start_addr: last function's start address, a weak minimum boundary for current function start address. *)

(* This function is to find the real entrance of a function. Given a
   position of push ebp / sub esp, it identifies the start address by
   finding the end of previous function, assuming that function is
   followed by the end instruction of prior function such as ret or nop
   or jmp or call. *)
  let rec back i e_index last_start_addr =
    let end_addr = fst (List.nth addr_hex_list e_index) in
    if i > e_index then Some end_addr 
    else 
      let start_addr = fst (List.nth addr_hex_list (e_index-i)) in
      if start_addr <= last_start_addr then None
      else if end_addr -% start_addr > bi 30 then None
      else if i = e_index then Some start_addr
      else try
        let _ = dprintf "look from %s to %s" (~% start_addr) (~% end_addr) in 
        (*if end_addr >= Int64.of_int 0x80da9f0 then *)
        (* let line = input_line stdin in
        dprintf "%s" line; *)
        let (ir, n) = Asmir.asm_addr_to_bap p start_addr in
        (* let line = input_line stdin in
        dprintf "%s" line; *)
        if liftable_asm_addr_to_bap p n end_addr then
          if check_call_get_pc_thunk ir pcthunk_addr_v_list then back (i+1) e_index last_start_addr
          else(
            dprintf "check mnemonics";
            if List.exists (check_appears ir) ["ret"; "nop"; "jmp"; "call"; "lea    0x0(%edi,%eiz,1),%edi"; "lea    0x0(%esi,%eiz,1),%esi"; "lea    0x0(%esi),%esi"; "lea    0x0(%edi),%edi"] then Some n
            else back (i+1) e_index last_start_addr
          )
        else back (i+1) e_index last_start_addr
      with _ -> back (i+1) e_index last_start_addr
  in
  let exclue_0x55 l = 
   not (List.exists (fun x -> x = 0x55) l) in
  let rec filter acc l index last_start_addr =
    let jump_to rest len =
      let entrance = back 1 index last_start_addr in
      match entrance with
        | Some addr -> (dprintf "%s is start address" (~% addr); filter (addr::acc) rest (index + len) addr)
        | _ -> filter acc rest (index + len) last_start_addr
    in
    (* let _ = match l with
    | (addr, x) :: rest -> dprintf "Filtering %Lx, %x" addr x
    | [] -> () in *)
    match l with
    (* prolog: 55 89 e5: push %ebp, mov %esp %ebp*)
    (*   807bdf7: 8b 55 d0              mov    -0x30(%ebp),%edx
         807bdfa: e9 1a ff ff ff        jmp    807bd19 <ExaCheckPutImage+0x109>
         807bdff: 90                    nop
        +807be00 <main>:
         807be00: 55                    push   %ebp
         807be01: 89 e5                 mov    %esp,%ebp
         Here 0x55 at 807bdf8 will be filtered with 89 e5 in 807be01. This should not be filtered, so we need a new condition: no 0x55 occured in 0x55 ... 0x89 0xe5
    *)
    | (addr, 0x55)::(_, 0x89)::(_, 0xe5) :: rest -> (dprintf "%s hitts push ebp" (~% addr); jump_to rest 3)
    | (addr, 0x55)::(_, b1)::(_, 0x89)::(_, 0xe5) :: rest when b1 <> 0x55 -> (dprintf "%s hitts push ebp" (~% addr); jump_to rest 4)
    | (addr, 0x55)::(_, b1)::(_, b2)::(_, 0x89)::(_, 0xe5)::rest when exclue_0x55 [b1;b2]-> (dprintf "%s hitts push ebp" (~% addr) ;jump_to rest 5)
    | (addr, 0x55)::(_, b1)::(_, b2)::(_, b3)::(_, 0x89)::(_, 0xe5)::rest when exclue_0x55 [b1;b2;b3]-> (dprintf "%s hitts push ebp" (~% addr); jump_to rest 6)
    | (addr, 0x55)::(_, b1)::(_, b2)::(_, b3)::(_, b4)::(_, 0x89)::(_, 0xe5)::rest when exclue_0x55 [b1;b2;b3;b4]-> (dprintf "%s hitts push ebp" (~% addr); jump_to rest 7)
    | (addr, 0x55)::(_, b1)::(_, b2)::(_, b3)::(_, b4)::(_, b5)::(_, 0x89)::(_, 0xe5)::rest when exclue_0x55 [b1;b2;b3;b4;b5]-> (dprintf "%s hitts push ebp" (~% addr); jump_to rest 8)
    | (addr, 0x55)::(_, b1)::(_, b2)::(_, b3)::(_, b4)::(_, b5)::(_, b6)::(_, 0x89)::(_, 0xe5)::rest when exclue_0x55 [b1;b2;b3;b4;b5;b6]-> (dprintf "%s hitts push ebp" (~% addr); jump_to rest 9)
    | (addr, 0x55)::(_, b1)::(_, b2)::(_, b3)::(_, b4)::(_, b5)::(_, b6)::(_, b7)::(_, b8)::(_, b9)::(_, b10)::(_, b11)::(_, 0x89)::(_, 0xe5)::rest when exclue_0x55 [b1;b2;b3;b4;b5;b6;b7;b8;b9;b10;b11] -> (dprintf "%s hitts push ebp" (~% addr); jump_to rest 14)
    
    (* another prolog: 83 ec or 81 ec : sub xxx %esp*)
    | (addr, 0x81) :: (_, 0xec) :: rest | (addr, 0x83) :: (_, 0xec) :: rest -> (dprintf "%s sub" (~% addr); jump_to rest 2)
   
    (* __libc_csu_fini, started with f3 c3: repz ret *)
    | (addr, 0xf3)::(_, 0xc3)::rest
    (* _start, started with 31 ed: xor %ebp %ebp, pop %esi *)
    | (addr, 0x31) :: (_, 0xed) :: rest ->
      let _ = dprintf "add %s into start address list" (~% addr) in
      filter (addr :: acc) rest (index + 2) addr

    (* __lib_csu_init, started with 55 57 56: push %ebp, push %edi, push %esi *)
    | (addr, 0x55) :: (_, 0x57) :: (_, 0x56) :: rest ->
      let _ = dprintf "add %s into start address list" (~% addr) in
      filter (addr :: acc) rest (index + 3) addr

    (* atexit, started with 53 e8 ec ff ff ff: push %bx, call __i686.get_pc_thunk.bx *)
    | (addr, 0x53) :: (_, 0xe8) :: (_, 0xec) :: (_, 0xff) :: (_, 0xff) :: (_, 0xff) :: rest ->
      let _ = dprintf "add %s into start address list" (~% addr) in
      filter (addr :: acc) rest (index + 6) addr

    | first :: rest -> filter acc rest (index + 1) last_start_addr 
    | [] -> List.rev acc
  in
  let start_address_list = 
    let exclude_get_pc_list = filter [] addr_hex_list 0 bi0 in
    let pcthunk_addr_list = 
      if List.length !pcthunk_addr_v_list = 1 then [fst(List.hd !pcthunk_addr_v_list)] 
      else List.map (fst) (List.filter (fun (a, v) -> !v) !pcthunk_addr_v_list)
    in
    exclude_get_pc_list @ pcthunk_addr_list 
  in
  List.iter (fun addr -> dprintf "%s" (~% addr)) start_address_list;
  start_address_list

(*This function is to get function boundaries for general binaries*)
let get_function_ranges p =
  let open Libbfd in
  let open Libasmir in
  let open Asmir in
  let open Asmir_consts in
  let starts =
    try (
      let symb = get_symbols p in
      let is_function = match Asmir.get_flavour p with
        | Bfd_target_elf_flavour
        | Bfd_target_coff_flavour ->
          (fun s -> s.bfd_symbol_flags land (bsf_function lor bsf_gnu_indirect_function) <> 0)
        | Bfd_target_mach_o_flavour ->
          (fun s -> dprintf "Symbol %s, flags=%#x" s.bfd_symbol_name s.bfd_symbol_flags;
            s.bfd_symbol_flags land bsf_global <> 0)
        | _ ->
          wprintf "Unknown file format flavour.  Assuming it has a function flag for symbols, which may be incorrect.";
          (fun s -> s.bfd_symbol_flags land (bsf_function lor bsf_gnu_indirect_function) <> 0)
      and symb_to_tuple s =
        (* FIXME: section_end doesn't seem to get the right values... *)
        (* did this fix it? --aij *)
        let sec = s.bfd_symbol_section in
        let vma = bfd_section_get_vma sec in
        (bi64 (Int64.add s.bfd_symbol_value vma),
         bi64 (Int64.add vma (bfd_section_get_size sec)),
         s.bfd_symbol_name)
      in
      let starts =
        Array.fold_left
          (fun l s -> if is_function s then symb_to_tuple s :: l else l)
          [] symb
      in
      starts
    )
    with _ ->
      let n = ref 0 in
      (* XXX: Ugly hack: we only use the end address for the last symbol *)
      let end_address = match List.rev (get_exec_mem_contents_list p) with
        | (a, _)::_ -> a
        | _ -> bim1
      in
      List.map (fun a -> incr n;
        (a, end_address, "unknown_"^(string_of_int !n)) ) (start_addresses p)
  in
  let starts = Array.of_list starts in
  (* FIXME: probably should do unsigned comparison *)
  let () = Array.fast_sort compare starts in
  let ranges = Array.mapi
    (fun i (s,e,name) ->
       let e' =
         try let (s,_,_) = starts.(i+1) in s
         with Invalid_argument "index out of bounds" -> e
       in
       (name,s,e') (* section_end doesn't work *)
    ) starts
  in
  let unfiltered = Array.to_list ranges in
  (* filter out functions that start at 0 *)
  List.filter (function
                 |(s,i,_) when i = bi0 -> false
                 |("_init",_,_) -> false
                 | _ -> true)
    unfiltered

let post_process cfg =
  let cfg_t = Hacks.ast_remove_indirect cfg in
  let cfg_t = Ast_cond_simplify.simplifycond_cfg cfg_t in
  let cfg_t = Prune_unreachable.prune_unreachable_ast cfg_t in
  cfg_t

let end_address_at cfg =
  Cfg.AST.G.fold_vertex (fun v max ->
    let stmts = Cfg.AST.get_stmts cfg v in
    List.fold_left (fun m -> function
      | Ast.Label(Type.Addr l, _) -> if l >= m then l else m
      | _ -> m
    ) max stmts
  ) cfg bi0
