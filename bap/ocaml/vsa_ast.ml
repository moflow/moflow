(** Value-Set Analysis / Value-Set Arithmetic

    See Gogul Balakrishnan's thesis at
    http://pages.cs.wisc.edu/~bgogul/Research/Thesis/thesis.html

    TODO:
    * Alternate memstore implementation
    * Add a real interface; automatically call simplify_cond
    * Big int support
    * Idea: Use copy propagation information to maintain equivalence
      classes, and use intersection over equivalence class members at
      edge transfer
    * Partial/overlapping memory
    * Special case memory writes of Top: since we will be removing
      entries, we do not have to iterate through all addresses
    * Unified interface to Arithmetic for singleton values
    * Strided-interval aware applicative data type:
      It would store values as strided intervals, rather than
      individual points.
*)

module VM = Var.VarMap

open Big_int_convenience
open Big_int_Z
open Util
open Type
open Ast
open Vsa

module D = Debug.Make(struct let name = "Vsa_ast" and default=`NoDebug end)
open D
module DV = Debug.Make(struct let name = "VsaVerbose_ast" and default=`NoDebug end)

(* A default stack pointer to put in options so that we can verify the
   user actually changed it to a real one *)
let default_sp = Var.newvar "default_sp" reg_1;;

(* Treat unsigned comparisons the same as signed: should be okay as
   long as overflow does not occur. Should be false for soundness. *)
let signedness_hack = ref true

let bits_of_exp e = bits_of_width (Typecheck.infer_ast e)

module SI = SI
module VS = VS
module MemStore = MemStore
module AbsEnv = AbsEnv
type options = Vsa.options

(** This does most of VSA, except the loop handling and special dataflow *)
module AlmostVSA =
struct
  module DFP =
  struct
    module CFG = Cfg.AST
    module L =
    struct
      type t = AbsEnv.t option
      let top = None
      let equal = BatOption.eq ~eq:AbsEnv.equal
      let meet (x:t) (y:t) =
        if equal x y then x
        else match x, y with
        | None, None -> None
        | (Some _ as s), None
        | None, (Some _ as s) -> s
        | Some x, Some y ->
          Some (VM.merge
                  (fun k v1 v2 -> match v1, v2 with
                  | Some (`Scalar a), Some (`Scalar b) -> Some(`Scalar(VS.union a b ))
                  | Some (`Array a), Some (`Array b) -> Some(`Array(MemStore.union a b))
                  | Some (`Scalar _), Some (`Array _)
                  | Some (`Array _), Some (`Scalar _) -> failwith "Tried to meet scalar and array"
                  | Some (`Scalar a), None
                  | None, Some (`Scalar a) ->
                    (* Defined on one side; top on the other -> top *)
                    Some (`Scalar (VS.top (VS.width a)))
                  | Some (`Array a), None
                  | None, Some (`Array a) ->
                    (* Defined on one side; top on the other -> top *)
                    Some (`Array MemStore.top)
                  | None, None -> None) x y)
      let widen (x:t) (y:t) =
        if equal x y then x
        else match x, y with
        | None, None -> None
        | (Some _ as s), None
        | None, (Some _ as s) -> s
        | Some x, Some y ->
          Some (VM.merge
                  (fun k v1 v2 -> match v1, v2 with
                  | Some (`Scalar a), Some (`Scalar b) -> dprintf "widening %s" (Pp.var_to_string k); Some(`Scalar(VS.widen a b ))
                  | Some (`Array a), Some (`Array b) -> dprintf "widening %s" (Pp.var_to_string k); Some(`Array(MemStore.widen a b))
                  | Some (`Scalar _), Some (`Array _)
                  | Some (`Array _), Some (`Scalar _) -> failwith "Tried to widen scalar and array"
                  | Some (`Scalar a), None
                  | None, Some (`Scalar a) ->
                    (* Defined on one side; top on the other -> top *)
                    Some (`Scalar (VS.top (VS.width a)))
                  | Some (`Array a), None
                  | None, Some (`Array a) ->
                    (* Defined on one side; top on the other -> top *)
                    Some (`Array MemStore.top)
                  | None, None -> None) x y)

(*      let widen x y =
        let v = widen x y in
        print_string "x\n";
        AbsEnv.pp print_string x;
        print_string "\ny\n";
        AbsEnv.pp print_string y;
        print_string "\nwiden\n";
        AbsEnv.pp print_string v;
        print_string "\n";
        v *) 
    end
    (* VSA optional interface: specify a "real" memory read function *)
    module O = struct
      type t = options
      let default = { initial_mem = [];
                      (* pick something that doesn't make sense so we
                         can make sure the user changed it later *)
                      sp = default_sp;
                      mem = default_sp;
                    }
    end

    let s0 _ _ = CFG.G.V.create Cfg.BB_Entry

    (** Creates a lattice element that maps each of the given variables to
        it's own region. (For use as an inital value in the dataflow problem.)
    *)
    let init_vars vars =
      List.fold_left (fun vm x -> VM.add x (`Scalar [(x, SI.zero (bits_of_width (Var.typ x)))]) vm) AbsEnv.empty vars

    let init_mem vm {initial_mem; mem} =
      let write_mem m (a,v) =
        DV.dprintf "Writing %#x to %s" (Char.code v) (~% a);
        let v = bi (Char.code v) in
        let index_bits = Typecheck.bits_of_width (Typecheck.index_type_of (Var.typ mem)) in
        let value_bits = Typecheck.bits_of_width (Typecheck.value_type_of (Var.typ mem)) in
        if value_bits <> 8
        then failwith "VSA assumes memory is byte addressable";
        MemStore.write 8 m (VS.single index_bits a) (VS.single 8 v)
      in
      let m = List.fold_left write_mem (MemStore.top) initial_mem in
      if Var.equal mem default_sp
      then failwith "Vsa: Non-default memory must be provided";
      VM.add mem (`Array m) vm

    let init ({sp} as o) g : L.t =
      if Var.equal sp default_sp
      then failwith "Vsa: Non-default stack pointer must be given";
      let vm = init_vars [sp] in
      Some(init_mem vm o)

    let dir _ = GraphDataflow.Forward

    let find v l = VM.find v l
    let do_find = AbsEnv.do_find_vs
    let do_find_ae = AbsEnv.do_find_ae

    (* aev = abstract environment value *)
    let rec exp2vs ?o l e =
      match exp2aev ?o l e with
      | `Scalar vs -> vs
      | _ -> failwith "exp2vs: Expected scalar"
    and exp2aev ?o l e : AbsEnv.value =
      match Typecheck.infer_ast e with
      | Reg nbits -> (
        let new_vs = try (match e with
          | Int(i,t)->
            VS.of_bap_int i t
          | Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)")
          | Var v -> do_find l v
          | BinOp(op, x, y) ->
            let f = VS.binop_to_vs_function op in
            let k = bits_of_exp x in
            f k (exp2vs ?o l x) (exp2vs ?o l y)
          | UnOp(op, x) ->
            let f = VS.unop_to_vs_function op in
            let k = bits_of_exp x in
            f k (exp2vs ?o l x)
          | Load(Var m, i, _e, t) ->
            (* FIXME: assumes deendianized.
               ie: _e and _t should be the same for all loads and
               stores of m. *)
            DV.dprintf "doing a read from %s" (VS.to_string (exp2vs ?o l i));
            MemStore.read (bits_of_width t) ?o (do_find_ae l m) (exp2vs ?o l i)
          | Cast (ct, t, x) ->
            let f = VS.cast_to_vs_function ct in
            let k = bits_of_width t in
            f k (exp2vs ?o l x)
          | Load _ | Concat _ | Extract _ | Ite _ | Unknown _ | Let _ | Store _ ->
            raise(Unimplemented "unimplemented expression type"))
          with Unimplemented s | Invalid_argument s -> DV.dprintf "unimplemented %s %s!" s (Pp.ast_exp_to_string e); VS.top nbits
        in `Scalar new_vs
      )
      | TMem _ | Array _ -> (
        let new_vs = try (match e with
          | Var v ->
            do_find_ae l v
          | Store(Var m,i,v,_e,t) ->
                    (* FIXME: assumes deendianized.
                       ie: _e and _t should be the same for all loads and
                       stores of m. *)
            DV.dprintf "doing a write... to %s of %s." (VS.to_string (exp2vs ?o l i)) (VS.to_string (exp2vs ?o l v));
            (* dprintf "size %#Lx" (VS.numconcrete (exp2vs ?o l i)); *)
            MemStore.write (bits_of_width t)  (do_find_ae l m) (exp2vs ?o l i) (exp2vs ?o l v)
          | _ ->
            raise(Unimplemented "unimplemented memory expression type"))
          with Unimplemented _ | Invalid_argument _ -> MemStore.top
        in `Array new_vs
      )

    let get_map = function
      | Some l -> l
      | None -> failwith "Unable to get absenv; this should be impossible!"

    let rec stmt_transfer_function o _ _ s l =
      dprintf "Executing %s" (Pp.ast_stmt_to_string s);
      match s with
        | Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
        | Assert _ | Assume _ | Jmp _ | CJmp _ | Label _ | Comment _
        | Halt _ ->
            l
        | Special _ -> L.top
        | Move(v, e, _) ->
          let l = get_map l in
          try
            let new_vs = exp2aev ~o l e in
            if DV.debug () then
            (match new_vs with
            | `Scalar new_vs ->
              DV.dprintf "Assign %s <- %s" (Pp.var_to_string v) (VS.to_string new_vs)
            | _ -> ());
            Some (VM.add v new_vs l)
          with Invalid_argument _ | Not_found ->
            Some l

    let edge_transfer_function o g edge _ l =
      dprintf "edge from %s to %s" (Cfg_ast.v2s (Cfg.AST.G.E.src edge)) (Cfg_ast.v2s (Cfg.AST.G.E.dst edge));
      let l = get_map l in
      let accept_signed_bop bop =
        match !signedness_hack, bop with
        | false, (SLE|SLT) -> true
        | true, (SLE|SLT|LE|LT) -> true
        | _, _ -> false
      in
      let l = match CFG.G.E.label edge with
      (* Because strided intervals represent signed numbers, we
         cannot convert unsigned inequalities to strided intervals (try
         it). *)
      | Some(_, BinOp(EQ, (BinOp((SLE|SLT|LE|LT) as bop, Var v, Int(i, t)) as be), Int(i', t')))
      | Some(_, BinOp(EQ, (BinOp((SLE|SLT|LE|LT) as bop, Int(i, t), Var v) as be), Int(i', t')))
          when accept_signed_bop bop ->

        let dir = match be with
          | BinOp(_, Var _, Int _) -> `Below
          | BinOp(_, Int _, Var _) -> `Above
          | _ -> failwith "impossible"
        in

        (* Reverse if needed *)
        let e, dir, bop =
          if bi_is_one i' then be, dir, bop
          else
            let newbop = match bop with
              | SLE -> SLT
              | SLT -> SLE
              | LE -> LT
              | LT -> LE
              | _ -> failwith "impossible"
            in
            match dir with
            | `Below -> BinOp(newbop, Int(i, t), Var v), `Above, newbop
            | `Above -> BinOp(newbop, Var v, Int(i, t)), `Below, newbop
        in
        let vsf = match dir, bop with
          | `Below, SLE -> VS.beloweq
          | `Below, LE -> VS.beloweq_unsigned
          | `Below, SLT -> VS.below
          | `Below, LT -> VS.below_unsigned
          | `Above, SLE -> VS.aboveeq
          | `Above, LE -> VS.aboveeq_unsigned
          | `Above, SLT -> VS.above
          | `Above, LT -> VS.above_unsigned
          | _ -> failwith "impossible"
        in
        let vs_v = do_find l v in
        let vs_c = vsf (bits_of_width t) i in
        let vs_int = VS.intersection vs_v vs_c in
        dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.var_to_string v) (Cfg_ast.v2s (CFG.G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int);
        VM.add v (`Scalar vs_int) l
      | Some(_, BinOp(EQ, (BinOp((SLE|SLT|LE|LT) as bop, (Load(Var m, ind, _e, t) as le), Int(i, t')) as be), Int(i', t'')))
      | Some(_, BinOp(EQ, (BinOp((SLE|SLT|LE|LT) as bop, Int(i, t'), (Load(Var m, ind, _e, t) as le)) as be), Int(i', t'')))
          when accept_signed_bop bop ->
        let dir = match be with
          | BinOp(_, Load _, Int _) -> `Below
          | BinOp(_, Int _, Load _) -> `Above
          | _ -> failwith "impossible"
        in

        (* Reverse if needed *)
        let e, dir, bop =
          if bi_is_one i' then be, dir, bop
          else
            let newbop = match bop with
              | SLE -> SLT
              | SLT -> SLE
              | LT -> LE
              | LE -> LT
              | _ -> failwith "impossible"
            in
            match dir with
            | `Below -> BinOp(newbop, Int(i, t), Load(Var m, ind, _e, t)), `Above, newbop
            | `Above -> BinOp(newbop, Load(Var m, ind, _e, t), Int(i, t)), `Below, newbop
        in
        let vsf = match dir, bop with
          | `Below, SLE -> VS.beloweq
          | `Below, LE -> VS.beloweq_unsigned
          | `Below, SLT -> VS.below
          | `Below, LT -> VS.below_unsigned
          | `Above, SLE -> VS.aboveeq
          | `Above, LE -> VS.aboveeq_unsigned
          | `Above, SLT -> VS.above
          | `Above, LT -> VS.above_unsigned
          | _ -> failwith "impossible"
        in
        let vs_v = exp2vs ~o l le in
        let vs_c = vsf (bits_of_width t) i in
        let vs_int = VS.intersection vs_v vs_c in
        dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.ast_exp_to_string le) (Cfg_ast.v2s (CFG.G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int);
        let orig_mem = do_find_ae l m in
        let new_mem = MemStore.write_intersection (bits_of_width t) orig_mem (exp2vs l ind) vs_int in
        VM.add m (`Array new_mem) l
      | Some(_, BinOp(EQ, (BinOp(EQ|NEQ as bop, Var v, Int(i, t))), Int(i', t')))
      | Some(_, BinOp(EQ, (BinOp(EQ|NEQ as bop, Int(i, t), Var v)), Int(i', t'))) ->

        (* We can make a SI for equality, but not for not for
           inequality *)
        let vs_c =
          let s = VS.of_bap_int i t in
          match bop with
          | EQ when i' = bi1 -> s
          | NEQ when i' = bi0 -> s
          | _ -> VS.top (bits_of_width t)
        in

        let vs_v = do_find l v in
        let vs_int = VS.intersection vs_v vs_c in
        dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.var_to_string v) (Cfg_ast.v2s (CFG.G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int);
        VM.add v (`Scalar vs_int) l

      | Some(_, BinOp((SLT|SLE), Var v2, Var v1)) ->
        (* XXX: Can we do something different for SLT? *)
        let vs_v1 = do_find l v1
        and vs_v2 = do_find l v2 in
        let vs_lb = VS.remove_upper_bound vs_v2
        and vs_ub = VS.remove_lower_bound vs_v1 in
        let vs_v1 = VS.intersection vs_v1 vs_lb
        and vs_v2 = VS.intersection vs_v2 vs_ub in
        let l = VM.add v1 (`Scalar vs_v1) l in
        VM.add v2 (`Scalar vs_v2) l
      | Some(_, e) -> dprintf "no edge match %s" (Pp.ast_exp_to_string e); l
      | _ -> l
      in Some l

  end

  module DF = CfgDataflow.MakeWide(DFP)

end

let exp2vs = AlmostVSA.DFP.exp2vs ?o:None

(* Main vsa interface *)
let vsa ?nmeets opts g =
  Checks.connected_astcfg g "VSA";
  AlmostVSA.DF.worklist_iterate_widen_stmt ?nmeets ~opts g

let last_loc = AlmostVSA.DF.last_loc

let build_default_arch_options arch =
  {
    initial_mem = [];
    sp=Arch.sp_of_arch arch;
    mem=Arch.mem_of_arch arch;
  }

let build_default_prog_options asmp =
  let x = build_default_arch_options (Asmir.get_asmprogram_arch asmp) in
  { x with initial_mem=Asmir.get_readable_mem_contents_list asmp
  }
