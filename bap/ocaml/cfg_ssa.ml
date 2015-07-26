(*
    Static Single Assignment translation

    @author Ivan Jager
*)

open Big_int_Z
open Big_int_convenience
open Util
open Ssa
open Cfg
open Type
open BatListFull

module D = Debug.Make(struct let name = "Cfg_ssa" and default=`NoDebug end)
open D

module VH = Var.VarHash
module C = Cfg.SSA
module BH = Cfg.BH
module CA = Cfg.AST
module Dom = Dominator.Make(C.G)
open Var

let v2s n = bbid_to_string (C.G.V.label n)

(* A translation context (for translating to SSA) *)
module Ctx =
struct
  type t = var VH.t * aststmtloc VH.t * var VH.t * (var*var) Stack.t Stack.t

  let create() = (VH.create 570, VH.create 570, VH.create 570, Stack.create())

  let copy (vh,to_oldloc,to_oldvar,stacks) =
    VH.copy vh, VH.copy to_oldloc, VH.copy to_oldvar, Stack.copy stacks

  let lookup (vh,_,_,_) var =
    try VH.find vh var
    with Not_found -> var

  let extend (vh,to_oldloc,to_oldvar,stacks) v v' loc =
    Stack.push (v,v') (Stack.top stacks);
    VH.add vh v v';
    VH.add to_oldvar v' v;
    BatOption.may (VH.add to_oldloc v') loc

  (* Called to add a let variable to the context *)
  let letextend (vh,_,_,_) v v' =
    VH.add vh v v'
      (* FIXME: We didn't used to add these to to_oldvar, do we want to now? *)
      (* VH.add to_oldvar v' v *)

  (* Called to remove a let variable from the context *)
  let letunextend (vh,_,_,_) v =
    VH.remove vh v

  let push (_,_,_,stacks) =
    Stack.push (Stack.create()) stacks

  let pop (vh,_,_,stacks) =
    let myvars = Stack.pop stacks in
    Stack.iter (fun (v,_) -> VH.remove vh v) myvars
end

let type_of_exp = Typecheck.infer_ssa

(* share the strings in the variable names we create, to save memory *)
let ssa_temp_name = "T_temp"

(* @return a reversed list of SSA stmts and an exp that is equivalent to
   the Ast expression *)
let rec exp2ssa (ctx:Ctx.t) ~(revstmts:stmt list) ?(attrs=[]) e : stmt list * exp =
  let exp2ssa = exp2ssa ctx ~attrs in
  match e with
  | Ast.Ite(b, e1, e2) ->
      let (revstmts, vb) = exp2ssa ~revstmts b in
      let (revstmts, v1) = exp2ssa ~revstmts e1 in
      let (revstmts, v2) = exp2ssa ~revstmts e2 in
      (revstmts, Ite(vb, v1, v2))
  | Ast.Extract(h, l, e) ->
      let (revstmts, ve) = exp2ssa ~revstmts e in
      (revstmts, Extract(h, l, ve))
  | Ast.Concat(le, re) ->
      let (revstmts, lv) = exp2ssa ~revstmts le in
      let (revstmts, rv) = exp2ssa ~revstmts re in
      (revstmts, Concat(lv, rv))
  | Ast.BinOp(op, e1, e2) ->
      let (revstmts, v1) = exp2ssa ~revstmts e1 in
      let (revstmts, v2) = exp2ssa ~revstmts e2 in
      (revstmts, BinOp(op,v1,v2))
  | Ast.UnOp(op, e1) ->
      let (revstmts, v1) = exp2ssa ~revstmts e1 in
      (revstmts, UnOp(op,v1))
  | Ast.Int(i, t) ->
      (revstmts, Int(i,t))
  | Ast.Lab s ->
      (revstmts, Lab s)
  | Ast.Var name ->
      (revstmts, Var(Ctx.lookup ctx name))
  | Ast.Load(arr,idx,endian, t) ->
      let (revstmts,arr) = exp2ssa ~revstmts arr in
      let (revstmts,idx) = exp2ssa ~revstmts idx in
      let (revstmts,endian) = exp2ssa ~revstmts endian in
      (revstmts, Load(arr,idx,endian,t))
  | Ast.Store(arr,idx,vl, endian, t) ->
      let (revstmts,arr) = exp2ssa ~revstmts arr in
      let (revstmts,idx) = exp2ssa ~revstmts idx in
      let (revstmts,vl) = exp2ssa ~revstmts vl in
      let (revstmts,endian) = exp2ssa ~revstmts endian in
      (revstmts, Store(arr,idx,vl, endian,t))
  | Ast.Cast(ct,t,e1) ->
      let (revstmts, v1) = exp2ssa ~revstmts e1 in
      (revstmts, Cast(ct,t,v1))
  | Ast.Unknown(s,t) ->
      (revstmts, Unknown(s,t))
  | Ast.Let(v, e1, e2) ->
      let v' = Var.renewvar v in
      let (revstmts,e1) = exp2ssa ~revstmts e1 in
      let revstmts = Move(v',e1, attrs)::revstmts in
      Ctx.letextend ctx v v';
      let (revstmts,e2) = exp2ssa ~revstmts e2 in
      Ctx.letunextend ctx v;
      (revstmts, e2)


(* @return a reversed list of SSA stmts *)
let rec stmt2ssa ctx ctxmap ~(revstmts: stmt list) loc s =
  let f () =
    let exp2ssa = exp2ssa ctx in
    match s with
      Ast.Jmp(e1, a) ->
	let (revstmts,v1) = exp2ssa ~revstmts e1 in
	Jmp(v1, a) :: revstmts
    | Ast.CJmp(e1,e2,e3,a) ->
      let (revstmts,v1) = exp2ssa ~revstmts e1 in
      let (revstmts,v2) = exp2ssa ~revstmts e2 in
      let (revstmts,v3) = exp2ssa ~revstmts e3 in
      CJmp(v1,v2,v3,a) :: revstmts
    | Ast.Move(v, e2, a) ->
      let (revstmts, e) = exp2ssa ~revstmts e2 in
      let nv = Var.renewvar v in
      Ctx.extend ctx v nv (Some loc);
      Move(nv, e, a)::revstmts
    | Ast.Label(label,a) ->
      Label(label,a) :: revstmts
    | Ast.Comment(s,a) ->
      Comment(s,a)::revstmts
    | Ast.Special(s,None,_) ->
      raise (Invalid_argument("SSA: Impossible to handle specials without a defuse They should be replaced with their semantics. Special: "^s))
    | Ast.Special(s,Some du,a) ->
      let duuse = List.map (Ctx.lookup ctx) du.uses in
      let dudef = List.map (fun v ->
        let nv = Var.renewvar v in
        Ctx.extend ctx v nv (Some loc); nv)
      du.defs in
      Special(s, {defs = dudef; uses = duuse}, a)::revstmts
    | Ast.Assert(e,a) ->
      let (revstmts,v) = exp2ssa ~revstmts e in
      Assert(v,a)::revstmts
    | Ast.Assume(e,a) ->
      let (revstmts,v) = exp2ssa ~revstmts e in
      Assume(v,a)::revstmts
    | Ast.Halt(e,a) ->
      let (revstmts,v) = exp2ssa ~revstmts e in
      Halt(v,a)::revstmts
  in
  let o = f () in
  Hashtbl.add ctxmap loc (Ctx.copy ctx);
  o

(* Translates a list of Ast statements that get executed sequentially to SSA. *)
let stmts2ssa ctx ctxmap ?tac bb ss =
  let revstmts,_ =
    List.fold_left
      (fun (rs,n) s ->
        stmt2ssa ctx ctxmap ~revstmts:rs (bb,n) s, n+1) ([],0) ss in
    List.rev revstmts

module ToTac = struct
  let rec exp2tacexp ~(revstmts:stmt list) ?(attrs=[]) e : stmt list * exp =
    let exp2tac = exp2tac ~attrs in
    match e with
    | Ite(b, e1, e2) ->
      let (revstmts, vb) = exp2tac ~revstmts b in
      let (revstmts, v1) = exp2tac ~revstmts e1 in
      let (revstmts, v2) = exp2tac ~revstmts e2 in
      (revstmts, Ite(vb, v1, v2))
    | Extract(h, l, e) ->
      let (revstmts, ve) = exp2tac ~revstmts e in
      (revstmts, Extract(h, l, ve))
    | Concat(le, re) ->
      let (revstmts, lv) = exp2tac ~revstmts le in
      let (revstmts, rv) = exp2tac ~revstmts re in
      (revstmts, Concat(lv, rv))
    | BinOp(op, e1, e2) ->
      let (revstmts, v1) = exp2tac ~revstmts e1 in
      let (revstmts, v2) = exp2tac ~revstmts e2 in
      (revstmts, BinOp(op,v1,v2))
    | UnOp(op, e1) ->
      let (revstmts, v1) = exp2tac ~revstmts e1 in
      (revstmts, UnOp(op,v1))
    | Int(i, t) ->
      (revstmts, Int(i,t))
    | Lab s ->
      (revstmts, Lab s)
    | Var name ->
      (revstmts, Var name)
    | Load(arr,idx,endian, t) ->
      let (revstmts,arr) = exp2tac ~revstmts arr in
      let (revstmts,idx) = exp2tac ~revstmts idx in
      let (revstmts,endian) = exp2tac ~revstmts endian in
      (revstmts, Load(arr,idx,endian,t))
    | Store(arr,idx,vl, endian, t) ->
      let (revstmts,arr) = exp2tac ~revstmts arr in
      let (revstmts,idx) = exp2tac ~revstmts idx in
      let (revstmts,vl) = exp2tac ~revstmts vl in
      let (revstmts,endian) = exp2tac ~revstmts endian in
      (revstmts, Store(arr,idx,vl, endian,t))
    | Cast(ct,t,e1) ->
      let (revstmts, v1) = exp2tac ~revstmts e1 in
      (revstmts, Cast(ct,t,v1))
    | Unknown(s,t) ->
      (revstmts, Unknown(s,t))
    | Phi l ->
      (revstmts, Phi l)

  and exp2tac ~(revstmts:stmt list) ?(attrs=[]) ?(name=ssa_temp_name) e : stmt list * exp =
  (* Make an SSA value for an SSA expression by adding an assignment to
     revstmts if needed *)
    let exp2val (revstmts, exp) =
      match exp with
      | (Var _ | Int _ | Lab _) as e -> (revstmts, e)
      | e ->
	let t = type_of_exp exp in
	let l = Var.newvar name t in
	(Move(l, exp, attrs)::revstmts, Var l)
    in
    exp2val(exp2tacexp ~revstmts ~attrs e)

  let rec stmt2tac ~(revstmts: stmt list) s =
    let exp2tac = exp2tac in
    let exp2tacexp = exp2tacexp in
    match s with
      Jmp(e1, a) ->
        let (revstmts,v1) = exp2tac ~revstmts e1 in
        Jmp(v1, a) :: revstmts
    | CJmp(e1,e2,e3,a) ->
      let (revstmts,v1) = exp2tac ~revstmts e1 in
      let (revstmts,v2) = exp2tac ~revstmts e2 in
      let (revstmts,v3) = exp2tac ~revstmts e3 in
      CJmp(v1,v2,v3,a) :: revstmts
    | Move(v, e2, a) ->
      let (revstmts, e) = exp2tacexp ~revstmts e2 in
      Move(v, e, a)::revstmts
    | Label(label,a) ->
      Label(label,a) :: revstmts
    | Comment(s,a) ->
      Comment(s,a)::revstmts
    | Special(s,du,a) ->
      Special(s,du,a)::revstmts
    | Assert(e,a) ->
      let (revstmts,v) = exp2tac ~revstmts e in
      Assert(v,a)::revstmts
    | Assume(e,a) ->
      let (revstmts,v) = exp2tac ~revstmts e in
      Assume(v,a)::revstmts
    | Halt(e,a) ->
      let (revstmts,v) = exp2tac ~revstmts e in
      Halt(v,a)::revstmts

  let stmts2tac ss =
    let revstmts,_ =
      List.fold_left
        (fun (rs,n) s ->
          stmt2tac ~revstmts:rs s, n+1) ([],0) ss in
    List.rev revstmts

  let reset_edges g =
    C.G.fold_vertex (fun v g ->
      match List.rev (C.get_stmts g v) with
      | CJmp(c, _, _, _)::_ ->
        C.G.fold_succ_e (fun e g ->
          let dst = C.G.E.dst e in
          let cond =
            match C.G.E.label e with
            | Some(b, BinOp(EQ, e1, e2)) ->
              Some(b, BinOp(EQ, c, e2))
            | _ -> failwith "reset_edges: Expected edge conditions"
          in
          let e' = C.G.E.create v cond dst in
          C.add_edge_e (C.remove_edge_e g e) e'
        ) g v g
      | _ -> g
    ) g g

  let ssa2tac g =
    reset_edges (C.G.fold_vertex (fun v g ->
      let stmts = C.get_stmts g v in
      C.set_stmts g v (stmts2tac stmts)
    ) g g)
end

(* This is only for use by trans_cfg, as it has some hacks *)
let defsites cfg =
  let h = VH.create 57
  and globals = ref [] in
  let defs stmts =
    let res = ref [] in
    let f = function
        | Ast.Move(v, _, _) ->  res := v :: !res; globals := v :: !globals
        | _ -> ()
    in
    List.iter f stmts;
    !res
  in
  CA.G.iter_vertex
    (fun b ->
       let id = CA.G.V.label b in
       let vars = list_unique  (defs (CA.get_stmts cfg b)) in
       List.iter (fun v -> VH.add h v id) vars
    )
    cfg;
  (* add globals as being defined at the entry node. We only actually need
     the globals that might conditionally be assigned to. *)
  List.iter (fun v -> VH.add h v BB_Entry) (list_unique !globals);
  (VH.find_all h, !globals)
    (* FIXME: maybe avoiding find_all will make it faster *)


type cfg_translation_results = {
  ssacfg : Cfg.SSA.G.t;
  to_ssaexp: Cfg.aststmtloc -> Ast.exp -> Ssa.exp;
  to_ssavar: Var.t -> Var.t;
  to_astvar: Var.t -> Var.t;
  to_astloc: Var.t -> Cfg.aststmtloc;
}

let rec trans_cfg ?(tac=true) cfg =
  pdebug "Translating to SSA";

  let cfg = Prune_unreachable.prune_unreachable_ast (CA.copy cfg) in
  pdebug "Creating new cfg";
  let ssa, vs2a, es2a = Cfg.map_ast2ssa (fun _ -> []) (fun _ -> exp_true) cfg in
  pdebug "Computing defsites";
  let (defsites, globals) = defsites cfg in
    (* keep track of where we need to insert phis *)
  let phis : (bbid * var, var * var list) Hashtbl.t = Hashtbl.create 57 in
  pdebug "Computing dominators";
  let {Dom.dom_tree=dom_tree; Dom.dom_frontier=df} =
    Dom.compute_all ssa (C.G.V.create BB_Entry)
  in
  let add_phis_for_var v =
    (* Note that contrary to the book, we don't need membership testing at all
       for the worklist, since the only time we try to add a node is when it
       couldn't be in the worklist in the first place. --aij
       Errata sent to Andrew W. Appel <appel@princeton.edu> on 2007-06-10
    *)
    (* let () = dprintf "Adding phis for variable '%s'" (var_to_string v) in *)
    let rec do_work = function
      | [] -> ()
      | n::worklist ->
          let worklist =
            List.fold_left
              (fun toadd y ->
                 let y = C.G.V.label y in (* for now *)
                 if not(Hashtbl.mem phis (y,v))
                 then (Hashtbl.add phis (y,v) (v,[]);
                       if List.mem y (defsites v) then toadd else y::toadd )
                 else toadd
              )
              worklist
              (df (C.G.V.create n))
          in
            do_work worklist
    in
    do_work (defsites v)
  in
  dprintf "Adding phis";
  List.iter add_phis_for_var globals;
  dprintf "Added %d phis" (Hashtbl.length phis);
  (* we now have an entry in phis for every phi expression
     we need to add, although we still don't have the RHS and LHS. *)
  dprintf "Grouping phis by block";
  let blockphis =
    (* returns the phis for a given block *)
    let h = Hashtbl.create 57 in
    Hashtbl.iter (fun (n,v) _ -> Hashtbl.add h n v) phis;
    Hashtbl.find_all h
  in
  let exitctx = VH.create 57 in (* context at end of exit node *)
  let ctxmap = Hashtbl.create 1000 in (* context at various locations *)
  let (vh_ctx,to_oldloc,to_oldvar,stacks) as ctx = Ctx.create() in
  let lookup = Ctx.lookup ctx in
  let extend = Ctx.extend ctx in
  let rec rename_block ssa b =
    let bbid = C.G.V.label b in
    dprintf "Translating block %s" (bbid_to_string bbid);
    let cfgb = CA.G.V.create bbid in
    Ctx.push ctx;
    let () =
      (* create variables for our phis *)
      List.iter
        (fun v ->
           let v' = Var.renewvar v in
           let (v'',vs) = Hashtbl.find phis (bbid,v) in
             assert(v'' == v);
             Hashtbl.replace phis (bbid,v) (v',vs);
	   extend v v' None (* phis have no ast location *)
        )
        (blockphis bbid)
    in
    let ssa =
      (* rename variables *)
      let stmts = CA.get_stmts cfg cfgb in
      dprintf "translating stmts";
      let ssa, stmts' = match List.rev stmts with
        | Ast.CJmp _::_ ->
          let stmts' = stmts2ssa ctx ctxmap cfgb stmts in
          let v = match List.rev stmts' with
            | Ssa.CJmp (v, _, _, _)::_ -> v
            | _ -> failwith "impossible"
          in
          C.G.fold_succ_e
            (fun e ssa ->
              let ssa = C.remove_edge_e ssa e in
              let new_lab = match C.G.E.label e with
                | Some(Some true, _) -> Some(Some true, BinOp(EQ, v, val_true))
                | Some(Some false, _) -> Some(Some false, BinOp(EQ, v, val_false))
                | _ -> failwith "Successor of a CJmp should have a label"
              in
              let newe = C.G.E.create (C.G.E.src e) new_lab (C.G.E.dst e) in
              C.add_edge_e ssa newe
            ) ssa b ssa, stmts'
        | Ast.Jmp (je, _) as jmp::tl ->
          let revstmts = List.rev (stmts2ssa ctx ctxmap cfgb (List.rev tl)) in
          let revstmts, e' = exp2ssa ~revstmts ctx je in
          let ssa, revstmts = C.G.fold_succ_e
            (fun e (ssa, revstmts) ->
              let ssa = C.remove_edge_e ssa e in
              let new_lab, revstmts = match CA.G.E.label (es2a e) with
                | Some(a, Ast.BinOp(EQ, e1, e2)) ->
                  (* We assume that e and e1 are the same here. We may
                     want to relax this eventually. *)
                  if e1 <> je then
                    failwith (Printf.sprintf "Unexpected different expressions: %s %s" (Pp.ast_exp_to_string je) (Pp.ast_exp_to_string e1));
                  let revstmts, e2' = exp2ssa ~revstmts ctx e2 in
                  Some(a, Ssa.BinOp(EQ, e', e2')), revstmts
                | Some _ -> failwith "unknown edge condition format"
                | None -> None, revstmts
              in
              let newe = C.G.E.create (C.G.E.src e) new_lab (C.G.E.dst e) in
              C.add_edge_e ssa newe, revstmts
            ) ssa b (ssa, revstmts)
          in
          let revstmts = stmt2ssa ctx ctxmap ~revstmts (cfgb, List.length tl) jmp in
          ssa, List.rev revstmts
        | _ -> let stmts' = stmts2ssa ctx ctxmap cfgb stmts in ssa, stmts'
      in
      C.set_stmts ssa b stmts'
    in
    dprintf "going on to children";
    (* rename children *)
    let ssa = List.fold_left rename_block ssa (dom_tree b) in
    let () =
      (* Update any phis in our successors *)
      List.iter
        (fun s ->
           let s = C.G.V.label s in
           List.iter
             (fun v ->
                try
                  let (p,vs) = Hashtbl.find phis (s,v) in
                (* Note that lookup v will return different results
                   for each predecessor. There is also no guarantee
                   that each predecessor will have a unique
                   definition. *)
                  let v' = lookup v in
                if List.mem v' vs then ()
                else Hashtbl.replace phis (s,v) (p, v'::vs)
                with Not_found ->
                  failwith("phi for variable "^Pp.var_to_string v
                           ^" not found in "^Cfg.bbid_to_string s)
             )
             (blockphis s)
        )
        (C.G.succ ssa b)
    in
    (* save context for exit node *)
    (if bbid = BB_Exit then (
      VH.iter (fun k v -> VH.replace exitctx k (VH.find vh_ctx k)) vh_ctx ));
    (* restore context *)
    Ctx.pop ctx;
    ssa
  in
  let ssa = rename_block ssa (C.G.V.create BB_Entry) in
  dprintf "Adding %d phis to the CFG" (Hashtbl.length phis);
  let rec split_labels revlabels stmts =
    match stmts with
    | ((Label _ | Comment _) as s)::ss ->
      split_labels (s::revlabels) ss
    | _ -> (revlabels, stmts)
  in
  let ssa =
    (* actually add all our phis to the CFG *)
    C.G.fold_vertex
      (fun b ssa ->
         let bbid = C.G.V.label b in
         let vars = blockphis bbid in
         let (revlabs,stmts) = split_labels [] (C.get_stmts ssa b) in
         let stmts =
           List.fold_left
             (fun s v ->
                let (p,vs) = Hashtbl.find phis (bbid,v) in
                assert(vs <> []);
                (* FIXME: do something reasonable with attributes *)
                Move(p,Phi(vs), [])::s )
             stmts
             vars
         in
         C.set_stmts ssa b (List.rev_append revlabs stmts)
      )
      ssa ssa
  in
  let ssa = if tac then ToTac.ssa2tac ssa else ssa in
  dprintf "Done translating to SSA";
  let to_ssaexp loc e =
    let ctx = Hashtbl.find ctxmap loc in
    match exp2ssa (Ctx.copy ctx) ~revstmts:[] e with
    | [], e -> e
    | s, _ -> failwith (Printf.sprintf "to_ssaexp: Unable to convert %s to SSA without modifying the program" (Pp.ast_exp_to_string e))
  in
  let to_ssavar v = try VH.find exitctx v with Not_found -> v in
  let to_astvar v = try VH.find to_oldvar v with Not_found -> v in
  let to_astloc = VH.find to_oldloc in
  {ssacfg=ssa; to_ssaexp; to_ssavar; to_astvar; to_astloc}

let of_astcfg ?tac cfg =
  let {ssacfg=ssa} = trans_cfg ?tac cfg in
  ssa

let of_ast ?tac p =
  of_astcfg ?tac (Cfg_ast.of_prog p)


let uninitialized cfg =
  let module VS = Var.VarSet in
  let refd = ref VS.empty
  and assnd = ref VS.empty
  and add sr v = sr := VS.add v !sr in
  let process stmts =
    let rec f_s = function
      | Move(v, e, _) -> add assnd v; f_e e
      | Assert(e, _)
      | Assume(e, _)
      | Halt(e, _)
      | Jmp(e, _) -> f_e e
      | CJmp(e1, e2, e3, _) -> f_e e1; f_e e2; f_e e3
      | Label _ | Comment _ -> ()
      | Special(_,{defs; uses},_) ->
        List.iter (add assnd) defs;
        List.iter (add refd) uses
    and f_e = function
      | Load(v1,v2,v3,_) -> f_e v1; f_e v2; f_e v3
      | Store(v1,v2,v3,v4,_) -> f_e v1; f_e v2; f_e v3; f_e v4
      | Var v -> add refd v
      | Int _ | Lab _ -> ()
      | Ite(cond,v1,v2) -> f_e cond; f_e v1; f_e v2
      | Extract(_,_,v) -> f_e v
      | Concat(lv,rv) -> f_e lv; f_e rv
      | BinOp(_,v1,v2) -> f_e v1; f_e v2
      | UnOp(_,v)
      | Cast(_,_,v) -> f_e v
      | Phi vs -> List.iter (add refd) vs
      | Unknown _ -> ()
    in
    List.iter f_s stmts;
  in
  C.G.iter_vertex (fun b -> process (C.get_stmts cfg b)) cfg;
  VS.diff !refd !assnd


(* Hack to make a "unique" label *)
let rec mklabel =
  let r = ref 0
  and n = "autolabel" in
  let has_label c l =
    try ignore(C.find_label c l); true
     with Not_found -> false
  in
  (fun c ->
     let l = n ^ string_of_int !r in
     incr r;
     if has_label c (Name l) then mklabel c else l
  )

(** Perform edge splitting on the CFG.
    After edge splitting, it is guaranteed that every edge has either a unique
    predecessor or a unique successor.
*)
let split_edges c =
  let module E = C.G.E in
  let split_edge c e =
    let s = E.src e and d = E.dst e in
    (* s has multiple successors and d has multiple predecesors *)
    let revs = List.rev (C.get_stmts c s) in
    let cjmp = List.hd revs in
    let (cond,t1,t2,attrs) = match cjmp with
      | CJmp(c, t1, t2, a) -> (c,t1,t2,a)
      | _ -> failwith("Missing cjmp at end of node with multiple successors: "^Pp.ssa_stmt_to_string cjmp)
    in
    let newl = mklabel c in
    let el = E.label e in
    let (t1,t2,_) = match el with
      | Some (Some true, _) -> (Lab newl, t2, t1)
      | Some (Some false, _) -> (t1, Lab newl, t2)
      | _ -> failwith "Unlabeled edges from cjmp"
    in
    let revs = CJmp(cond, t1, t2, attrs) :: List.tl revs in
    let (c,v) = C.create_vertex c [Label(Name newl, [])] in
    let e' = C.G.E.create s el v in
    let c = C.add_edge_e c e' in
    let c = C.add_edge c v d in
    let c = C.remove_edge_e c e in
    C.set_stmts c s (List.rev revs)

  in
  let edges_to_split =
    C.G.fold_edges_e
      (fun e es ->
         if C.G.out_degree c (E.src e) > 1 && C.G.in_degree c (E.dst e) > 1
         then e::es
         else es
      )
      c []
  in
  List.fold_left split_edge c edges_to_split


let list_count p =
  List.fold_left (fun a e -> if p e then a+1 else a) 0


let rm_phis ?(dsa=false) ?(attrs=[]) cfg =
  let size = C.G.fold_vertex
    (fun b a ->
       a + list_count (function Move _->true | _->false) (C.get_stmts cfg b) )
    cfg 0
  in
  (* maps variables to the BBs where they were defined *)
  let assn = VH.create size in
  let () =
    C.G.iter_vertex
      (fun b ->
         List.iter
           (function
              | Move(v,_, _) -> VH.add assn v b
              | Special(_,du,_) -> List.iter (fun v -> VH.add assn v b) du.defs
              | _->())
           (C.get_stmts cfg b)
      )
      cfg
  in
  let entry = C.G.V.create BB_Entry in
    (* fake assignments for globals at the entry node *)
  Var.VarSet.iter (fun v -> VH.add assn v entry) (uninitialized cfg);
  (* split edges if needed *)
  let cfg = if dsa then split_edges cfg else cfg in

  let cfg, phis =
    (* Remove all the phis from all the BBs *)
    (* FIXME: make this readable *)
    C.G.fold_vertex
      (fun b (cfg,phis) ->
         let (ps,revstmts) =
           List.fold_left
             (fun (ps,revstmts) -> function
                | Move(l, Phi vs, _) ->
                    dprintf "rm_phis: removing phi for %s"(Pp.var_to_string l);
                    ((l,vs)::ps, revstmts)
                | s ->
                    (ps, s::revstmts)
             )
             (phis,[])
             (C.get_stmts cfg b)
         in
         (* Note that the statements in the block are now reversed *)
         (C.set_stmts cfg b revstmts, ps)
      )
      cfg
      (cfg, [])
  in
  let append_move b l p cfg=
    (* note that since stmts are reversed, we can prepend
       instead of appending. We must still be careful to not put
       assignments after a jump. *)
    let move = Move(l, Var p, (StrAttr "MoveFromPhi")::attrs) in
    C.set_stmts cfg b
      (match C.get_stmts cfg b with
       | (Jmp _ as j)::stmts
       | (CJmp _ as j)::stmts
       | (Halt _ as j)::stmts ->
           j::move::stmts
       | stmts ->
           move::stmts )
  in
  let cfg =
    (* Documentation note:

       Consider A <- PHI(X, Y).  Normally when we convert out of SSA
       we would add assignments A <- X where X is defined, and A <- Y
       where Y is defined.  However:

       X1 = 1
       cjmp foo, "t1", "t2"
       label t1:
       X2 = 2
       label t2:
       X3 = Phi(X1, X2)

       If you push the assignment all the way up to where X1 is
       assigned, when foo is true you'd be assigning X3 twice (e.g., violating DSA).

       (Thanks to Ivan Jager for the example)

       So, when we are converting to DSA we put the assignments
       directly in the predecessor.  *)

    if dsa then (
      let idom = Dom.compute_idom cfg entry in
      let dominators = Dom.idom_to_dominators idom in

      (* Map each node to a list of nodes above it in the dominator
         tree. The closest ancestor is first in the list. *)
      (* let domassns = *)
      (*   let () = dprintf "Building domassns" in *)
      (*   let domassns = BH.create (C.G.nb_vertex cfg) in *)
      (*   let rec fill_assns initassns cfg bb = *)
      (*     dprintf "Visiting bb %s" (v2s bb); *)
      (*     (\* The dominating blocks set acc assignments *\) *)
      (*     let newassns = List.fold_left (fun l s -> match s with Move(v,_,_) -> v::l | _ -> l) initassns (C.get_stmts cfg bb) in *)
      (*     (\* Add current assignments to domassns *\) *)
      (*     List.iter (fun v -> dprintf "addv: %s" (Pp.var_to_string v)) newassns; *)
      (*     let () = BH.add domassns (C.G.V.label bb) newassns in *)
      (*     List.iter (fill_assns newassns cfg) (dom_tree bb) *)
      (*   in *)
      (*   let () = fill_assns [] cfg entry in *)
      (*   let () = dprintf "... done" in *)
      (*   domassns *)
      (* in *)

      (* add an assignment for l to the end of v *)
      let dsa_push (l,vars) cfg bb =

        (* dprintf "dsa_push %s" (List.fold_left (fun s v -> s^" "^(Pp.var_to_string v)) "" vars); *)

        (* let rec find_var bb = (\* walk up idom tree starting at bb *\) *)
        (*   (\* dprintf " at %s" (v2s bb); *\) *)
        (*   try List.find (fun v -> bb = (VH.find assn v)) vars *)
        (*   with Not_found -> find_var (idom bb) *)
        (* in *)
        (* let _v' = find_var bb in *)
        (* let vbb = VH.find assn v in *)
        (* dprintf "%s assigned in bb %s, we are at %s" (Pp.var_to_string v) (v2s (VH.find assn v)) (v2s bb); *)
        (* A node can be its own predecessor, so we should also look
           at ourself. Dominators is sorted by dominance, with the most
           dominant node first, but we want to look at less dominant
           nodes with higher priority, so we need to reverse the list. *)
        (* dprintf "Looking for var in %s %s" (v2s bb) (List.fold_left (fun s v -> s^" "^(Pp.var_to_string v)) "" vars); *)
        let vl = bb :: (List.rev (dominators bb)) in
        (* List.iter (fun v -> dprintf "BB %s dominates" (v2s v)) vl; *)
        let assocl = List.map (fun v -> (VH.find assn v, v)) vars in
        (* List.iter (fun v -> dprintf "Var %s assigned in %s" (Pp.var_to_string v) (v2s (VH.find assn v))) vars; *)
        let v =
          let mybb = List.find (fun bb ->
            try ignore(List.assoc bb assocl); true
            with Not_found -> false) vl
          in
          List.assoc mybb assocl
        in
        (* dprintf "Found it: %s" (Pp.var_to_string myv); *)
        (* assert(myv = v); *)
        append_move bb l v cfg
      in
      (* assign the variable the phi assigns at the end of each of it's
         predecessors *)
      List.fold_left
        (fun cfg ((l, vars) as p) ->
           dprintf "rm_phis: adding assignments for %s" (Pp.var_to_string l);
          (* dprintf "There are %d preds" (List.length (C.G.pred cfg (VH.find assn l))); *)
           List.fold_left (dsa_push p) cfg (C.G.pred cfg (VH.find assn l))
        )
        cfg
        phis
    )
    else (
    (* assign the variables the phi assigns at the end of each block a variable
       the phi references is assigned. *)
    List.fold_left
      (fun cfg (l, vars) ->
         dprintf "rm_phis: adding assignments for %s" (Pp.var_to_string l);
         List.fold_left (fun cfg p -> append_move (VH.find assn p) l p cfg) cfg vars
      )
      cfg
      phis
    )
  in
  (* put statements back in forward order *)
  C.G.fold_vertex
    (fun b cfg -> C.set_stmts cfg b (List.rev(C.get_stmts cfg b)))
    cfg cfg

type tm = Ssa.exp VH.t

(* This function creates a mapping of temporary variables to their
   expression, so later copy propagation can be applied when converting
   from SSA three-address form to AST expressions of arbitrary size.

   The refd map keeps track of how many times a variable was
   referenced.  Non-existant bindings indicate zero references, true
   bindings indicate a single reference, and false bindings indicate
   more than one reference.  Temporaries are only added to the final
   map when there is one (or zero) reference.  *)
let create_tm c =
  let tm = VH.create 5700
  and refd = VH.create 5700 in
  let vis = object
    inherit Ssa_visitor.nop
    method visit_rvar v =
      (try
         if VH.find refd v then (
           VH.remove tm v;
           VH.replace refd v false)
       with Not_found -> VH.add refd v true);
      DoChildren

    method visit_stmt = function
      | Move(_, Phi _, _) ->
          DoChildren
      | Move(v,e,_) ->
          (* FIXME: should we check whether we introduced this var? *)
          (* FIX: we introduced it if it is named "temp" *)
          if (try VH.find refd v with Not_found -> true)
              && (Var.name v == ssa_temp_name)
          then VH.add tm v e;
          DoChildren
      | _ ->
          DoChildren
  end in
  C.G.iter_vertex
    (fun b -> ignore(Ssa_visitor.stmts_accept vis (C.get_stmts c b)))
    c;
  C.G.iter_edges_e
    (fun e -> match C.G.E.label e with
    | Some (_, e) -> ignore(Ssa_visitor.exp_accept vis e)
    | None -> ()
    ) c;
  tm

let rec exp2ast tm e =
  let e2a = exp2ast tm in
  match e with
    | Int(i,t) -> Ast.Int(i,t)
    | Lab s -> Ast.Lab s
    | Var l -> (try e2a (VH.find tm l) with Not_found -> Ast.Var l)
    | Ite(c,v1,v2) -> Ast.Ite(e2a c, e2a v1, e2a v2)
    | Extract(h,l,v) -> Ast.Extract(h, l, e2a v)
    | Concat(lv,rv) -> Ast.Concat(e2a lv, e2a rv)
    | BinOp(bo,v1,v2) -> Ast.BinOp(bo, e2a v1, e2a v2)
    | UnOp(uo, v) -> Ast.UnOp(uo, e2a v)
    | Cast(ct,t,v) -> Ast.Cast(ct, t, e2a v)
    | Unknown(s,t) -> Ast.Unknown(s,t)
    | Load(arr,idx,e, t) -> Ast.Load(e2a arr, e2a idx, e2a e, t)
    | Store(a,i,v, e, t) -> Ast.Store(e2a a, e2a i, e2a v, e2a e, t)
    | Phi _ -> failwith "exp2ast cannot translate Phi expressions"

(* Translates an SSA stmt back to Ast *)
let stmt2ast tm e =
  let e2a = exp2ast tm in
  match e with
    | Special(s,du,a) -> Ast.Special(s,Some du,a)
    | Jmp(t,a) -> Ast.Jmp(e2a t, a)
    | CJmp(c,tt,tf,a) -> Ast.CJmp(e2a c, e2a tt, e2a tf, a)
    | Label(l,a) -> Ast.Label(l,a)
    | Comment(s,a) -> Ast.Comment(s,a)
    | Assert(t,a) -> Ast.Assert(e2a t, a)
    | Assume(t,a) -> Ast.Assume(e2a t, a)
    | Halt(t,a) -> Ast.Halt(e2a t, a)
    | Move(l,e,a) -> Ast.Move(l, exp2ast tm e, a)

let stmts2ast tm stmts =
  let is_trash = function
    | Move(l,_,a) when List.mem Liveout a -> false
    | Move(l,_,_) when VH.mem tm l -> true
    | _ -> false
  in
  List.fold_right
    (fun s ast -> if is_trash s then ast else stmt2ast tm s :: ast)
    stmts []

(** Convert an ssa cfg (with phis already removed) back to a ast cfg *)
let cfg2ast tm cfg =
  let astcfg, _, _ = Cfg.map_ssa2ast (stmts2ast tm) (fun e -> exp2ast tm e) cfg in
  astcfg

type ssa_translation_results = {
  cfg : Cfg.AST.G.t;
  to_astexp: Ssa.exp -> Ast.exp; (** Maps SSA expressions to AST expressions. *)
}

let trans_ssacfg ?(remove_temps=true) ?(dsa=false) c =
  let tm = if remove_temps then create_tm c else VH.create 1 in
  {cfg=cfg2ast tm (rm_phis ~dsa c);
   to_astexp=exp2ast tm}

(** Convert an SSA CFG to an AST CFG. *)
let to_astcfg ?remove_temps ?dsa c =
  match trans_ssacfg ?remove_temps ?dsa c with
  | {cfg} -> cfg

(** Convert an SSA CFG to an AST program. *)
let to_ast ?(remove_temps=true) c =
  Cfg_ast.to_prog (to_astcfg ~remove_temps c)

let do_tac_ssacfg = ToTac.ssa2tac
