(* Copy propagation

   XXX: Is it possible to implement this for AST and SSA without code
   duplication?
*)

open Ssa
open Type
open Var

module D = Debug.Make(struct let name = "Copy_prop" and default=`NoDebug end)
open D
module VH = Var.VarHash
module VM = Var.VarMap
module VS = Var.VarSet

module CPSpecSSA = struct

  module L = struct
    type et = Middle of Ssa.exp (** One assigned exp *) | Bottom (** Multiple assigned exps *)
    let et_to_string = function
      | Middle e -> Printf.sprintf "Middle %s" (Pp.ssa_exp_to_string e)
      | Bottom -> "Bottom"
    type t = Top | Map of et VM.t
    let top = Top
    let elementmeet x y = match x,y with
      | Bottom, _ -> Bottom
      | _, Bottom -> Bottom
      | x, y when x = y (* For Middle x = Middle x *) -> x
      | x, y -> (* For Middle x = Middle y when x <> y *) Bottom
    let meet x y = match x, y with
      | Top, x -> x
      | x, Top -> x
      | Map x, Map y ->
        Map (VM.fold
          (fun k v newmap ->
              (* Take the meet of each element. If not found, use v,
                 since elementmeet v v = v *)
              let v' = try VM.find k newmap with Not_found -> v in
              VM.add k (elementmeet v v') newmap
          ) x y)
    let equal x y = match x, y with
      | Top, Top -> true
      | Map x, Map y -> VM.equal (=) x y
      | _, _ -> false
  end
  module CFG = Cfg.SSA
  module O = GraphDataflow.NOOPTIONS

  let stmt_transfer_function () _g (_bb,_) stmt l =
    let l = match l with | L.Map m -> m | L.Top -> failwith (Printf.sprintf "Expected Map, not Top at %s" (Cfg_ssa.v2s _bb)) in
    L.Map (match stmt with
    | Move(v, Phi _, _) ->
      VM.add v L.Bottom l
    | Move(v,e,_) ->
      (* dprintf "ignoring %s" (Pp.ssa_stmt_to_string s); *)
      VM.add v (L.Middle e) l
    | _ -> l)

  let edge_transfer_function _ g _ _ l = l

  let s0 _ _ = Cfg.SSA.G.V.create Cfg.BB_Entry
  let init _ _ = L.Map VM.empty
  let dir _ = GraphDataflow.Forward
end

module CPSpecAST = struct

  module L = struct
    type et =
      | Middle of (Cfg.AST.G.V.t * int) * Ast.exp (** One assigned exp *)
      | Bottom (** Multiple assigned exps *)
    let et_to_string = function
      | Middle (v,e) -> Printf.sprintf "Middle %s" (Pp.ast_exp_to_string e)
      | Bottom -> "Bottom"
    type t = Top | Map of et VM.t
    let top = Top
    let elementmeet x y = match x,y with
      | Bottom, _ -> Bottom
      | _, Bottom -> Bottom
      | x, y when x = y (* For Middle x = Middle x *) -> x
      | x, y -> (* For Middle x = Middle y when x <> y *) Bottom
    let meet x y = match x, y with
      | Top, x -> x
      | x, Top -> x
      | Map x, Map y ->
        Map (VM.fold
          (fun k v newmap ->
              (* Take the meet of each element. If not found, use v,
                 since elementmeet v v = v *)
              let v' = try VM.find k newmap with Not_found -> v in
              VM.add k (elementmeet v v') newmap
          ) x y)
    let equal x y = match x, y with
      | Top, Top -> true
      | Map x, Map y -> VM.equal (=) x y
      | _, _ -> false
  end
  module CFG = Cfg.AST
  module O = GraphDataflow.NOOPTIONS

  let stmt_transfer_function () g ((_bb,_) as loc) stmt l =
    let l = match l with | L.Map m -> m | L.Top -> failwith (Printf.sprintf "Expected Map, not Top at %s" (Cfg_ast.v2s _bb)) in
    L.Map (match stmt with
    | Ast.Move(v,e,_) ->
      (* dprintf "seeing %s" (Pp.ast_stmt_to_string s); *)
      VM.add v (L.Middle (loc, e)) l
    | _ -> l)

  let edge_transfer_function _ g _ _ l = l

  let s0 _ _ = Cfg.AST.G.V.create Cfg.BB_Entry
  let init _ _ = L.Map VM.empty
  let dir _ = GraphDataflow.Forward
end

module CPSSA = CfgDataflow.Make(CPSpecSSA)

let copyprop_ssa ?(stop_before=(fun _ -> false)) ?(stop_after=(fun _ -> false)) g =
  let _, dfout =
    CPSSA.worklist_iterate g in
  let has f e =
    let vis = object(self)
      inherit Ssa_visitor.nop
      method visit_exp e =
        if f e
        then raise Exit
        else DoChildren
    end
    in
    try ignore(Ssa_visitor.exp_accept vis e); false
    with Exit -> true
  in
  let propagate l v =
    let vis = object(self)
      inherit Ssa_visitor.nop
      method visit_exp e = dprintf "visit_exp %s" (Pp.ssa_exp_to_string e); match e with (*function*)
        | Ssa.Var v ->
          (try
             match VM.find v l with
             | CPSpecSSA.L.Middle e ->
               dprintf "%s to %s" (Pp.var_to_string v) (Pp.ssa_exp_to_string e);
               if has stop_before e then SkipChildren
               else if has stop_after e then ChangeTo e
               else ChangeToAndDoChildren e
             | _ -> SkipChildren
           with Not_found -> SkipChildren)
        | _ -> DoChildren
    end in
    Ssa_visitor.exp_accept vis v
  in

  let l = dfout (Cfg.SSA.G.V.create Cfg.BB_Exit) in
  let l = match l with
    | CPSpecSSA.L.Map m -> m
    | _ -> failwith "Expected to find a map: BB_Exit probably unreachable"
  in
  let cm =
    lazy
      (VM.fold (fun k v newmap ->
        (match v with
        | CPSpecSSA.L.Middle x ->
          let ssae = propagate l x in
        (* dprintf "%s maps to %s" (Pp.var_to_string k) (Pp.ssa_exp_to_string ssae); *)
          VM.add k ssae newmap
        | _ ->
          newmap)
       ) l VM.empty)
  in
  let rm =
    VM.filter_map (fun k v -> match v with
      | CPSpecSSA.L.Middle x -> Some x
      | CPSpecSSA.L.Bottom -> None
    ) l
  in
  cm, rm, propagate l

module CPAST = CfgDataflow.Make(CPSpecAST)

let copyprop_ast ?(stop_before=(fun _ -> false)) ?(stop_after=(fun _ -> false)) g =
  let dfin, _ = CPAST.worklist_iterate_stmt g in
  let get_map = function
    | CPSpecAST.L.Map m -> m
    | _ -> failwith "Expected to find a map: BB_Exit probably unreachable"
  in
  let _, defs = Depgraphs.UseDef_AST.usedef g in
  let vars_in e =
    let s = ref VS.empty in
    let v = object(self)
      inherit Ast_visitor.nop
      method visit_rvar v =
        s := VS.add v !s;
        SkipChildren
    end in
    ignore(Ast_visitor.exp_accept v e);
    !s
  in
  let module UD = Depgraphs.UseDef_AST in
  let module LS = Depgraphs.UseDef_AST.LS in
  let has f e =
    let vis = object(self)
      inherit Ast_visitor.nop
      method visit_exp e =
        if f e
        then raise Exit
        else DoChildren
    end
    in
    try ignore(Ast_visitor.exp_accept vis e); false
    with Exit -> true
  in
  let rec propagate dfin origloc visitedlocs loc e =
    let l = get_map (dfin loc) in
    let vis = object(self)
      inherit Ast_visitor.nop
      method visit_exp = function
        (* Stop propagating *)
        | Ast.Var v ->
          (try
             match VM.find v l with
             | CPSpecAST.L.Middle (newloc, e) ->
               (* Use copy propagation results AT the location of the
                  earlier definition, but only if all definitions of
                  variables in e are the ones in scope at the current
                  location *)
               (* Make sure all variables referenced in e are the same
                  definitions at the original location *)
               let can_copy = VS.for_all
                 (fun use ->
                   let vdefs = defs origloc use in
                   let vdefs' = defs newloc use in
                   (* Same definitions *)
                   vdefs = vdefs'
                   (* No cycle: The definitions do not include a previously visited location *)
                   && LS.is_empty (LS.inter vdefs visitedlocs)
                  ) (vars_in e) in
               if can_copy then (
                 if has stop_before e then SkipChildren
                 else if has stop_after e then ChangeTo e
                 else ChangeTo (propagate dfin origloc (LS.add (UD.LocationType.Loc newloc) visitedlocs) newloc e)
               ) else SkipChildren
             | _ -> SkipChildren
           with Not_found -> SkipChildren)
        | _ -> DoChildren
    end in
    Ast_visitor.exp_accept vis e
  in

  (fun loc ->
    let l = get_map (dfin loc) in
    VM.fold (fun k v newmap ->
      (match v with
      | CPSpecAST.L.Middle (loc, x) ->
        let aste = propagate dfin loc (LS.singleton (UD.LocationType.Loc loc)) loc x in
        (* dprintf "%s maps to %s" (Pp.var_to_string k) (Pp.ast_exp_to_string aste); *)
        VM.add k aste newmap
      | _ ->
        newmap)
    ) l VM.empty,

    (* Export converted lattice value *)
    VM.filter_map (fun _ v -> match v with
      | CPSpecAST.L.Middle (loc, e) -> Some(loc, e)
      | CPSpecAST.L.Bottom -> None) l,

    propagate dfin loc (LS.singleton (UD.LocationType.Loc loc)) loc
  )

let get_vars ?stop_before ?stop_after g es =
  let _, m, _ = copyprop_ssa ?stop_before ?stop_after g in

  let s = ref VS.empty in
  let add v = s := VS.add v !s in
  let v = object(self)
    inherit Ssa_visitor.nop
    method visit_exp = function
      | Var v ->
        add v;
        (try ChangeToAndDoChildren (VM.find v m)
         with Not_found -> SkipChildren)
      | _ -> DoChildren
  end in
  List.iter (fun e -> ignore(Ssa_visitor.exp_accept v e)) es;
  !s
