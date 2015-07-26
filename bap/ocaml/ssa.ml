(** 
    Static Single Assignment form.
    This is the intermediate language where most analysis should be happening.

    @author Ivan Jager
*)

open Big_int_Z
open Big_int_convenience
open BatListFull
open Type
open Var

type var = Var.t

(* type value = *)
(*   | Int of big_int * typ *)
(*   | Var of var *)
(*   | Lab of string *)


type exp = 
  | Load of exp * exp * exp * typ  (** Load(arr,idx,endian, t) *)
  | Store of exp * exp * exp * exp * typ  (** Store(arr,idx,val, endian, t) *)
  | BinOp of binop_type * exp * exp
  | UnOp of unop_type * exp
  | Var of var
  | Lab of string
  | Int of big_int * typ
  | Cast of cast_type * typ * exp (** Cast to a new type. *)
  (* Should SSA have Lets? *)
  | Unknown of string * typ
  | Ite of exp * exp * exp
  | Extract of big_int * big_int * exp
  | Concat of exp * exp
  | Phi of var list
      (** Joins variables that were assigned over different paths *)

type attrs = Type.attributes

type stmt =
  | Move of var * exp * attrs  (** Assign the exp on the right to the
				      var on the left *)
  | Jmp of exp * attrs (** Jump to a label/address *)
  | CJmp of exp * exp * exp * attrs
      (** Conditional jump. If e1 is true, jumps to e2, otherwise jumps to e3 *)
  | Label of label * attrs (** A label we can jump to *)
  | Halt of exp * attrs
  | Assert of exp * attrs
  | Assume of exp * attrs
  | Special of string * defuse * attrs
  | Comment of string * attrs (** A comment to be ignored *)
  (* | Special of string * attrs (** A "special" statement. (does magic) *) *)

let val_false = Int(zero_big_int, reg_1)
let val_true = Int(unit_big_int, reg_1)

(** If possible, make a label that would be refered to by the given
    expression. *)
let lab_of_exp = function
  | Lab s -> Some(Name s)
  | Int(i, t) ->
      Some(Addr(Arithmetic.to_big_int (i,t)))
  | _ -> None

(******************************************************************************)
(*                 Equality of SSA expressions and statements                 *)
(******************************************************************************)

let full_value_eq v1 v2 = v1 = v2
let quick_value_eq = full_value_eq

let num_exp = function
  | Load _ -> 0
  | Store _ -> 1
  | BinOp _ -> 2
  | UnOp _ -> 3
  | Var _ -> 4
  | Lab _ -> 5
  | Int _ -> 6
  | Cast _ -> 7
  | Unknown _ -> 8
  | Ite _ -> 9
  | Extract _ -> 10
  | Concat _ -> 11
  | Phi _ -> 12

  (* Returns elist, tlist, btlist, utlist, slist, clist, varlist, ilist *)
let getargs_exp = function
  | Load(e1,e2,e3,t1) -> [e1;e2;e3], [t1], [], [], [], [], [], []
  | Store(e1,e2,e3,e4,t1) -> [e1;e2;e3;e4], [t1], [], [], [], [], [], []
  | BinOp(bt,e1,e2) -> [e1;e2], [], [bt], [], [], [], [], []
  | UnOp(ut,e1) -> [e1], [], [], [ut], [], [], [], []
  | Var(v1) -> [], [], [], [], [], [], [v1], []
  | Lab(s1) -> [], [], [], [], [s1], [], [], []
  | Int(i1,t1) -> [], [t1], [], [], [], [], [], [i1]
  | Cast(c1,t1,e1) -> [e1], [t1], [], [], [], [c1], [], []
  | Unknown(s1,t1) -> [], [t1], [], [], [s1], [], [], []
  | Ite(e1,e2,e3) -> [e1;e2;e3], [], [], [], [], [], [], []
  | Extract(i1,i2,e1) -> [e1], [], [], [], [], [], [], [i1;i2]
  | Concat(e1,e2) -> [e1;e2], [], [], [], [], [], [], []
  | Phi(vl1) -> [], [], [], [], [], [], vl1, []


(** quick_exp_eq e1 e2 returns true if and only if the subexpressions
    in e1 and e2 are *physically* equal. *)
let quick_exp_eq e1 e2 =
  if (num_exp e1) <> (num_exp e2) then false else
    let l1,l2,l3,l4,l5,l6,l7,l8 = getargs_exp e1 in
    let r1,r2,r3,r4,r5,r6,r7,r8 = getargs_exp e2 in
    let b1 = List.for_all2 (==) l1 r1 in
    let b2 = List.for_all2 (==) l2 r2 in
    let b3 = List.for_all2 (==) l3 r3 in
    let b4 = List.for_all2 (==) l4 r4 in
    let b5 = List.for_all2 (==) l5 r5 in
    let b6 = List.for_all2 (==) l6 r6 in
    let b7 = List.for_all2 (==) l7 r7 in
    let b8 = List.for_all2 (==) l8 r8 in
    if b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 then
      true else false

(** full_exp_eq e1 e2 returns true if and only if e1 and e2 are
    structurally equivalent. *)
let rec full_exp_eq e1 e2 = e1 = e2

let (===) = full_exp_eq

let num_stmt = function
  | Move _ -> 0
  | Jmp _ -> 1
  | CJmp _ -> 2
  | Label _ -> 3
  | Halt _ -> 4
  | Assert _ -> 5
  | Assume _ -> 6
  | Comment _ -> 7
  | Special _ -> 8

let getargs_stmt = function
    (* value, var, label, attr, string, exp *)
  | Move(v,e,a) -> [], [v], [], [a], [], [e]
  | CJmp(e1,e2,e3,a) -> [e1;e2;e3], [], [], [a], [], []
  | Label(l,a) -> [], [], [l], [a], [], []
  | Jmp(e,a)
  | Halt(e,a)
  | Assert(e,a)
  | Assume(e,a) -> [e], [], [], [a], [], []
  | Comment(s,a) -> [], [], [], [a], [s], []
  | Special(s,{Var.defs; Var.uses},a) -> [], defs@uses, [], [a], [s], []

(** quick_stmt_eq returns true if and only if the subexpressions in e1
    and e2 are *physically* equal. *)
let quick_stmt_eq s1 s2 =
  if (num_stmt s1) <> (num_stmt s2) then false else
    let l1,l2,l3,l4,l5,l6 = getargs_stmt s1 in
    let r1,r2,r3,r4,r5,r6 = getargs_stmt s2 in
    let b1 = List.for_all2 (==) l1 r1 in
    let b2 = List.for_all2 (==) l2 r2 in
    let b3 = List.for_all2 (==) l3 r3 in
    let b4 = List.for_all2 (==) l4 r4 in
    let b5 = List.for_all2 (==) l5 r5 in
    let b6 = List.for_all2 (==) l6 r6 in
    if b1 && b2 && b3 && b4 && b5 && b6 then
      true
    else if b1 && b2 && b3 && b4 && b5 then
      (* s1 and s2 are not physically equal.  But maybe their
         subexpressions are physically equal. *)
      List.for_all2 quick_exp_eq l6 r6
    else
      false

(** full_stmt_eq returns true if and only if e1 and e2 are
    structurally equivalent. *)
let full_stmt_eq s1 s2 = s1 = s2
  (* if (num_stmt s1) <> (num_stmt s2) then false else *)
  (*   let l1,l2,l3,l4,l5,l6 = getargs_stmt s1 in *)
  (*   let r1,r2,r3,r4,r5,r6 = getargs_stmt s2 in *)
  (*   let b1 = List.for_all2 (==) l1 r1 in (\* e must use == *\) *)
  (*   let b2 = List.for_all2 (=) l2 r2 in *)
  (*   let b3 = List.for_all2 (=) l3 r3 in *)
  (*   let b4 = List.for_all2 (=) l4 r4 in *)
  (*   let b5 = List.for_all2 (=) l5 r5 in *)
  (*   let b6 = List.for_all2 (==) l6 r6 in *)
  (*   if b1 & b2 & b3 & b4 & b5 & b6 then *)
  (*     true *)
  (*   else if b2 & b3 & b4 & b5 then *)
  (*     (\* e1 and e2 are not physically equal.  But maybe the *)
  (*        subexpressions are structurally, but not physically, *)
  (*        equal. *\) *)
  (*     List.for_all2 full_exp_eq l6 r6 *)
  (*     && List.for_all2 full_value_eq l1 r1 *)
  (*   else *)
  (*     false *)

let get_attrs = function
  | Move(_,_,a)
  | Jmp(_,a)
  | CJmp(_,_,_,a)
  | Label(_,a)
  | Halt(_,a)
  | Assert(_,a)
  | Assume(_,a)
  | Comment(_,a)
  | Special(_,_,a) -> a

let exp_true = Int(bi1, Reg 1)
let exp_false = Int(bi0, Reg 1)
