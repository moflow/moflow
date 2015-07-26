(* Common VSA stuff *)

open Big_int_convenience
open Big_int_Z
open Type
open Util
module VM = Var.VarMap

module D = Debug.Make(struct let name = "Vsa" and default=`NoDebug end)
open D

let gcd x y =
  if x = bi0 then y
  else if y = bi0 then x
  else Z.gcd x y
let lcm = Z.lcm
let rem = Z.erem

let bits_of_width = Typecheck.bits_of_width

let mem_max = ref (Some(1 lsl 16))

exception Unimplemented of string

(* Strided Intervals *)
module SI =
struct
(* FIXME: some of these functions can return a stride of 1 for a singleton *)

  (** number of bits * unsigned stride * signed lower bound * signed upper bound *)
  type t = int * big_int * big_int * big_int

  let is_empty (k,s,lb,ub) =
    s = bim1 && lb = bi1 && ub = bi0

  let to_string ((k,s,lb,ub) as si) =
    if is_empty si then "[empty]"
    else Printf.sprintf "%s%s[%s,%s]" (if debug () then "("^string_of_int k^")" else "") (~% s) (~% lb) (~% ub)

  let size (_,s,lb,ub) =
    if s = bi0 then bi1
    else ((ub -% lb) /% s) +% bi1

  let highbit k =
    if k = 1 then bi1 else bi1 <<% (k-1)

  (* Cast i as signed number *)
  let extend k i =
    Arithmetic.to_sbig_int (i, Reg k)

  (* Set the irrelevant upper bits of i to 0 *)
  let trunc k i =
    Arithmetic.to_big_int (i, Reg k)

  let maxi k = highbit k -% bi1
  let mini k = extend k (highbit k)
  let top k = (k, bi1, mini k, maxi k)
  let empty k = (k, bim1, bi1, bi0)

  let rec upper k i s =
    if s >= bi1 then (
    let offset = rem i s in
    let max = maxi k in
    let maxoffset = rem max s in
    let o = if maxoffset >= offset then
        max -% (maxoffset -% offset)
      else
        max -% ((maxoffset +% s) -% offset)
    in
    if debug ()
    then (assert (o <= maxi k && o > maxi k -% s);
          assert (rem o s = rem i s));
    o) else maxi k

  let rec lower k i s =
    if s >= bi1 then (
    let offset = rem i s in
    let min = mini k in
    let minoffset = rem min s in
    let o = if offset >= minoffset then
        min +% (offset -% minoffset)
      else
        min +% ((offset +% s) -% minoffset)
    in
    if debug ()
    then (assert (o >= mini k && o < mini k +% s);
          assert (rem o s = rem i s));
    o) else mini k

  let remove_lower_bound (k,s,a,b) =
    (k,s,lower k b s,b)
  let remove_upper_bound (k,s,a,b) =
    (k,s,a,upper k a s)

  let single k x = (k,bi0,x,x)
  let of_bap_int i t = single (bits_of_width t) (extend (bits_of_width t) i)

  let above k x = (k, bi1, x +% bi1, maxi k)
  let below k x = (k, bi1, mini k, x -% bi1)

  (* These are hacks *)
  let above_unsigned k x = (k, bi1, x +% bi1, maxi k)
  let below_unsigned k x = (k, bi1, bi0, x -% bi1)

  let aboveeq k x = (k, bi1, x, maxi k)
  let beloweq k x = (k, bi1, mini k, x)

  (* These are hacks *)
  let aboveeq_unsigned k x = (k, bi1, x, maxi k)
  let beloweq_unsigned k x = (k, bi1, bi0, x)

  let zero k = single k bi0
  let one k = single k bi1
  let minus_one k = single k bim1

  (* XXX: Remove k argument *)
  let is_reduced k ((k',s,lb,ub) as si) =
    if k > 64 then raise (Unimplemented (Printf.sprintf "Register type of %d bits is too large (must be <= 64)" k));
    assert(k=k');
    assert(k>0 && k<=64);
    (lb >= mini k && ub <= maxi k) &&
      is_empty si ||
      (if s = bi0 then lb = ub
       else lb < ub && let r1 = rem lb s and r2 = rem ub s in r1 = r2 || r2 -% r1 = s)

  let check_reduced k si =
    if not(is_reduced k si)
    then failwith(string_of_int k^"-bit Strided Interval "^to_string si^" not in reduced form")

  let check_reduced2 k si1 si2 =
    check_reduced k si1; check_reduced k si2

  (* XXX: Remove k argument *)
  let renorm k ((k',a,b,c) as si) =
    assert(k=k');
    let si' = if b = c then (k,bi0,b,b) else si in
    let si' = if a < bi0 || c < b then empty k else si' in
    check_reduced k si';
    si'

  let renormtri f k x y z = renorm k (f k x y z)
  let renormbin f k x y = renorm k (f k x y)
  let renormun f k x = renorm k (f k x)

  (* For union/intersection which don't use k argument *)
  let renormbin' f ((k,_,_,_) as x) y = renorm k (f x y)

  (* XXX: Remove k *)
  (** Addition of strided intervals *)
  let add ?(allow_overflow=true) k ((k',s1,lb1,ub1) as a) ((k'',s2,lb2,ub2) as b) =
    assert (k=k' && k=k'');
    check_reduced2 k a b;
    let lb' = lb1 +% lb2
    and ub' = ub1 +% ub2 in
    (* Overflow cases 2 and 4; see dissertation *)
    let lbunderflow = lb' < mini k
    and uboverflow = ub' > maxi k
    and s = gcd s1 s2
    in
    let overflow = lbunderflow || uboverflow in
    match overflow with
    | true when allow_overflow ->
      top k
    | _ ->
      let lb' = extend k lb' in
      let ub' = extend k ub' in
      let lb'' = if lbunderflow then lower k lb' s else lb'
      and ub'' = if uboverflow then upper k ub' s else ub'
      in
      (k, s, lb'', ub'')

  let add ?allow_overflow k x y = renormbin (add ?allow_overflow) k x y

  (* XXX: Remove k *)
  (** Negation of a strided interval *)
  let neg k ((k',s,lb,ub) as si) =
    assert(k=k');
    check_reduced k si;
    if lb <> extend k (highbit k) then
      (k, s, Z.neg ub, Z.neg lb)
    else if lb = ub then
      single k (mini k)
    else
      top k
  let neg = renormun neg

  (** Subtraction of strided intervals *)
  let sub k a b =
    add k a (neg k b)
  let sub = renormbin sub

  let minor k a b c d =
    let rec loop m =
      let cont() = loop (m >>% 1) in
        if m = bi0 then a |% c
        else if Z.lognot a &% c &% m <> bi0 then
          let temp = (a |% m ) &% Z.neg m in
            if big_int_ucompare temp b <= 0 then
              temp |% c
            else cont()
        else if a &% Z.lognot c &% m  <> bi0 then
          let temp = (c +% m) &% Z.neg m in
            if big_int_ucompare temp d <= 0 then
              temp  |% a
            else cont()
        else
          cont()
    in
      loop (highbit k)

  let maxor k a b c d =
    let rec loop m =
      let cont() = loop (m >>% 1) in
        if m = bi0 then b |% d
        else if b &% d &% m <> bi0 then
          let temp1 = (b -% m) |% (m -% bi1) in
          let temp2 = (d -% m) |% (m -% bi1) in
            if big_int_ucompare temp1 a >= 0 then
              temp1 |% d
            else if big_int_ucompare temp2 c >= 0 then
              temp2 |% b
            else cont()
        else
          cont()
    in
      loop (highbit k)

  let ntz x =
    let y = Z.neg x &% (x -% bi1) in
    let rec bits n y =
      if y = bi0 then n else bits (n+1) (y $>>% 1)
    in
      bits 0 y


  (** Bitwise OR *)
  let logor k ((k',s1,lb1,ub1) as a) ((k'',s2,lb2,ub2) as b) =
    assert (k=k' && k=k'');
    check_reduced2 k a b;
    let t = min (ntz s1) (ntz s2) in
    let s' = bi1 <<% t in
    let lowbits = (lb1 |% lb2) &% (s' -% bi1) in
    let (lb', ub') = match (lb1 < bi0, ub1 < bi0, lb2 < bi0, ub2 < bi0) with
      | (true, true, true, true)
      | (true, true, false, false)
      | (false, false, true, true)
      | (false, false, false, false) ->
          (minor k lb1 ub1 lb2 ub2, maxor k lb1 ub1 lb2 ub2)
      | (true, true, true, false) ->
          (lb1, bim1)
      | (true, false, true, true) ->
          (lb2, bim1)
      | (true, false, true, false) ->
          (min lb1 lb2, maxor k bi0 ub1 bi0 ub2)
      | (true, false, false, false) ->
          (minor k lb1 (bim1) lb2 ub2, maxor k bi0 ub1 lb2 ub2)
      | (false, false, true, false) ->
          (minor k lb1 ub1 lb2 (bim1), maxor k lb1 ub1 lb2 ub2)
      | _ -> failwith "Impossible: check_reduced prevents this"
    in
    let highmask = Z.lognot(s' -% bi1) in
      (k, s', (lb' &% highmask) |% lowbits, (ub' &% highmask) |% lowbits)
  let logor = renormbin logor

  (* XXX: Get rid of _k *)
  (** Bitwise NOT *)
  let lognot (_k:int) (k,s,l,u) =
    assert (_k = k);
    (k, s, Z.lognot u, Z.lognot l)
  let lognot = renormun lognot


  (** Bitwise AND *)
  let logand k x y =
    lognot k (logor k (lognot k x) (lognot k y))
  let logand = renormbin logand


  (** Bitwise XOR *)
  let logxor k x y =
    let n = lognot k
    and o = logor k in
    o (n(o (n x) y)) (n(o x (n y)))
  let logxor = renormbin logxor

  (** unsigned modulus *)
  let modulus k (k',s1,a,b) (k'',s2,c,d) =
    assert(k=k' && k=k'');
    if b = bi0 then single k bi0
    else
      (k, bi1, bi0, big_int_umin b d)
  let modulus = renormbin modulus

(* XXX: Get rid of k *)
(* shifting by more than k or by negative values
 * will be the same as shifting by k. *)
  let toshifts k =
    let f x = if x > bi k || x < bi0 then k else Big_int_Z.int_of_big_int x in
      function
        | (k',z,x,y) when z = bi0 ->
          assert(x=y);
          assert(k=k');
          let s = f x in
          (s,s)
        | (k',_s,x,y) ->
          assert(k=k');
          if x < bi0 then
            if y >= bi0 then
              (* FIXME: using stride information could be useful here *)
              (0, k)
            else (k,k)
          else (* x >= 0L *)
            (f x, f y)

  (* Get rid of k *)
  let mk_shift dir shifter k ((k',s1,a,b) as x) ((k'',_,_,_) as y) =
    assert(k=k' && k=k'');
    check_reduced2 k x y;
    (* Get the lower and upper bound for y, as shift amounts.  Shifts
       amounts are always in [0,k]. *)

    let (z1,z2) = toshifts k y
    (* Set the upper bits of a and b to 0 *)
    and aa = trunc k a
    and bb = trunc k b
    (* Shift and cast as signed number *)
    and shift n z = extend k (shifter n z) in

    (* Shift the lower bound by all possible shift amounts, and the
       upper bound by all possible shift amounts.  The min/max of the
       resulting values is the min/max bound of the shift result. *)

    let open BatPervasives in
    let mins = List.map (shift aa) (BatList.of_enum (z1--z2))
    and maxs = List.map (shift bb) (BatList.of_enum (z1--z2)) in
    let shifts = mins@maxs in

    let l = BatList.reduce min shifts in
    let u = BatList.reduce max shifts in

    let sign x = x >= bi0 in
    let simpleshift =
    (* We have a simple shift if all shifts never change the sign of
       the value *)
      let minok = List.for_all (fun x -> sign x = sign aa) mins in
      let maxok = List.for_all (fun x -> sign x = sign bb) maxs in
      minok && maxok
    in

    let s', l, u = match simpleshift, dir with
      | true, `Rightshift -> big_int_umax (s1 >>% z2) bi1, l, u
      | true, `Leftshift -> big_int_umax (s1 <<% z1) bi1, l, u
      | false, _ -> bi1, mini k, maxi k (* top *)
    in
    renorm k (k,s',l,u)

  (** Logical right-shift *)
  let rshift = mk_shift `Rightshift (>>%)

  (** Arithmetic right-shift *)
  let arshift = mk_shift `Rightshift ($>>%)

  (** Left shift *)
  let lshift = mk_shift `Leftshift (<<%)

  let cast_low tok ((k,s,a,b) as v) =
    assert (tok <= k);
    if tok = k then v
    else (
      let fits x = x = extend tok x in
      (* If a and b are in the lowest tok bits, just keep those! *)
      if fits s && fits a && fits b then
        (tok, s, a, b)
      else
        (* XXX: We can probably do better here *)
        top tok
    )
  let cast_low = renormun cast_low

  let cast_high tok ((k,s,a,b) as v) =
    assert (tok <= k);
    if tok = k then v
    else (
      (* Shift right, then cast low *)
      let v = rshift k v (single k (bi (k - tok))) in
      cast_low tok v)
  let cast_high = renormun cast_high

  let cast_signed tok ((k,s,a,b) as _v) =
    assert (tok >= k);
    (* Signed extension preserves signed values, so this is super
       easy! *)
    (tok,s,a,b)
  let cast_signed = renormun cast_signed

  let cast_unsigned tok ((k,s,a,b) as _v) =
    assert (tok >= k);
    (* Unsigned casting of signed numbers is bizarre.  For positive
       numbers, there is no problem, since sign-extension is the same as
       zero-extension for positive numbers.  For negative numbers,
       however, a negative number is transformed into a (large) positive
       number.  *)
    let c x =
      if x >= bi0 then x
      else trunc k x in
    let a' = c a
    and b' = c b in
    (* Re-order if needed *)
    let a',b' = if a' <= b' then a',b' else b',a' in
    (tok,s,a',b')
  let cast_unsigned = renormun cast_unsigned

  let extract k h l ((k,_,_,_) as x) =
    let nb = (h-l)+1 in
    assert (h >= 0);
    assert (nb >= 0);
    let x = if l <> 0 then rshift k (single k (bi l)) x else x in
    let x = if nb <> k then cast_low nb x else x in
    x
  let extract = renormtri extract

  let concat k (((k1,s1,l1,u1) as x) : t) (((k2,s2,l2,u2) as y) : t) =
    assert (k = k1 + k2);
    let x = cast_unsigned k x in
    let y = cast_unsigned k y in
    let x = lshift k x (single k (bi k2)) in
    logor k x y
  let concat = renormbin concat

  (* construct these only once *)
  let yes = single 1 bim1
  and no = single 1 bi0
  and maybe = (1, bi1, bim1, bi0)

  (* XXX: Remove k *)
  let eq k ((k',s1,a,b) as x) ((k'',s2,c,d) as y) =
    assert(k=k' && k=k'');
    check_reduced2 k x y;
    if a = b && a = c && a = d then
      yes
    else if b < c || d < a then
      no
    else
      let s' = gcd s1 s2 in
      let r1 = rem a s'
      and r2 = rem c s' in
        if r1 = r2 then
          maybe
        else
          no

  let union ((k,s1,a,b) as si1) ((k',s2,c,d) as si2) =
    if k <> k' then raise (Invalid_argument "bitwidth");
    (* Empty sets *)
    if is_empty si1 then si2
    else if is_empty si2 then si1
    else
    let s' = gcd s1 s2 in
      if s' = bi0 then
        if a = b && c = d then
          let u = max a c
          and l = min a c in
            (k, u -% l, l, u)
        else failwith "union: strided interval not in reduced form"
      else
        let r1 = rem a s' (* not right when s' is negative. *)
        and r2 = rem c s' in
        let u = max b d
        and l = min a c in
        if s' > bi0 && r1 = r2 then
          (k, s', l, u)
        else
          let s'' = gcd (abs_big_int (r1 -% r2)) s' in
          (k, s'', l, u)
  let union = renormbin' union

  let intersection ((k,s1,a,b) as si1) ((k',s2,c,d) as si2) =
    if is_empty si1 || is_empty si2 then empty k
    else if k <> k' then raise (Invalid_argument "bitwidth")
    else
    let l = max a c
    and u = min b d in
    if s1 = bi0 && s2 = bi0 then
      if a = c then (k,s1,a,b) else (empty k)
    else if s1 = bi0 then
      if rem (c -% a) s2 = bi0 && a >= c && a <= d then (k,s1,a,b) else (empty k)
    else if s2 = bi0 then
      if rem (c -% a) s1 = bi0 && c >= a && c <= b then (k',s2,c,d) else (empty k)
    else (
      let s' = lcm s1 s2 in
      if rem a s' = bi0 && rem c s' = bi0 then
        let l = l and u = u -% (rem u s') in
        if u >= l then (k, s', l, u -% (rem u s')) else empty k
      else (k, bi1, l, u))
  let intersection = renormbin' intersection

  let widen ((k,s1,a,b) as si1) ((k',s2,c,d) as si2) =
    if is_empty si1 && not (is_empty si2) then top k
    else if is_empty si1 then si2
    else if is_empty si2 then si1
    else if k <> k' then failwith "widen: expected same bitwidth intervals"
    else
    (* dprintf "Widen: %s to %s" (to_string si1) (to_string si2); *)
    let s' = gcd s1 s2 in
    let l = if c < a then lower k a s' else a
    and u = if d > b then upper k b s' else b in
    if s' = bi0 then
      if a = b && c = d then
        (k, u -% l, l, u)
      else failwith "widen: strided interval not in reduced form"
    else (k, s', l, u)
  let widen = renormbin' widen

  let rec fold f ((k,s,a,b) as si) init =
    if a = b then f a init
    else if is_empty si then init
    else fold f (k, s, a+%s ,b) (f a init)

  let binop_to_si_function = function
    | PLUS -> add ~allow_overflow:true
    | MINUS -> sub
    | AND -> logand
    | OR -> logor
    | XOR -> logxor
    | MOD -> modulus
    | RSHIFT -> rshift
    | ARSHIFT -> arshift
    | LSHIFT -> lshift
    | EQ -> eq
    | NEQ -> fun k x y -> lognot 1 (eq k x y)
    | TIMES
    | DIVIDE
    | SDIVIDE
    | SMOD
    | LT
    | LE
    | SLT
    | SLE
      -> raise (Unimplemented "unimplemented binop")

  let unop_to_si_function = function
    | NEG -> neg
    | NOT -> lognot

  let cast_to_si_function = function
    | CAST_UNSIGNED -> cast_unsigned
    | CAST_SIGNED -> cast_signed
    | CAST_LOW -> cast_low
    | CAST_HIGH -> cast_high

end (* module SI *)

(* Value Sets *)
module VS =
struct
  type region = Var.t (* FIXME? *)

  type address = region * SI.t

  type t = address list

  let global = Var.newvar "global region" (Reg 64) (* value doesn't really matter, so long as it's unique *)

  let top k = [(global, SI.top k)]
  let empty k = [(global, SI.empty k)]

  let rec width = function
    | (_, (k,_,_,_))::_ -> k
    | [] -> failwith "width: empty value set"

  let size k vs =
    BatList.reduce (+%) (List.map (fun (_,si) -> SI.size si) vs)

  let pp_address p (r, si) =
    if r == global then p "$" else p(Pp.var_to_string r);
    p " |=> ";
    p (SI.to_string si)

  let pp p = function
    | [] -> failwith "this should not happen"
    | x::xs ->
        p "(";
        pp_address p x;
        List.iter (fun x -> p ", "; pp_address p x) xs;
        p ")"

  let to_string vs =
    let b = Buffer.create 57 in
    let p = Buffer.add_string b in
    pp p vs;
    Buffer.contents b

  let single k x = [(global, SI.single k x)]
  let of_bap_int i t = [(global, SI.of_bap_int i t)]

  let remove_lower_bound = List.map (fun ((r:region),(si:SI.t)) -> (r, SI.remove_lower_bound si))
  let remove_upper_bound = List.map (fun ((r:region),(si:SI.t)) -> (r, SI.remove_upper_bound si))

  let zero k = [(global, SI.zero k)]
  let one k = [(global, SI.one k)]
  let minus_one k = [(global, SI.minus_one k)]

  let above k i = [(global, SI.above k i)]
  let below k i = [(global, SI.below k i)]
  let above_unsigned k i = [(global, SI.above_unsigned k i)]
  let below_unsigned k i = [(global, SI.below_unsigned k i)]

  let aboveeq k i = [(global, SI.aboveeq k i)]
  let beloweq k i = [(global, SI.beloweq k i)]
  let aboveeq_unsigned k i = [(global, SI.aboveeq_unsigned k i)]
  let beloweq_unsigned k i = [(global, SI.beloweq_unsigned k i)]

  let add k x y =
    let e = empty k in
    if x = e || y = e then e
    else match (x,y) with
    | ([r2,si2],[r1,si1]) when r1 == global ->
      let allow_overflow = r2 == global in
      [(r2, SI.add ~allow_overflow k si1 si2)]
    | ([r1,si1],[r2,si2]) when r1 == global ->
      let allow_overflow = r2 == global in
      [(r2, SI.add ~allow_overflow k si1 si2)]
    | ([r,si1], xs) | (xs, [r,si1]) when r == global ->
      List.map (fun (r,si) ->
        let allow_overflow = r == global in
        (r, SI.add ~allow_overflow k si1 si)) xs
    | _ -> top k

  let sub k x = function
    | [r,si] when r == global ->
      List.map (fun (r,si') -> (r, SI.sub k si' si)) x
    | _ -> top k

  let makeother f id annihilator k x y =
    match (x,y) with
    | ([r1,si1], [r2,si2]) when r1 == global && r1 == r2 ->
      [(r1, f k si1 si2)]
    | ([_] as vsg, vs) when vsg = id ->
        vs
    | (vs, ([_] as vsg))  when vsg = id ->
        vs
    | ([_] as vsg, _)  when Some vsg = annihilator ->
        BatOption.get annihilator
    | (_,([_] as vsg))  when Some vsg = annihilator ->
        BatOption.get annihilator
    | _ -> top k

  let logand k = makeother SI.logand (minus_one k) (Some (zero k)) k

  let logor k = makeother SI.logor (zero k) (Some (minus_one k)) k

  let logxor k = makeother SI.logxor (zero k) None k

  let si_to_vs_binop_function f k =
    let g vs1 vs2 = match vs1, vs2 with
      | [(r1,si1)], [(r2,si2)] when r1 == global && r2 == global -> [(r1, f k si1 si2)]
      | _ -> raise(Unimplemented "unimplemented binop") in
    g

  let si_to_vs_unop_function f k =
    let g vs1 = match vs1 with
      | [(r1,si1)] when r1 == global -> [(r1, f k si1)]
      | _ -> raise(Unimplemented "unimplemented unop") in
    g

  let si_to_vs_cast_function = si_to_vs_unop_function

  let concat = si_to_vs_binop_function SI.concat

  let yes = [(global, SI.yes)]
  let no = [(global, SI.no)]
  let maybe = [(global, SI.maybe)]

  (** Slightly unconservative equality checking. *)
  let eq k x y = match (x,y) with
     | ([r1,si1], [r2,si2]) when r1 == r2 ->
         [(global, SI.eq k si1 si2)]
     | (r, _) when r = top k -> maybe
     | (_, r) when r = top k -> maybe
     | _ ->
         if List.exists (fun(r,s)-> List.exists (fun(r2,s2)-> r == r2 && SI.eq k s s2 <> SI.no) y) x
         then maybe
         else no

  let equal x y =
    if x == y then true
    else x = y

  let union x y =
    if equal x y then x
    else
      let k = width x in
      if not (k = width y) then raise (Invalid_argument "bitwidth");
      if x = top k || y = top k then top k else
        let h = Hashtbl.create (List.length x + List.length y) in
        let add (r,si) =
          try Hashtbl.replace h r (SI.union (Hashtbl.find h r) si)
          with Not_found ->
            Hashtbl.add h r si
        in
        List.iter add x;
        List.iter add y;
        Hashtbl.fold (fun k v r -> (k,v)::r) h []

  let intersection x y =
    if equal x y then x
    else let k = width x in
         if not (k = width y) then raise (Invalid_argument "bitwidth");
         if x = top k then y
         else if y = top k then x
         else let hx = Hashtbl.create (List.length x) in
              let add (r,si) =
                Hashtbl.add hx r si
              in
              List.iter add x;
              let o = List.fold_left
                (fun l (r,si) ->
                  try (r, SI.intersection si (Hashtbl.find hx r))::l
                  with Not_found -> l)
                [] y in
              if o = [] then empty k else o

  let widen x y =
    if equal x y then x
    else let k = width x in
         if debug () then (assert (k = width y));
         if x = top k || y = top k then top k else
           let h = Hashtbl.create (List.length x + List.length y) in
           let add (r,si) =
             try Hashtbl.replace h r (SI.widen (Hashtbl.find h r) si)
             with Not_found ->
               Hashtbl.add h r si
           in
           List.iter add x;
           List.iter add y;
           Hashtbl.fold (fun k v r -> (k,v)::r) h []

  let fold f vs init =
    List.fold_left (fun a (r,si) -> SI.fold (fun v -> f (r,v)) si a) init vs

  let concrete ?max vs =
    let get_value (r,o) (l,ctr) =
      (match max with
      | Some x -> if ctr > x then raise Exit
      | None -> ());
      if r == global then
        o::l, ctr+1
      else raise Exit in
    try
      let l,_ = fold get_value vs ([],1) in
      Some l
    with Exit -> None

  let numconcrete vs =
    fold (fun _ a -> a +% bi1) vs bi0

  let binop_to_vs_function = function
    | PLUS -> add
    | MINUS -> sub
    | AND -> logand
    | OR -> logor
    | XOR -> logxor
    | EQ -> eq
    | TIMES
    | DIVIDE
    | SDIVIDE
    | MOD
    | SMOD
    | LSHIFT
    | RSHIFT
    | ARSHIFT
    | NEQ
    | LT
    | LE
    | SLT
    | SLE as bop
      -> si_to_vs_binop_function (SI.binop_to_si_function bop)

  let unop_to_vs_function = function
    | NEG
    | NOT as unop
      -> si_to_vs_unop_function (SI.unop_to_si_function unop)

  let cast_to_vs_function = function
    | CAST_UNSIGNED
    | CAST_SIGNED
    | CAST_HIGH
    | CAST_LOW as ct
      -> si_to_vs_cast_function (SI.cast_to_si_function ct)

end

(** Abstract Store *)
module MemStore = struct
  type aloc = VS.region * big_int
  module M1 = BatMap.Make(struct type t = VS.region let compare = Var.compare end)
  module M2 = BatMap.Make(struct type t = big_int let compare = Big_int_Z.compare_big_int end)

  (** This implementation may change... *)
  type t = VS.t M2.t M1.t


  let top = M1.empty

  (** Fold over all addresses in the MemStore *)
  let fold f ae i =
    M1.fold (fun r m2 a -> M2.fold (fun i vs a -> f (r,i) vs a) m2 a) ae i

  let pp p a =
    p "Memory contents:\n";
    fold (fun (r,i) vs () ->
      let region = if r == VS.global then "$" else Pp.var_to_string r in
      p (Printf.sprintf " %s[%s] -> %s\n" region (~% i) (VS.to_string vs))) a ();
    p "End contents.\n"

  let rec read_concrete k ?o ae (r,i) =
    try
      let v = M2.find i (M1.find r ae) in
      let w = VS.width v in
      assert (w mod 8 = 0);
      if w = k then v
      else (
        (* We wanted to read k bits, but read w instead. Let's try to
           read from i+w/8 and get the rest. *)
        if w > k then
          (* We read too many bytes: use extract *)
          VS.top k
        else
          (* We read too few bytes: use concat
             XXX: Handle address wrap-around properly
          *)
          let rest = read_concrete (k-w) ?o ae (r, i+%((bi w)/% bi8)) in
          (* XXX: Endianness *)
          (* let () = dprintf "Concatenating %Ld %s and %s ->" i (VS.to_string rest) (VS.to_string v) in *)
          VS.concat k rest v)
    with Not_found ->
      VS.top k

  let read k ?o ae = function
    | v when v = VS.empty k -> VS.empty k
    | addrs -> (* FIXME: maybe shortcut this *)
      try
        let res =
          VS.fold
            (fun v a ->
              match a with
            | None -> Some (read_concrete k ?o ae v)
            | Some a ->
              if a = VS.top k then raise Exit
              else
                Some (VS.union (read_concrete k ?o ae v) a)
            ) addrs None
        in
        match res with
        | Some x -> x
        | None -> failwith "MemStore.read impossible address"
      with Exit -> VS.top k

  let widen_region r =
    match !mem_max with
    | Some m ->
      if M2.cardinal r > m then M2.empty
      else r
    | None -> r

  let widen_mem m =
    M1.map (fun r -> widen_region r) m

  let write_concrete_strong k ae (r,i) vl =
    if vl = VS.top k then
      try
        let m2 = M1.find r ae in
        let m2' = M2.remove i m2 in
        if M2.is_empty m2' then M1.remove r ae else M1.add r m2' ae
      with Not_found -> ae
    else
      let m2 = try M1.find r ae with Not_found -> M2.empty in
      (* Don't overwrite the old value if it's the same; this wastes
         memory in the applicative data structure. *)
      if (try M2.find i m2 = vl with Not_found -> false)
      then ae
      else M1.add r (M2.add i vl m2) ae

  let write_concrete_weak k ae addr vl =
    write_concrete_strong k ae addr (VS.union vl (read_concrete k ae addr))

  let write_concrete_intersection k ae addr vl =
    write_concrete_strong k ae addr (VS.intersection vl (read_concrete k ae addr))

  let write_concrete_weak_widen k ae addr vl =
    write_concrete_strong k ae addr (VS.widen vl (read_concrete k ae addr))

  let write k ae addr vl =
    let width = VS.width addr in
    if addr = VS.top width then (
      if vl = VS.top k then top
      else match !mem_max with
      | None -> fold (fun addr v a -> write_concrete_weak k a addr vl) ae ae
      | Some _ -> top
    ) else match addr with
      | [(r, ((k,_,_,_) as o))] when o = SI.top k ->
        (* Set this entire region to Top *)
        M1.remove r ae
      | [(r, (_,z,x,y))] when x = y && z = bi0 ->
        write_concrete_strong k ae (r,x) vl
      | _ ->
        (match !mem_max with
        | Some m ->
          if VS.size k addr > bi m then top
          else widen_mem (VS.fold (fun v a -> write_concrete_weak k a v vl) addr ae)
        | None -> widen_mem (VS.fold (fun v a -> write_concrete_weak k a v vl) addr ae))

  let write_intersection k ae addr vl =
    match addr with
    | [(r, (_,z,x,y))] when x = y && z = bi0 ->
      write_concrete_intersection k ae (r,x) vl
    | _ ->
      (* Since we don't know what location is getting the
         intersection, we can't do anything. *)
      ae

  let equal x y =
    if x == y then true
    else M1.equal (M2.equal (=)) x y

  let merge_region ~inclusive ~f x y =
    if M2.equal (=) x y then x
    else
      M2.merge (fun a v1 v2 -> match v1, v2, inclusive with
      | Some v1, Some v2, _ ->
        (* Note: Value sets are not guaranteed to be the same width *)
        (try Some(f v1 v2)
         with Invalid_argument "bitwidth" -> None)
      | (Some _ as s), None, true
      | None, (Some _ as s), true -> s
      | Some _, None, false
      | None, Some _, false -> None
      | None, None, _ -> None) x y

  let merge_mem ~inclusive ~f =
    M1.merge (fun r v1 v2 -> match v1, v2, inclusive with
    | Some v1, Some v2, _ -> Some (merge_region ~inclusive ~f v1 v2)
    | (Some _ as s), None, true
    | None, (Some _ as s), true -> s
    | Some _, None, false
    | None, Some _, false -> None
    | None, None, _ -> None)

  let intersection (x:t) (y:t) =
    if equal x y then x
    else merge_mem ~inclusive:true ~f:VS.intersection x y

  let union (x:t) (y:t) =
    if equal x y then x
    else merge_mem ~inclusive:false ~f:VS.union x y

  let widen (x:t) (y:t) =
    if equal x y then x
    else merge_mem ~inclusive:true ~f:VS.widen x y

end

(** Abstract Environment *)
module AbsEnv = struct

  type value = [ `Scalar of VS.t | `Array of MemStore.t ]

  (** This implementation may change *)
  type t = value VM.t

  let empty = VM.empty

  let pp_value p = function
    | `Scalar s -> VS.pp p s
    | `Array a -> MemStore.pp p a

  let value_to_string v =
    let b = Buffer.create 57 in
    let p = Buffer.add_string b in
    pp_value p v;
    Buffer.contents b

  let pp p m =
    VM.iter (fun k v ->
      p ("\n " ^ (Pp.var_to_string k) ^ " -> ");
      pp_value p v;
    ) m

  let to_string m =
    let b = Buffer.create 57 in
    let p = Buffer.add_string b in
    pp p m;
    Buffer.contents b

  let value_equal x y = match x,y with
    | (`Scalar x, `Scalar y) -> VS.equal x y
    | (`Array x, `Array y) -> MemStore.equal x y
    | _ -> failwith "value_equal"

  let equal x y =
    if x == y then true
    else VM.equal (value_equal) x y

  let do_find_vs_int ae v =
    match VM.find v ae with
    | `Scalar vs -> vs
    | _ -> failwith "type mismatch"

  let do_find_vs ae v =
    try do_find_vs_int ae v
    with Not_found -> VS.top (bits_of_width (Var.typ v))

  let do_find_vs_opt ae v =
    try Some(do_find_vs_int ae v )
    with Not_found -> None

  (* let astval2vs ae = function *)
  (*   | Int(i,t) -> VS.of_bap_int (int64_of_big_int i) t *)
  (*   | Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)") *)
  (*   | Var v -> do_find_vs ae v *)

  let do_find_ae_int ae v =
    match VM.find v ae with
      | `Array ae -> ae
      | _ -> failwith "type mismatch"

  let do_find_ae ae v =
    try do_find_ae_int ae v
    with Not_found -> MemStore.top

  let do_find_ae_opt ae v =
    try Some(do_find_ae_int ae v)
    with Not_found -> None
end  (* module AE *)

type options = { initial_mem : (addr * char) list;
                 sp : Var.t;
                 mem : Var.t;
               }
