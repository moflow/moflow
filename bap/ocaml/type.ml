(** Type declarations for BAP.

    @author Ivan Jager
*)

(** Addresses are big_ints *)
type addr = Big_int_Z.big_int

(** Labels are program locations that can be jumped to. *)
type label = 
  | Name of string (** For named labels*)
  | Addr of addr (** For addresses. Cast REG_type as unsigned when comparing. *)

(** The IR type of a BAP expression *)
type typ =
  | Reg of int (** an N-bit bitvector (use 1 for booleans). *)
  | TMem of typ * typ (** Memory of given index type, element type. *)
  | Array of typ * typ (** Array of index type, element type. *)

let reg_1 = Reg 1
and reg_8 = Reg 8
and reg_16 = Reg 16
and reg_32 = Reg 32
and reg_64 = Reg 64
and reg_128 = Reg 128
and reg_256 = Reg 256

(** [Array] memories can only be updated or accessed in terms of
    their element type, which is usually [Reg 8].  [TMem] memories
    can be updated or accessed by any type, for instance both [Reg 8]
    and [Reg 32].  Native code is generally lifted with [TMem]
    memories for simplicity, and converted to [Array] type when
    needed.  SMT solvers require [Array] type memories. *)

(** Different forms of casting *)
type cast_type =
  | CAST_UNSIGNED (** 0-padding widening cast. *)
  | CAST_SIGNED (** Sign-extending widening cast. *)
  | CAST_HIGH (** Narrowning cast. Keeps the high bits. *)
  | CAST_LOW (** Narrowing cast. Keeps the low bits. *)

(** Binary operations implemented in the IR *)
type binop_type =
  | PLUS (** Integer addition. (commutative, associative) *)
  | MINUS (** Subtract second integer from first. *)
  | TIMES (** Integer multiplication. (commutative, associative)*)
  | DIVIDE (** Unsigned integer division. *)
  | SDIVIDE (** Signed integer division. *)
  | MOD (** Unsigned modulus. *)
  | SMOD (** Signed modulus. *)
  | LSHIFT (** Left shift. *)
  | RSHIFT (** Right shift, fill with 0. *)
  | ARSHIFT (** Right shift, sign extend. *)
  | AND (** Bitwise and. (commutative, associative) *)
  | OR (** Bitwise or. (commutative, associative) *)
  | XOR (** Bitwise xor. (commutative, associative) *)
  | EQ (** Equals (commutative) (associative on booleans) *)
  | NEQ (** Not equals (commutative) (associative on booleans) *)
  | LT (** Unsigned less than *)
  | LE (** Unsigned less than or equal to *)
  | SLT (** Signed less than *)
  | SLE (** Signed less than or equal to *)
                  

(** Unary operations implemented in the IR *)
type unop_type =
  | NEG (** Negate (2's complement) *)
  | NOT (** Bitwise not *)


(** The position of a statement in a source file *)
type pos = (string * int)

(** {5 Extra attributes} *)

type taint_type = Taint of int
type usage = RD | WR | RW

(** Information about a concrete operand from a trace *)
type context = 
 {
   name  : string;
   mem   : bool;
   t     : typ;
   index : addr;
   value : Big_int_Z.big_int;
   usage : usage;
   taint : taint_type
 }

(** Attributes are extra information contained in a [stmt]. *)
type attribute = 
  | Pos of pos  (** The position of a statement in the source file *)
  | Asm of string (** Assembly representation of the following IL code *)
  | Address of addr (** The address corresponding to lifted IL. *)
  | Target of label (** An address this insn may jump to (esp. function specials) *)
  | Liveout (** Statement should be considered live by deadcode elimination *)
  | StrAttr of string (** Generic printable and parseable attribute *)
  | NamedStrAttr of string * string (** Generic printable and parseable attribute *)
  | Context of context         (** Information about the
                                   instruction operands from a
                                   trace. *)
  | ThreadId of int (** Executed by a specific thread *)
  | ExnAttr of exn (** Generic extensible attribute, but no parsing *)
  | InitRO (** The memory in this assignment is stored in the binary *)
  | Synthetic (** Operation was added by an analysis *)
  | SpecialBlock (** Start of a special block *)
  | TaintIntro of int * string * int

type attributes = attribute list

(** Visitors are a systematic method for exploring and changing
    objects of type ['a].  In BAP, ['a] can be [stmt], [exp], etc. The
    children of statements are expressions; the children of
    expressions are subexpressions and variables. *)
type 'a visit_action =
  | SkipChildren (** Do not visit the children. Return the node
                     as is. *)
  | DoChildren      (** Continue exploring children of the current node. Changes to children will propagate up. *)
  | ChangeTo of 'a  (** Replace the current object with the specified one. *)
  | ChangeToAndDoChildren of 'a (** Replace the current object with
                                    the given one, and visit children
                                    of the {b replacement} object. *)

(** Specifies whether generated VCs will be evaluated for
    satisfiability or validity. Alternatively, quantifiers can be
    used. *)
type formula_mode = Sat | Validity | Foralls
