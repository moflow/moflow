(** Interface to loop nesting forest (LNF) algorithms. *)

(** Steensgard *)
module Steensgard : Lnf.MakeType

(** Havlak *)
module Havlak : Lnf.MakeType

(** Reduced Havlak *)
module Reduced_Havlak : Lnf.MakeType

(** Sreedhar *)
module Sreedhar : Lnf.MakeType

(** A list of all supported LNF algorithms. *)
val lnflist : (string * (module Lnf.MakeType)) list
