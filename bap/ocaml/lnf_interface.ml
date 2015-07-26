open Lnf

module Steensgard = Lnf_steensgard.Make
module Havlak = Lnf_havlak.Make
module Reduced_Havlak = Lnf_reduced_havlak.Make
module Sreedhar = Lnf_sreedhar.Make

let lnflist =
  ("steensgard", (module Steensgard : MakeType))
  :: ("havlak", (module Havlak : MakeType))
  :: ("reduced_havlak", (module Reduced_Havlak : MakeType))
  :: ("sreedhar", (module Sreedhar : MakeType))
  :: []
