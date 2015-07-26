open BatPervasives
open OUnit

open Lnf_test
open Lnf

module L = Lnf_sreedhar.Make(G)

let n = G.G.V.create;;

let v0 = n 0
and va = n 1
and vb = n 2
and vc = n 3
and vd = n 4
and ve = n 5
and vf = n 6
and vg = n 7
and vh = n 8
and vend = n 9

let ramalingam_fig2 : test = (v0, [
  (v0, [va; vend]);
  (va, [vb; vc]);
  (vb, [vd]);
  (vc, [ve]);
  (vd, [vb; ve; vend]);
  (ve, [vc; vd; vend]);
  (vend, [])
], [
  {headers=[vb; vc; vd; ve]; body=[vb; vc; vd; ve]; children=[]}
])

let suite = "Sreedhar" >:::
  [
    "ramalingam_fig2_test" >:: (fun () -> run_test L.lnf ramalingam_fig2);
  ]
