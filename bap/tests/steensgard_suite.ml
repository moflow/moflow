open BatPervasives
open OUnit
open Lnf_test

module L = Lnf_steensgard.Make(G)

open Lnf

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

let steensgard_fig2 : test = (v0, [
  (v0, [va; vend]);
  (va, [vb; vc]);
  (vb, [vc]);
  (vc, [vd]);
  (vd, [vb]);
  (vend, [])
], [{headers=[vb; vc]; body=[vb; vc; vd]; children=[]}])

let steensgard_fig3 : test = (v0, [
  (v0, [va; vend]);
  (va, [vb]);
  (vb, [vc]);
  (vc, [vd]);
  (vd, [vb; vc]);
  (vend, [])
], [{headers=[vb]; body=[vb; vc; vd]; children=[
  {headers=[vc]; body=[vc; vd]; children=[]}
]}])

let ramalingam_fig2 : test = (v0, [
  (v0, [va; vend]);
  (va, [vb; vc]);
  (vb, [vd]);
  (vc, [ve]);
  (vd, [vb; ve; vend]);
  (ve, [vc; vd; vend]);
  (vend, [])
], [
  {headers=[vb; vc]; body=[vb; vc; vd; ve]; children=[
    {headers=[vd; ve]; body=[vd; ve]; children=[]}
  ]}
])

let suite = "Steensgard" >:::
  [
    "steensgard_fig2_test" >:: (fun () -> run_test L.lnf steensgard_fig2);
    "steensgard_fig3_test" >:: (fun () -> run_test L.lnf steensgard_fig3);
    "ramalingam_fig2_test" >:: (fun () -> run_test L.lnf ramalingam_fig2);
  ]

