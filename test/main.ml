open OUnit2
open Uno

let tests = "uno test suite" >::: []
let _ = run_test_tt_main tests
