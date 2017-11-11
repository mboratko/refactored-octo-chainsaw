(* tests.ml
 * NOTE: Tests should not be written here directly; this simply imports tests
 * and collects/runs them. To include tests, write them in `some_test_file.ml`
 * and add `open Some_test_file` to the top of this file
 *)

open Verif
open Smtlib
open Helpers
open Imp

(*  vvv  Add your test files HERE!  vvv  *)
open Ben_wp



(*  vvv  No touching!  vvv  *)
module PTest = Ppx_test.Test

let _ = Ppx_test.Test.collect ()
