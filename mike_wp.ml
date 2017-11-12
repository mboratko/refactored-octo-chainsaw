(*
 * Tests written by Mike for method wp
 *)

open Verif
open Smtlib
open Helpers
open Imp
open Convenience

module PTest = Ppx_test.Test

(* Tests of wp *)

[%%TEST
  run_wp "No conditions" "skip;" (btrue);;
  run_wp "Simple skip" "ensures x < y; skip;" (lt x y);;
  run_wp "Assign constant right" "ensures x < 1; x = 2;" (lt two one);;
  run_wp "Assign constant left" "ensures 1 < x; x = 2;" (lt one two);;
  run_wp "Assign var left" "ensures y < x; y = 2;" (lt two x);;
  run_wp "Assign var right" "ensures y < x; x = 2;" (lt y two);;
  run_wp "Assign var right" "ensures y < x; x = 2;" (lt y two);;
  run_wp "Two steps" "ensures y < x; x = 1; y = 2;" (lt two one);;
  run_wp "Swap (multiple steps)" "ensures x == 2 && y == 1; c = x; x = y; y = c;"
    (band (equals y two) (equals x one));;
  run_wp "Simple if" "ensures y == 1; if (true) y=1; else y=2;"
    (bif btrue (equals one one) (equals two one));;
  run_wp "Mixed assignment if" "ensures x == y; if (x > y) y = x; else x = y;"
    (bif (gt x y) (equals x x) (equals y y));;
]

(* Tests of verify *)

[%%TEST
  run_verify "Bad Absolute Value"
    "ensures y > 0; if (x < 0) y = 0 - x; else y = x;"
    false;;
  run_verify "Trivial superfluous conditions"
    "ensures y > 0 || y < 0 || y ==0; skip;"
    true;;
]