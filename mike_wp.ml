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
  run_verify "Repeated variable declaration"
    "ensures x == 2; x = 1; x = 2;"
    true;;
  run_verify "Simple while loop with no invariant"
    "ensures x >= 5;
    while (x < 5)
    invariant true
    {
      x = x + 1;
    }"
    true;;
  run_verify "Bad simple while loop"
    "ensures x >= 6;
    while (x < 5)
    invariant true
    {
      x = x + 1;
    }"
    false;;

  (* Obviously we'd like to avoid this, but our current implementation
     does not account for things like "decrement" which would show that
     the loop eventually terminates.*)
  run_verify "Infinite loop"
    "requires x < 5;
    ensures x >= 5;
    while (x < 5)
    invariant true
    {
      skip;
    }"
    true;;
  
  run_verify "False invariant on enter"
  "requires x < 1;
  ensures x > 1;
  while (x < 1)
  invariant x > 1
  {
    x = x + 1;
  }"
  false;;

  run_verify "Simple invariant test"
  "ensures x == y;
  x = 10;
  y = 0;
  while (x > y)
  invariant 10 == x + y && y <= x
  {
    x = x - 1;
    y = y + 1;
  }"
  true;;

  run_verify "Catch invariant not actually invariant"
  "ensures x == y;
  x = 10;
  y = 0;
  while (x > y)
  invariant 10 == x + y && y <= x
  {
    x = x - 1;
    y = x + 1;
  }"
  false;;

  run_verify "Euclidean Algorithm"
    "requires n > 0 && m >= 0;
    ensures m == q * n + r && r >= 0 && r < n;
    r = m;
    q = 0;
    while (r >= n)
    invariant m == q * n + r && r >= 0
    {
      q = q + 1;
      r = r - n;
    }"
    true;;
]