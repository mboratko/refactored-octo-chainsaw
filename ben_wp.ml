open Verif
open Smtlib
open Helpers
open Imp
module PTest = Ppx_test.Test
(* Programs *)
let abort1  = "requires true ; ensures false ; abort;"
let abort2  = "ensures false ; abort;"
let assign1 = "ensures true ; x = 2;"
let assign2 = "requires y > 0; ensures x + y > 2; x = 2;"
let assign3 = "requires y > 1 && x > 1; ensures z > 2; z = x * y;"
let assign4 = "ensures z == 1; a = 1; z = a;"
let assign5 = "ensures z == 3; a = 1; b = 2; z = a + b;"
let assign6 = "ensures z == 9; a = 2; b = 3; c = 4; z = a + b + c;"
let assign7 = "ensures z == 0; a = 0; a = 1; a = 2; a = 0; z = a;"
let skip1   = "requires true ; ensures true ; skip;"
let skip2   = "requires true ; ensures false; skip;"

(* Helper functions *)
let run_wp test_name s expected = 
  let (pre,cmd,post) = from_string s in
  let result = wp cmd post in
  if result = expected then true else
    (Printf.printf "[!] Error: In test %s\n    Found result: %s but expected: %s\n" test_name (show_bexp result) (show_bexp expected); false)

(* Test wp *)
let%TEST "abort1"  = run_wp "abort1"  abort1  (BConst false)
let%TEST "abort2"  = run_wp "abort2"  abort2  (BConst false)
let%TEST "assign1" = run_wp "assign1" assign1 (BConst true )
let%TEST "assign2" = run_wp "assign2" assign2 (BCmp (Gt, AOp (Add, AConst 2, AVar "y" ), AConst 2))
let%TEST "assign3" = run_wp "assign3" assign3 (BCmp (Gt, AOp (Mul, AVar "x", AVar "y" ), AConst 2))
let%TEST "assign4" = run_wp "assign4" assign4 (BCmp (Eq, AConst 1, AConst 1))
let%TEST "assign5" = run_wp "assign5" assign5 (BCmp (Eq, (AOp (Add, (AConst 1), (AConst 2))), AConst 3))
let%TEST "assign6" = run_wp "assign6" assign6 (BCmp (Eq, AOp (Add, AOp (Add, AConst 2, AConst 3), (AConst 4 )), AConst 9))
let%TEST "assign7" = run_wp "assign7" assign7 (BCmp (Eq, AConst 0, AConst 0))
let%TEST "skip1"   = run_wp "skip1"   skip1   (BConst true )
let%TEST "skip2"   = run_wp "skip2"   skip2   (BConst false)

(*
let%TEST "stress test 2 for let-poly in get field" =
    let impl = Implicit.from_string
      "let f = fun r -> r.a in
      if (f {a:true}) then f {a:1} 
      else f {a:true}"
    in try  let expl = typeinf impl 
    in (print_string ("Whoops: " ^ (pretty_exp expl) ^ "\n"); false ) 
  with Failure _ -> true
*)
