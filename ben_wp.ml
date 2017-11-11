(*
 * Tests written by ben for method wp
 *)

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
let assign8 = "ensures z == 7; a = 2 * 3; z = a; a = 1;  z = a + z;"
let assign9 = "ensures z == 7; a = 2 * 3; z = a; a = 1;  z = a + z; z = z;"

let if1     = "ensures z == 0; a = 0; if (a == 0) z = 0; else z = 0;"
let if2     = "ensures r >= 0; if (x < 0) r = 0 - x; else r = x;"
let if3     = "ensures r >= 0; 
if (a < b) {
  r = 0;
} else if (b < a) 
{ 
  r = 1; 
} else r = 2;
"


let skip1   = "requires true ; ensures true ; skip;"
let skip2   = "requires true ; ensures false; skip;"

(* Helper functions *)
let run_wp test_name s expected = 
  let (pre,cmd,post) = from_string s in
  let result = wp cmd post in
  if result = expected then true else
    (Printf.printf "[!] Error: In test %s\n    
    Found result: %s 
    but expected: %s\n" test_name (show_bexp result) (show_bexp expected); false)

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
let%TEST "assign8" = run_wp "assign8" assign8 (BCmp (Eq, AOp (Add, AConst 1, AOp (Mul, AConst 2, AConst 3)), AConst 7))
let%TEST "assign9" = run_wp "assign9" assign9 (BCmp (Eq, AOp (Add, AConst 1, AOp (Mul, AConst 2, AConst 3)), AConst 7))

(* These are some convenience methods and constants to make tests more readable *)
let x = AVar "x"
let y = AVar "y"
let z = AVar "z"

let zero = AConst 0
let one = AConst 1
let two = AConst 2

let neg b = BNot b
let band a b = BAnd (a,b)
let bor a b = BOr (a,b)

let equals a b = BCmp (Eq, a, b)
let lt a b = BCmp (Lt, a, b)
let lte a b = BCmp (Lte, a, b)
let gt a b = BCmp (Gt, a, b)
let gte a b = BCmp (Gte, a, b)

let add a b = AOp (Add, a, b)
let sub a b = AOp (Sub, a, b)
let mul a b = AOp (Mul, a, b)

let zero_eq_zero = equals zero zero
let nzez_or_zez = bor (neg zero_eq_zero) (zero_eq_zero) (* !0 == 0 || 0 == 0 *)
let zez_or_zez  = bor zero_eq_zero zero_eq_zero (* 0 == 0 || 0 == 0 *)
let%TEST "if1"     = run_wp "if1"     if1     (BAnd (nzez_or_zez, zez_or_zez))

let x_lt_zero  = lt x zero  (* x < 0 *)
let zero_sub_x = sub zero x (* 0 - x *)
let zero_sub_x_ge_zero = gte zero_sub_x zero (* 0 - x >= 0 *)
let not_x_lt_zero = neg x_lt_zero            (* !(x < 0) *)

(* !(x < 0) || (0 - x >= 0) *)
let clause1 = bor not_x_lt_zero zero_sub_x_ge_zero 
let x_ge_zero = gte x zero
let clause2 = bor x_lt_zero x_ge_zero
(* if2: ((! x < 0) || 0 - x >= 0) && ((x < 0) || x >= 0) *)

let%TEST "if2"     = run_wp "if2"     if2 (BAnd (clause1, clause2))

(* TODO: Calculate if3's expected value and format it *)
let%TEST "if3"     = run_wp "if3"     if3 (BAnd (clause1, clause2))

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
