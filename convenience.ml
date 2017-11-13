open Verif
open Smtlib
open Helpers
open Imp
(* Helper functions *)
let run_wp test_name s expected = 
  let (pre,cmd,post) = from_string s in
  let result, loop_cond = wp cmd post in
  if result = expected then true else
    (Printf.printf "[!] Error: In test %s\n    
    Found result: %s 
    but expected: %s\n" test_name (show_bexp result) (show_bexp expected); false)

let run_verify test_name s expected = 
  let (pre,cmd,post) = from_string s in
  let result = verify pre cmd post in
  result = expected

(* These are some convenience methods and constants to make tests more readable *)
let x = AVar "x"
let y = AVar "y"
let z = AVar "z"

let zero = AConst 0
let one = AConst 1
let two = AConst 2

let btrue = BConst true
let bfalse = BConst false

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

(* Note: Change this line if you implemented your => in a different order,
or your weakest preconditions will fail a bunch of tests. *)
let bimp p q = BOr (BNot p, q)
(* Note: Change this line if you implemented your if in a different order,
or your weakest preconditions will fail a bunch of tests. *)
let bif b wp_c1 wp_c2 = band (bimp b wp_c1) (bimp (neg b) wp_c2)