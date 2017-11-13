open Imp

let sep_top = "+------------------------------------------------------------------------------+"
let sep_bot = "+==============================================================================+"
let print_sep s = Printf.printf "%s\n    %s\n%s\n" sep_top s sep_bot
let show_cmp (op:cmp) = match op with
  | Lt -> "<"
  | Gt -> ">"
  | Eq -> "=="
  | Lte -> "<="
  | Gte -> ">="

let show_aop (op:aop) = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"

let rec show_aexp (a:aexp) = match a with
  | AConst n -> string_of_int n
  | AVar v -> v
  | AOp (op, a1, a2) -> "(" ^ (show_aexp a1) ^ " " ^ (show_aop op) ^ " " ^ (show_aexp a2) ^ ")"

let rec show_bexp (b:bexp) = match b with
  | BConst x -> if x then "true" else "false"
  | BAnd (x,y) -> "(" ^ (show_bexp x) ^ ") && (" ^ (show_bexp y) ^ ")"
  | BOr (x,y) -> "(" ^ (show_bexp x) ^ ") || (" ^ (show_bexp y) ^ ")"
  | BNot x -> "!(" ^ (show_bexp x) ^ ")"
  | BCmp (c,x,y) -> (show_aexp x) ^ " " ^ (show_cmp c) ^ " " ^ (show_aexp y)

let rec show_cmd (cmd:cmd) : string = match cmd with 
  | CSkip -> "skip;"
  | CAbort -> "abort;"
  | CAssign (s, a) -> s ^ " = " ^ (show_aexp a) ^ ";"
  | CIf (b, c1, c2) -> "if (" ^ (show_bexp b) ^ ") { " ^ (show_cmd c1)
      ^ " } else { " ^ (show_cmd c2) ^ " } " 
  | CWhile (cond, invar, c) -> "while " ^ (show_bexp cond) ^
          " invariant " ^ (show_bexp invar) ^ "{ " ^ (show_cmd c) ^ " }"
  | CSeq (c1, c2) -> (show_cmd c1) ^ " " ^ (show_cmd c2)