type t = [ `Nat  of int
         | `Bool of bool
         | `Var  of string
         | `Succ of t
         | `Plus of t * t
         | `Mult of t * t
         | `Div  of t * t
         | `Eq   of t * t
         | `And  of t * t
         | `Not  of t
         | `Or   of t * t
         | `If   of t * t * t
         | `Lam  of string * t
         | `App  of t * t
         | `Let  of string * t * t
         | `Mon  of t * t
         | `Pred of t
         | `CArr of t * t ]
[@@deriving eq, ord, show]

let hash = Shared.hash

let rec show = function
  | `Nat  i            -> string_of_int i
  | `Bool b            -> string_of_bool b
  | `Var  s            -> s
  | `Succ t            -> "(add1 "^(show t)^")"
  | `Plus (t, t')      -> "(+ "^(show t)^" "^(show t')^")"
  | `Mult (t, t')      -> "(* "^(show t)^" "^(show t')^")"
  | `Div  (t, t')      -> "(/ "^(show t)^" "^(show t')^")"
  | `Eq   (t, t')      -> "(= "^(show t)^" "^(show t')^")"
  | `And  (t, t')      -> "(and "^(show t)^" "^(show t')^")"
  | `Not  (t)          -> "(not "^(show t)^")"
  | `Or   (t, t')      -> "(or "^(show t)^" "^(show t')^")"
  | `If   (t, t', t'') -> "(if "^(show t)^" "^(show t')^" "^(show t'')^")"
  | `Lam  (x, t)       -> "(lambda "^x^" "^(show t)^")"
  | `App  (t, t')      -> "("^(show t)^" "^(show t')^")"
  | `Let  (x, t, t')   -> "(let ("^x^" "^(show t)^") "^(show t')^")"
  | `Mon  (t, t')      -> "(! "^(show t)^" "^(show t')^")"
  | `Pred t            -> "(? "^(show t)^")"
  | `CArr (t, t')      -> "(-> "^(show t)^" "^(show t')^")"

let syntax = 
  "C ::= (add1 C)\n"^
    "   |  (+ C C)\n"^
    "   |  (* C C)\n"^
    "   |  (/ C C)\n"^
    "   |  (= C C)\n"^
    "   |  (if C C C)\n"^
    "   |  (lambda var C)\n"^
    "   |  (C C)\n"^
    "   |  (! C C)\n"^
    "   |  (? C)\n"^
    "   |  (-> C C)\n"^
    "   |  bool | nat | var\n"^
    "where bool ::= true | false\n"^
    "      nat  ::= [0..9]+\n"^
    "  and var  ::= [a..Z]+"

