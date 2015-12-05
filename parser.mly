%{

open Lexing

%}

%token EOF LPAREN RPAREN COMMA TRUE FALSE EQ DIV IF SUCC PLUS MULT PRED LAM MON CARR APP LET AND NOT OR
%token <string> VAR
%token <string> INT

%start b

%type <Exp.t> b
%%

b:
| bexp     { $1 }
| bexp EOF { $1 }
;

var:
| VAR   { `Var $1 }
;

bexp:
| var  { $1 }
| LPAREN SUCC bexp RPAREN { `Succ $3 }
| LPAREN PLUS bexp bexp RPAREN { `Plus ($3, $4) }
| LPAREN MULT bexp bexp RPAREN { `Mult ($3, $4) }
| LPAREN   EQ bexp bexp RPAREN { `Eq   ($3, $4) }
| LPAREN   OR bexp bexp RPAREN { `Or   ($3, $4) }
| LPAREN  AND bexp bexp RPAREN { `And  ($3, $4) }
| LPAREN  NOT bexp RPAREN { `Not  $3 }
| LPAREN  DIV bexp bexp RPAREN { `Div  ($3, $4) }
| LPAREN   IF bexp bexp bexp RPAREN { `If   ($3, $4, $5) }
| LPAREN  LAM  VAR bexp RPAREN { `Lam ($3, $4) }
| LPAREN  MON bexp bexp RPAREN { `Mon ($3, $4) }
| LPAREN PRED bexp RPAREN { `Pred $3 }
| LPAREN CARR bexp bexp RPAREN { `CArr ($3, $4) }
| LPAREN  LET LPAREN VAR bexp RPAREN bexp RPAREN { `Let ($4, $5, $7) }
| LPAREN bexp bexp RPAREN { `App ($2, $3) }
| INT   { `Nat  (int_of_string $1) }
| TRUE  { `Bool true }
| FALSE { `Bool false }
;
