(*
{ header }
let ident = regexp …
rule entrypoint [arg1… argn] =
  parse regexp { action }
      | …
      | regexp { action }
and entrypoint [arg1… argn] =
  parse …
and …
{ trailer }
*)

{
  open Lexing
  open Parser
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- 
      { pos with
          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
          Lexing.pos_bol = pos.Lexing.pos_cnum;
      }

}

let dig = ['0'-'9']
let inte = '-'? dig+
let var_char = ['a'-'z' '_' 'A'-'Z' '\'' '*' '\\' '/' '!' '@' '#' '$' '%' '^' '&']
let var = var_char+
let eol = ['\n'] 

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | eol        { incr_linenum lexbuf; token lexbuf } 
  | ','        { COMMA }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  |  "true"     { TRUE }
  |  "false"    { FALSE }
  |  "="       { EQ }
  |  "/"      { DIV }
  |  "if"       { IF }
  |  "add1"     { SUCC }
  |  "+"     { PLUS }
  |  "*"     { MULT }
  | "true" | "True"     { TRUE }
  | "false" | "False"    { FALSE }
  | "eq" | "Eq"        { EQ }
  | "div" | "Div"      { DIV }
  | "if" | "If"        { IF }
  | "succ" | "Succ"     { SUCC }
  | "plus" |"Plus"     { PLUS }
  | "mult" | "Mult"     { MULT }
  | "lambda" | "Lam" { LAM }
  | "!" | "Mon" { MON }
  | "?" | "Pred" { PRED }
  | "->" | "CArr" { CARR}
  | "let" { LET }
  | "and" { AND }
  | "not" { NOT }
  | "or" { OR }
  | var as v   { VAR v }
  | inte as i   { INT i }
  | eof        { EOF }
