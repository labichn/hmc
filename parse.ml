let parse (exp : string) : Exp.t =
  let lexbuf = Lexing.from_string exp in
  let print_exc () =
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Printf.eprintf "Line %d, character %d: syntax error at %s\n%!"
      line cnum (if tok = "\n" then "newline" else Printf.sprintf "%s" tok) in
  try Parser.b Lexer.token lexbuf with exc -> print_exc (); raise exc
