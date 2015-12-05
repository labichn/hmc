open Spaces

let main () =
  let param_test  = ref false
  and param_infer = ref false in
  Arg.parse
    ["--test",      Arg.Set param_test,      "      -- runs tests for inference";
     "--infer",     Arg.Set param_infer,     "     -- type inference";]
    (fun anon_arg -> print_endline ("I don't know what to do with "^anon_arg^"..."); exit 1)
    "still no usage message";
  if !param_infer
  then Infer.repl ()
  else if !param_test
  then Test.test ()

let _ = main ()
