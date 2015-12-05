open Shared
open Spaces

let repl desc handle_exp () : unit =
  print_endline ("Welcome to the B language's "^desc^" REPL! (C-d to exit)\n");
  print_endline Exp.syntax; print_newline ();
  print_endline "You can add new variables to the environment with: \"set var (int | bool)\".\n\n";
  let env = ref VEnv.empty in
    (* quick and dirty environment modification: "set x 4" means x = 4 in this repl. *)
  let handle_set str =
    try (* gross *)
      let pair  = String.sub str 4 ((String.length str)-4) in
      let ind   = String.index pair ' ' in
      let var   = String.sub pair 0 ind in
      let value = String.sub pair (ind+1) ((String.length pair)-(ind+1)) in
      try
        env := VEnv.add var (`Nat (int_of_string value)) !env;
        print_endline (var^" = "^value);
        true
      with _ ->
        env := VEnv.add var (`Bool (bool_of_string value)) !env;
        print_endline (var^" = "^value);
        true
    with _ -> false in
  let rec loop () =
    try
      print_string "> ";
      let input  = read_line () in
      if handle_set input then
        loop ()
      else if handle_exp (Parse.parse input) !env then
        loop ()
      else
        (print_endline "I don't know what to say to that...";
         loop ())
    with
    | Parsing.Parse_error -> ()
    | End_of_file -> print_endline "\nbuh-bye!"
    | exn -> print_endline ("An exception occurred trying to handle that last input:\n"^
                               (Printexc.to_string exn)) in
  loop ()

(* this would be fine, except for set. this will work when the env doesn't have to be injected.

   module type S = sig
   val repl : string -> Exp.t -> unit -> unit
   end
   module Make(V : )(E : MapExt.S with type key = string) = struct
   let repl desc handle_exp () : unit =
   print_endline ("Welcome to the B language's "^desc^" REPL! (C-d to exit)\n");
   print_endline Exp.syntax; print_newline ();
   print_endline "You can add new variables to the environment with: \"set var (int | bool)\".\n\n";
   let env = ref E.empty in
    (* quick and dirty environment modification: "set x 4" means x = 4 in this repl. *)
   let handle_set str =
   try (* gross *)
   let pair  = String.sub str 4 ((String.length str)-4) in
   let ind   = String.index pair ' ' in
   let var   = String.sub pair 0 ind in
   let value = String.sub pair (ind+1) ((String.length pair)-(ind+1)) in
   try
   env := E.add var (`Nat (int_of_string value)) !env;
   print_endline (var^" = "^value);
   true
   with _ ->
   env := E.add var (`Bool (bool_of_string value)) !env;
   print_endline (var^" = "^value);
   true
   with _ -> false in
   let rec loop () =
   try
   print_string "> ";
   let input  = read_line () in
   if handle_set input then
   loop ()
   else if handle_exp (Parse.parse input) !env then
   loop ()
   else
   (print_endline "I don't know what to say to that...";
   loop ())
   with
      | Parsing.Parse_error -> ()
      | End_of_file -> print_endline "\nbuh-bye!"
      | exn -> print_endline ("An exception occurred trying to handle that last input:\n"^
                                 (Printexc.to_string exn)) in
    loop ()
end
*)

(*
maybe worth hacking on sometime later

module type REPLABLE  = sig
  type i
  type o
  val parse    : string -> i
  val f        : i -> o
  val show_err : (exn -> string) option
  val message  : string
end

module type S = sig
  val repl : string -> unit -> unit
end

module Make(I : CMP)(O : CMP)
  (R : REPLABLE with type i = I.t and type o = O.t) : S = struct

  let repl ?verbose:false (msg : string) () : unit =
    print_endline msg;
    print_newline ();
    print_endline "C-d to exit";
    print_newline ();
    
    let rec loop = function
      | () ->
        print_string "> ";
        let input = read_line () in
        let iinst = parse input in
        (if verbose then print_endline ("Parsed input: "^(I.show iinst)));
        let oinst = f iinst in
        print_endline (O.show oinst);
        loop ()
      | exception exn -> (match R.err with
        | Some handle_exn -> handle_exn exn; loop ()
        | None ->
          print_endline ("Unhandled exception: "^(Printexc.to_string exn));
          loop ()) in
    loop ()

end




module I = struct
  type t = VEnv.t * Exp.t
  [@@deriving Eq, Ord, Show]
  let hash = Shared.hash
end
module R = REPL.Make(I)(S)
  (struct
    type i = I.t
    type o = O.t
    let parse = Parse.parse
    let f (env, exp) =
    let err = Some (function
    Parsing.Parse_failure _ ->
    (* how can I get set working in here? *))
    let message = "Welcome to the B language's REPL! (C-d to exit)\n"^
      "B ::= (add1 B)\n"^
      "   |  (sub1 B)\n"^
      "   |  (+ B B)\n"^
      "   |  (* B B)\n"^
      "   |  (/ B B)\n"^
      "   |  (= B B)\n"^
      "   |  (if B B B)\n"^
      "   |  bool | int | var\n"^
      "where bool ::= true | false\n"^
      "      int  ::= -?[0..9]+\n"^
      "  and var  ::= [a..Z]+\n"^
      "You can add new variables to the environment with: \"set var (int | bool)\".\n"
   end)
*)
