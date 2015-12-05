open Shared
open Spaces
open Parse

(* DHM type inference for lc + pp + h-o contracts *)

module Eq = struct
  type t = Typ.t * Typ.t
  [@@deriving eq, ord, show]
  let hash = hash
end

(* { t0 = u0; t1 = u1; ... } *)
module EqS = SetExt.Make(Eq)

let bind (o : 'a option) (f : 'a -> 'b option) : 'b option =
  match o with
  | Some x -> f x
  | None   -> None
  | exception (Match_failure _) -> None

let replace (x : string) (t : Typ.t) (te : TEnv.t) : TEnv.t =
  TEnv.fold
    (fun y t' a -> match t' with
    | _      when x = y -> TEnv.add y t  a
    | `Var z when x = z -> TEnv.add y t  a
    | _                 -> TEnv.add y t' a)
    te
    TEnv.empty

let unify (eqs0 : EqS.t) : TEnv.t option =
  let rec occurs te x : Typ.t -> bool = function
    | `Arr (t, t') -> (occurs te x t) || (occurs te x t')
    | `Con t -> occurs te x t
    | `Var y -> y = x
    | _ -> false in
  let rec unify ((eqs, te) : EqS.t * TEnv.t) : TEnv.t =
    if EqS.is_empty eqs then te
    else
      let eq   = EqS.choose eqs in
      let eqs' = EqS.remove eq eqs in
      match eq with
      | t, t' when Typ.equal t t' -> unify (eqs', te)
      | (`Var x) as t, t' when occurs te x t' ->
        let x = Typ.show t  in
        let t = Typ.show t' in
        failwith ("An expression has type "^x^
                     " but an expression was expected of type "^t^
                     ". The type variable "^x^" occurs inside of "^t^".")
      | `Var x, t' when not (occurs te x t') ->
        (match TEnv.find x te with
        | t'' -> unify (EqS.add (t', t'') eqs', te)
        | exception Not_found -> unify (eqs', TEnv.add x t' (replace x t' te)))
      | t, (`Var _ as x) -> unify (EqS.add (x, t) eqs', te)
      | `Arr (t, t'), `Arr (t'', t''') ->
        unify (EqS.add (t, t'') (EqS.add (t', t''') eqs'), te)
      | `Con t, `Con t' -> unify (EqS.add (t, t') eqs', te)
      | t, t' -> failwith ("Types "^(Typ.show t)^" and "^(Typ.show t')^" are incompatible.") in
  try Some (unify (eqs0, TEnv.empty))
  with Failure msg -> print_endline msg; None

let infer (e : Exp.t) : Typ.t option =

  let gensym  = gensym () in
  let genvar  = genvar gensym in
  let inst    = inst genvar in

  let leaf t = Some (t, EqS.empty) in

  let expand_type (t : Typ.t) (te : TEnv.t) : Typ.t option =
    let rec expand : Typ.t -> Typ.t = function
      | `Var x when TEnv.mem x te -> expand (TEnv.find x te)
      | `Arr (t, t') -> `Arr (expand t, expand t')
      | t -> t in
    try Some (expand t)
    with
    | Not_found -> None
    | Failure x -> print_endline x; None in

  let rec infer (te : TEnv.t) (e : Exp.t) : (Typ.t * EqS.t) option =
    (*    print_endline (Exp.show e);*)
    match e with
    | `Nat  _          -> leaf `Nat
    | `Bool _          -> leaf `Bool
    | `Var x           -> (try  leaf (inst (TEnv.find x te))
                           with Not_found -> failwith ("Unbound variable during inference: "^x^"!"))
    | `Succ e          -> unary  `Nat  te e
    | `Mult (e, e')    -> binary `Nat  te e e'
    | `Plus (e, e')    -> binary `Nat  te e e'
    | `Div  (e, e')    -> binary `Nat  te e e'
    | `Not  e          -> unary  `Bool te e
    | `Or   (e, e')    -> binary `Bool te e e'
    | `And  (e, e')    -> binary `Bool te e e'
    | `Eq   (e, e')    -> bind (infer te e)  (function t,  eqs  ->
                          bind (infer te e') (function t', eqs' ->
                            Some (`Bool, EqS.union eqs eqs')))
    | `If (e, e', e'') -> bind (infer te e)   (function t,   eqs   ->
                          bind (infer te e')  (function t',  eqs'  ->
                          bind (infer te e'') (function t'', eqs'' ->
                            Some (t', EqS.unions [EqS.of_list [t, `Bool; t', t'']; eqs; eqs'; eqs'']))))
    | `Lam (x, e)      -> let t = genvar () in
                          bind (infer (TEnv.add x t te) e) (function t', eqs ->
                            Some (`Arr (t, t'), eqs))
    | `App (e, e')     -> bind (infer te e)   (function t,  eqs  ->
                          bind (infer te e')  (function t', eqs' ->
                            let t'' = genvar () in
                            Some (t'', EqS.add (t, `Arr (t', t'')) (EqS.union eqs eqs'))))
    | `Let (x, e, b)   -> bind (infer te e) (function t, eqs ->
                            let pt = genpoly te t in
                            let te' = TEnv.add x pt te in
                            bind (infer te' b) (function t', eqs' ->
                              Some (t', EqS.union eqs eqs')))
    | `Pred e          -> bind (infer te e) (function t, eqs ->
                            let tvar = genvar () in
                            Some (`Con tvar, EqS.add (t, `Arr (tvar, `Bool)) eqs))
    | `CArr (e, e')    -> bind (infer te e)  (function t,  eqs  ->
                          bind (infer te e') (function t', eqs' ->
                            let tvar  = genvar () in
                            let tvar' = genvar () in
                            Some (`Con (`Arr (tvar, tvar')),
                                  EqS.unions [EqS.of_list [t, `Con tvar; t', `Con tvar']; eqs; eqs'])))
    | `Mon (e, e')     -> bind (infer te e)  (function t,  eqs  ->
                          bind (infer te e') (function t', eqs' ->
                            let t'' = genvar () in
                            Some (t', EqS.add (t, `Con t'') (EqS.union eqs eqs'))))

  and unary  (b : Typ.t) (te : TEnv.t) (e : Exp.t) : (Typ.t * EqS.t) option =
    bind (infer te e) (function t, eqs -> Some (b, EqS.add (t, b) eqs))

  and binary (b : Typ.t) (te : TEnv.t) (e : Exp.t) (e' : Exp.t) : (Typ.t * EqS.t) option =
    bind (infer te e)  (function t,  eqs  ->
    bind (infer te e') (function t', eqs' ->
      Some (b, EqS.unions [EqS.of_list [t', b; t, b]; eqs; eqs']))) in

  bind (infer TEnv.empty e) (function t, eqs ->
  bind (unify eqs) (expand_type t))

let repl : unit -> unit =
  Repl.repl "type inferring"
    (fun exp env ->
      try
        (match infer exp with
        | Some t -> print_endline (Typ.show t)
        | None   -> print_endline "Could not be typed.");
        true
      with _ -> false)

let test () : unit =
  let tests : (string * Typ.t option) list =
    ["(= (* (add1 20) (add1 21)) 1)", Some `Bool; 
     "706", Some `Nat;
     "true", Some `Bool;
     "false", Some `Bool;
     "(/ (* 557 (+ (add1 (add1 962)) (add1 543))) 658)", Some `Nat;
     "(= (add1 (* 125 112)) 137)", Some `Bool; 
     "(if 5 (add1 5) false)", None;
     "(lambda x x)", Some (`Arr (`Var "0", `Var "0"));
     "((lambda x x) 5)", Some (`Nat);
     "(lambda x (lambda y x))", Some (`Arr (`Var "0", `Arr (`Var "1", `Var "0")));
     "(lambda x (lambda x x))", Some (`Arr (`Var "0", `Arr (`Var "1", `Var "1")));
     "(lambda x (lambda y (x y)))",
     Some (`Arr (`Arr (`Var "1", `Var "2"), `Arr (`Var "1", `Var "2")));
     "(((lambda x (lambda y x)) 5) false)", Some `Nat;
     "(((lambda x (lambda y y)) 5) false)", Some `Bool;
     "(((lambda x (lambda y (+ x y))) 5) 6)", Some `Nat;
     "(= false true)", Some `Bool;
     "(or false true)", Some `Bool;
     "(and false true)", Some `Bool;
     "(not false)", Some `Bool;
     "(let (b false) b)", Some `Bool;
     "(let (b (lambda x x)) b)", Some (`Arr (`Var "1", `Var "1"));
     "((lambda x x) 5)", Some (`Nat);
     "(((lambda b b) (lambda x x)) 5)", Some (`Nat);
     "(let (y (lambda x x)) (y 5))", Some `Nat;
     "(lambda x (x x))", None;
     "((lambda x (x x))(lambda x (x x)))", None;
     "(lambda f (lambda x (f (x x))))", None;
     "(let (boolp (lambda x (or (= x true) (= x false)))) (! (? boolp) true))", Some `Bool;
     "(let (boolp (lambda x (or (= x true) (= x false)))) (! (? boolp) 5))", Some `Nat;
     "(let (natp  (lambda x (not (or (= x true) (= x false))))) (! (? natp) true))", Some `Bool;
     "(let (boolp (lambda x (+ (= x true) (= x false)))) (! (? boolp) true))", None;
     "(let (boolp (lambda x (+ 5 6))) (! (? boolp) true))", None;
     "(let (boolp  (? (lambda x (or (= x true) (= x false)))))
        (let (bitp (? (lambda y (or (= y 0) (= y 1)))))
          (! (-> boolp bitp)
             (lambda bool (if bool 0 1)))))", Some (`Arr (`Bool, `Nat));
     "(let (boolp  (? (lambda x (or (= x true) (lambda f ((lambda x (f (x x))) (lambda x (f (x x)))))))))
        (let (bitp (? (lambda y (or (= y 0) (= y 1)))))
          (! (-> boolp bitp)
             (lambda bool (if bool 0 1)))))", None;
     "(let (ident (lambda x x)) (ident (= (ident 4) 3)))", Some `Bool;
    ] in
  List.iter
    (fun (str, expected) ->
      let out = infer (parse str) in (match out, expected with
        | Some t, Some t' when Typ.equal t t' ->
          print_endline ("passed     "^str^" : "^(Typ.show t'))
        | None,   None ->
          print_endline ("passed     "^str^" has no valid type")
        | _ -> print_endline ("FAILED     "^str^(match [@warning "-8"] out, expected with
          | Some t', Some t -> " : " ^ (Typ.show t') ^ " but we expected " ^ (Typ.show t)
          | Some t', None -> " : " ^ (Typ.show t') ^ " but we expected it to have no valid type"
          | None, Some t     -> " has no valid type, but we expected " ^ (Typ.show t)))))
    tests
    
