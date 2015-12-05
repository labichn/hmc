open Shared

(*

t ::= N
   |  2
   |  t -> t
   |  t contract

*)

module Str = struct
  type t = string
  [@@deriving eq, ord, show]
  let hash = hash
end

let gensym () : unit -> string =
  let n = ref ~-1 in
  (fun () -> let n' = !n+1 in n := n'; string_of_int n')

module rec Typ : sig
  type t = [ `Bool
           | `Nat
           | `Var  of string
           | `Arr  of t * t
           | `Con  of t
           | `Poly of XS.t * t ]
  include Comparable with type t := t
end = struct
  type t = [ `Bool
           | `Nat
           | `Var  of string
           | `Arr  of t * t
           | `Con  of t
           | `Poly of XS.t * t ]
  [@@deriving eq, ord, show]
  let hash = Shared.hash
  let rec show = function
    | `Bool  -> "Bool"
    | `Nat   -> "Nat"
    | `Var x -> "'"^x
    | `Arr (t, t') -> "("^(show t)^" -> "^(show t')^")"
    | `Con t -> "(? "^(show t)^")"
    | `Poly (bs, t) -> "(for all "^(XS.show bs)^" in "^(show t)^")"
  
end
  
and XS : SetExt.S with type elt = Typ.t =
  SetExt.Make(Typ)

let genvar (gs : unit -> string) : unit -> Typ.t =
  (fun () -> `Var (gs ()))
  
module TEnv = MapExt.Make(Str)(Typ)

let fv : Typ.t -> XS.t =
  let rec fv : Typ.t -> XS.t = function
    | `Var a -> XS.singleton (`Var a)
    | `Arr (t, t') -> XS.union (fv t) (fv t')
    | `Con t -> (fv t)
    | `Poly (bs, t) -> XS.diff (fv t) bs
    | _ -> XS.empty in
  fv

let fv_te (te : TEnv.t) : XS.t = TEnv.fold (fun _ t a -> XS.union (fv t) a) te XS.empty

let genpoly (te : TEnv.t) (t : Typ.t) : Typ.t =
  let fvte = fv_te te and
      fvt  = fv t   in
  let fvs = XS.diff fvt fvte in
  `Poly (fvs, t)

let rec beta t a t' = match t with
  | `Arr (t'', t''') -> `Arr (beta t'' a t', beta t''' a t')
  | `Con t'' -> `Con (beta t'' a t')
  | `Var _ as b when a = b -> t'
  | `Var _ -> t
  | `Poly (bs, _) when XS.mem a bs -> t
  | `Poly (bs, t'') -> `Poly (bs, beta t'' a t')
  | _ -> t

let inst (gv : unit -> Typ.t) : Typ.t -> Typ.t = function
  | `Poly (bs, t) -> XS.fold (fun b t' -> beta t' b (gv ())) bs t
  | t -> t

module rec VEnv : MapExt.S with type   key = Str.t
                            and type value = Val.t =
  MapExt.Make(Str)(Val)

and Val : Comparable with type t = [ `Bool of bool
                                   | `Nat  of int
                                   | `Clos of VEnv.t * Exp.t ] = struct
  type t = [ `Bool of bool
           | `Nat  of int
           | `Clos of VEnv.t * Exp.t ]
  [@@deriving eq, ord, show]
  let hash = hash
  let show : t -> string = function
    | `Bool b      -> string_of_bool b
    | `Nat  i      -> string_of_int  i
    | `Clos (r, e) -> "(clos "^(Exp.show e)^" "^(VEnv.show r)^")"
  let type_of (recur : VEnv.t -> Exp.t -> Typ.t) : t -> Typ.t = function
    | `Bool _ -> `Bool
    | `Nat _ -> `Nat
    | `Clos (r, e) -> recur r e

end

module Err = struct
  type t =
  [ `TypeError
  | `UnboundVar
  | `DivByZero ]
  [@@deriving eq, ord, show]
  let hash = hash
end

module Frame = struct
  type t =
  [ `Succ
  | `PlusL of Exp.t * VEnv.t
  | `PlusR of Val.t
  | `MultL of Exp.t * VEnv.t
  | `MultR of Val.t
  | `DivL  of Exp.t * VEnv.t
  | `DivR  of Val.t
  | `EqL   of Exp.t * VEnv.t
  | `EqR   of Val.t
  | `If    of Exp.t * Exp.t * VEnv.t
  | `AppL  of Exp.t * VEnv.t
  | `AppR  of Val.t
  | `MonL  of Exp.t * VEnv.t
  | `MonR  of Val.t
  | `Pred
  | `CArrL of Exp.t * VEnv.t
  | `CArrR of Val.t ]
  [@@deriving eq, ord, show]
  let hash = hash
end

module Con = struct
  type t = [ `Mt | `Con of Frame.t * t ]
  [@@deriving eq, ord, show]
  let hash = hash
end

module Ans = struct
  type t =
  [ Val.t
  | Err.t ]
  [@@deriving eq, ord, show]
  let hash = hash
end

module State = struct
  type t =
  [ Ans.t
  | `Ev of Exp.t * VEnv.t * Con.t
  | `Co of Val.t * Con.t ]
  [@@deriving eq, ord, show]
  let hash = hash
end
