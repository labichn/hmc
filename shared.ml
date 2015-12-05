let string_of_list l s_o_elt =
  if List.length l > 0 then
    let elts = List.fold_right (fun elt a -> (s_o_elt elt)^", "^a) l "" in
    "["^(String.sub elts 0 ((String.length elts)-2))^"]"
  else "[]"

let hash = Hashtbl.seeded_hash

let always = (fun _ -> true)

(* makes gensyms that produce strings of the form "cn",
   where c is a random char in 'a'..'z' and n is always
   a distinct nat, increasing from zero. *)
let mkgensym () : unit -> string =
  let n = ref 0 in
  (fun () ->
    let n' = !n in n := n'+1;
    (String.init 1 (fun _ -> (Char.chr (Random.int 26 + 97))))^
      (string_of_int n'))

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val hash : int -> t -> int
end

module SetExt = struct

  module type S = sig
    include Comparable
    include Set.S with type t := t
    val unions : t list -> t
  end

  module Make(Elt : Comparable) : S with type elt = Elt.t = struct
    include Set.Make(Elt)
    let equal s s' = compare s s' = 0
    let show s =
      let n, elts = fold (fun x (n, a) -> (n+1, (Elt.show x)^", "^a)) s (0, "") in
      if n > 0 then "{ "^(String.sub elts 0 ((String.length elts)-2))^" }"
      else "{}"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let hash seed s = fold (fun x a -> Elt.hash a x) s seed
    let to_list = elements
    let unions = List.fold_left union empty
  end

end

module MapExt = struct

  (* Just a Map with an established codomain. *)
  module type S = sig
    type key type value
    include Comparable
    val empty : t
    val is_empty : t -> bool
    val mem : key -> t -> bool
    val add : key -> value -> t -> t
    val singleton : key -> value -> t
    val remove : key -> t -> t
    val merge : (key -> value option -> value option -> value option) -> t -> t -> t
    val iter : (key -> value -> unit) -> t -> unit
    val fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (key -> value -> bool) -> t -> bool
    val exists : (key -> value -> bool) -> t -> bool
    val filter : (key -> value -> bool) -> t -> t
    val partition : (key -> value -> bool) -> t -> t * t
    val cardinal : t -> int
    val bindings : t -> (key * value) list
    val min_binding : t -> key * value
    val max_binding : t -> key * value
    val choose : t -> key * value
    val split : key -> t -> t * value option * t
    val find : key -> t -> value
    val map : (value -> value) -> t -> t
    val mapi : (key -> value -> value) -> t -> t
  end

  module Make (K : Comparable)(V : Comparable) : S with type key = K.t and type value = V.t = struct

    module M = Map.Make(K)

    type key   = K.t
    type value = V.t
    type t = value M.t
    [@compare M.compare V.compare]
    [@equal fun x x' -> compare x x' = 0]
    [@@deriving eq, ord]

    let empty : t = M.empty
    let is_empty : t -> bool = M.is_empty
    let mem : key -> t -> bool = M.mem
    let add : key -> value -> t -> t = M.add
    let singleton : key -> value -> t = M.singleton
    let remove : key -> t -> t = M.remove
    let merge : (key -> value option -> value option -> value option) -> t -> t -> t =
      M.merge
    let iter : (key -> value -> unit) -> t -> unit = M.iter
    let fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a = M.fold
    let for_all : (key -> value -> bool) -> t -> bool = M.for_all
    let exists : (key -> value -> bool) -> t -> bool = M.exists
    let filter : (key -> value -> bool) -> t -> t = M.filter
    let partition : (key -> value -> bool) -> t -> t * t = M.partition
    let cardinal : t -> int = M.cardinal
    let bindings : t -> (key * value) list = M.bindings
    let min_binding : t -> key * value = M.min_binding
    let max_binding : t -> key * value = M.max_binding
    let choose : t -> key * value = M.choose
    let split : key -> t -> t * value option * t = M.split
    let find : key -> t -> value = M.find
    let map : (value -> value) -> t -> t = M.map
    let mapi : (key -> value -> value) -> t -> t = M.mapi

    let show (m : t) : string =
      let binds, gt1 = fold (fun k v (b, g) ->
        b^", "^(K.show k)^" -> "^(V.show v), true) m ("", false) in
      if gt1 then "{ "^(String.sub binds 2 ((String.length binds) - 2))^" }"
      else "{}"

    let pp (fmt : Format.formatter) (x : t) : unit =
      Format.fprintf fmt "%s" (show x)

    let to_list = bindings

    let of_list : (K.t * V.t) list -> t =
      List.fold_left (fun a (k, v) -> add k v a) empty

    let hash seed s = fold (fun k v acc -> (K.hash (V.hash acc v) k)) s seed

  end

end

module PersistentDigraphExt = struct

  module MakeS(Vert : Comparable) = struct
    module type S = sig
      include Graph.Sig.P with type V.t = Vert.t
                          and type V.label = Vert.t
                          and type E.t = Vert.t * Vert.t
                          and type E.label = unit
      include Comparable with type t := t
      val to_lists : t -> Vert.t list * (int * int) list
      val of_lists : Vert.t list * (int * int) list -> t
      val dot   : t -> out_channel -> unit
    end
  end
    
  module Make(Vert : Comparable) : MakeS(Vert).S = struct
    include Graph.Persistent.Digraph.ConcreteBidirectional(struct include Vert let hash = hash 42 end)

    module NumV = struct
      type t = int * Vert.t
      (* the arguments to compare are reversed so I don't have to reverse the list of vertices
         in the lists function below... do not change or the d3 visualizer won't work! *)
      let compare (i, _) (i', _) = Pervasives.compare i' i
      let hash (_ : int) ((i, _) : t) : int = i
      let equal (i, _) (i', _) = i = i'
    end
    module NumG = Graph.Persistent.Digraph.ConcreteBidirectional(struct include NumV let hash = hash 42 end)
    (* Includes the numbered graph module and some graphviz specifics for dot file output. *)
    module NumGDot = struct
      include NumG
      let graph_attributes _ = []
      let edge_attributes _ = []
      let vertex_attributes (i, _) = [`Shape `Box; `Label (string_of_int i)]
      let default_edge_attributes _ = []
      let default_vertex_attributes _ = []
      let get_subgraph _ = None
      let vertex_name (i, _) = "\""^(string_of_int i)^"\""
    end
    (* Writes the graph to dot files *)
    module Output = Graph.Graphviz.Dot(NumGDot)

    let number (g : t) : NumG.t =
      let n = let count = ref 0 in (fun () -> let c = !count in count := c+1; c) in
      let tbl : ((Vert.t, int) Hashtbl.t) = Hashtbl.create 10 in
      let module S = Set.Make(struct
        type t = Vert.t
        let compare a a' = Pervasives.compare (Hashtbl.find tbl a) (Hashtbl.find tbl a')
      end) in
      iter_vertex (fun a -> Hashtbl.add tbl a (n ())) g;
      fold_edges
        (fun a a' ng -> NumG.add_edge ng (Hashtbl.find tbl a, a) (Hashtbl.find tbl a', a'))
        g
        NumG.empty

    let to_lists (g : t) : Vert.t list * (int * int) list =
      let ng = number g in
      (* The order of the verts on the left corresponds to the indices in the right list. *)
      let verts = NumG.fold_vertex (fun (_, a) acc -> a::acc) ng [] in
      let edges = NumG.fold_edges (fun (i, _) (i', _) acc -> (i, i')::acc) ng [] in
      (verts, edges)
(* 
     Example lists:

     ["a"; "b"; "c"],
     [0, 0; 0, 1; 1, 2]

     +->"a"-->"b"-->"c"
    /   /
   +---+
*)
    let of_lists ((vs, es) : Vert.t list * (int * int) list) : t =
      List.fold_left
        (fun g (i, j) -> add_edge g (List.nth vs i) (List.nth vs j))
        empty
        es

    let dot (g : t) (oc : out_channel) : unit =
      Output.output_graph oc (number g)

    module Conv = struct
      type t = Vert.t list * (int * int) list
      [@@deriving show]
    end

    let hash seed t = Hashtbl.seeded_hash seed t
    let pp fmt g = Conv.pp fmt (to_lists g)
    let show g = Conv.show (to_lists g)
    let compare g g' = compare (to_lists g) (to_lists g')
    let equal g g' = compare g g' = 0

  end

end
