(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  (({nodes = ["a"; "b"; "c"; "d"]; 
     edges = [("a", "b", 4); ("a", "c", 2); ("c", "b", 4)]}, "a"), 
   ([("b", 4); ("c", 2)]));
  (({nodes = ["a"; "b"; "c"; "d"]; 
     edges = [("a", "b", 4); ("a", "c", 2); ("c", "b", 4)]}, "d"), 
   ([]));
  (({nodes = ["a"; "b"]; 
     edges = [("a", "b", 2); ("b", "a", 3)]}, "a"), 
   ([("b", 2)]))
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list =
  let edge (l : ('a * weight) list) ((v1, v2, w) : 'a * 'a * weight) : ('a * weight) list = 
    if v1 = vertex then (v2, w) :: l
    else l
  in
  List.fold_left edge [] g.edges
    
(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node ((n, w): 'a * weight) (visited : 'a list) : ('a list * weight) = 
    if n = b then ([b], w)
    else if List.mem n visited then raise Fail
    else 
      let (l, t) = aux_list (neighbours g n) (n :: visited) in 
      ((n::l), t+w)
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) = 
    match nodes with 
    | [] -> raise Fail
    | (n2, w2)::t ->
        try 
          aux_node (n2,w2) visited
        with Fail -> aux_list t visited
  in
  aux_node (a, 0) []

(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node ((n, w): 'a * weight) (visited : 'a list) fc sc : ('a list * weight)=
    if n = b then sc ([b], w)
    else if List.mem n visited then fc ()
    else aux_list (neighbours g n) (n::visited) fc (fun (l, t) -> sc ((n::l), t+w))
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) =
    match nodes with 
    | [] -> fc ()
    | (n2, w2)::t -> aux_node (n2, w2) visited (fun () -> aux_list t visited fc sc) sc
  in
  aux_node (a, 0) [] (fun () -> raise Fail) (fun r -> r)


(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * int) list =
  let rec aux_node ((n,w): 'a * int) (visited : 'a list) : ('a list * int) list =
    if n = b then [([b], w)] 
    else if List.mem n visited then []
    else 
      let paths = aux_list (neighbours g n) (n :: visited) in
      List.map (fun (path, total_cost) -> (n :: path, w + total_cost)) paths
  and aux_list (nodes: ('a * int) list) (visited: 'a list) : ('a list * int) list =
    match nodes with
    | [] -> []
    | (next, cost) :: rest ->
        let paths_from_next = aux_node (next, cost) visited in
        let paths_from_rest = aux_list rest visited in
        paths_from_next @ paths_from_rest
  in
  aux_node (a, 0) []

(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option =
  let rec max_paths (paths : ('a list * int) list) (max : 'a list * int): 'a list * int = 
    match paths, max with 
    | [], (p, max) -> (p, max)
    | (l, c)::t, ([], 0) -> max_paths t (l, c)
    | (l, c)::t, (p, max) -> 
        if c < max then max_paths t (l, c)
        else max_paths t (p, max)
  in 
  let paths = find_all_paths g a b in 
  match paths with 
  | [] -> None
  | _ -> Some (max_paths paths ([], 0))

(* ---------- Hamming Numbers ----------- *)

let rec merge s1 s2 =
  ()

let rec hamming_series =
  ()
