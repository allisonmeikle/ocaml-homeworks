(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x), -1), []);
  (((fact), 4), [1;1;2;6;24]);
  (((fact), 0), [1]);
]

(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  tabulate (fun n -> dist_black n x (marblesTotal, marblesDrawn)) marblesTotal

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  (([[];[];[]]), true);
  (([[]]), true);
  (([[1.0];[2.0];[3.0]]), false);
  (([[1.0];[];[]]), false);
]

(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool = 
  let is_empty_row (row: 'a list) : bool = (
    match row with 
    | [] -> true
    | _ -> false) in
  List.for_all is_empty_row matrix

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  List.map (fun x -> dist_table (total, drawn) x) resultList

(* TODO: Implement combined_dist_table: float list list -> float list *)
let rec combined_dist_table (matrix: float list list) : float list = 
  let rec transpose matrix =
    if is_empty matrix then []
    else
      let first_column = List.map List.hd matrix in
      let rest_matrix = List.map List.tl matrix in
      first_column :: transpose rest_matrix
  in 
  let transposed_matrix = transpose matrix in 
  List.map (fun row -> List.fold_left ( *. ) 1.0 row) transposed_matrix
    
               
(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))


(* TODO: Implement all: (ingredients list -> bool) -> cake -> bool *)
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = 
  match c with 
  | Slice ingredients_list -> p ingredients_list
  | Cake (c1, c2) -> all p c1 && all p c2

(* TODO: Write some test cases for is_chocolate_cake. *)
let is_chocolate_cake_tests = [
  ((Cake(Slice[], Slice[])), false);
  ((Cake (Slice [Chocolate ; Flour], Cake (Slice [Chocolate ; Almonds] , Slice [Chocolate ; BlackBeans]))), true)
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let is_chocolate_cake (c: cake) : bool = 
  all (fun ings -> List.exists (fun i -> i = Chocolate) ings) c
  

(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = 
  match c with
  | Slice s -> Slice(p s)
  | Cake (c1, c2) -> Cake(map p c1, map p c2)

(* TODO: Write some test cases for add_ingredient. *)
let add_ingredient_tests = [
  ((Orange, Cake(Slice[Chocolate], Slice[Flour])), Cake(Slice[Chocolate; Orange], Slice[Flour; Orange]));
]

(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  let check (l : ingredients list) : ingredients list =
    if (List.exists (fun i -> i = x) l) then l 
    else l@[x]
  in map check c

(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  match c with 
  | Slice s -> f s base
  | Cake (c1, c2) -> fold_cake f (fold_cake f base c1) c2


(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let get_all_ingredients (c: cake) : ingredients list = 
  let rec add (x : ingredients list) (base : ingredients list) =
    match x with 
    | [] -> base 
    | x :: xs -> 
        if List.exists (fun i -> i = x) base then add xs base
        else add xs (base@[x])
  in
  fold_cake (add) [] c