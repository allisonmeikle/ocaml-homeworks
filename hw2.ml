(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ([A;A;A;A;G;G;A;T;T;T;C;T;C], [(4,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)]);
  ([], []);
  ([A], [(1,A)]);
  ([A;A;A;A], [(4,A)])
]

let rec comp (l : nucleobase list) (n : nucleobase) (num : int) (acc : (int * nucleobase) list) : (int * nucleobase) list = 
  match l with 
  | [] -> (acc @ [(num, n)])
  | x :: xs -> 
      if x = n then comp xs x (num+1) acc
      else comp xs x 1 (acc @ [(num, n)])

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list =
  match l with 
  | [] -> []
  | x :: xs -> comp l x 0 []

(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
  ([], []);
  ([(1,A)], [A]); 
  ([(4,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)], [A;A;A;A;G;G;A;T;T;T;C;T;C])
]

let rec decomp (l : (int * nucleobase) list) (num : int) (acc : nucleobase list) : nucleobase list = 
  match l with 
  | [] -> acc
  | (count, base) :: xs -> 
      if num = count then decomp xs 0 acc
      else decomp l (num+1) (acc @ [base])

(* TODO: Implement decompress. *)
let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  match l with 
  | [] -> []
  | (count, base) :: xs -> decomp l 0 []


(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [ 
  (FLOAT 2.1, 2.1);
  ((MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0)), 27.5);
  (EXP (FLOAT 0.0), 1.0);
  (SIN (COS (FLOAT 1.0)), 0.5143952585);
  (MINUS (FLOAT 2.2, FLOAT 0.1), 2.1)
]

(* TODO: Implement eval. *)
let rec eval (e: exp) : float = 
  match e with 
  | FLOAT f -> f
  | PLUS (e1, e2) -> (eval e1) +. (eval e2)
  | MINUS (e1, e2) -> (eval e1) -. (eval e2)
  | MULT (e1, e2) -> (eval e1) *. (eval e2)
  | DIV (e1, e2) -> (eval e1) /. (eval e2)
  | SIN e1 -> sin (eval e1)
  | COS e1 -> cos (eval e1)
  | EXP e1 -> exp (eval e1)

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  ((MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0)), [Float 2.2; Float 3.3; Plus; Float 5.0; Mult]);
  (FLOAT 2.0, [Float 2.0]);
  ((EXP (SIN (COS (FLOAT 1.0)))), [Float 1.0; Cos; Sin; Exp])
]

(* TODO: Implement to_instr. *)
let rec to_instr (e : exp) : instruction list =  
  match e with 
  | FLOAT f -> [Float f]
  | PLUS (e1, e2) -> to_instr e1 @ to_instr e2 @ [Plus]
  | MINUS (e1, e2) -> to_instr e1 @ to_instr e2 @ [Minus]
  | MULT (e1, e2) -> to_instr e1 @ to_instr e2 @ [Mult]
  | DIV (e1, e2) -> to_instr e1 @ to_instr e2 @ [Div]
  | SIN e1 -> to_instr e1 @ [Sin]
  | COS e1 -> to_instr e1 @ [Cos]
  | EXP e1 -> to_instr e1 @ [Exp]


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  ((Mult, [5.0; 5.5]), Some([27.5]));
  ((Exp, [0.0]), Some([1.0]));
  ((Plus, [1.0]), None);
  ((Float 4.2, [5.1; 2.0]), Some([4.2; 5.1; 2.0]));
  ((Sin, []), None)
]

(* TODO: Implement instr. *)               
let instr (i : instruction) (s : stack) : stack option = 
  match i with 
  | Float f -> Some(f :: s)
  | Plus -> (
      match s with 
      | [] | [_] -> None
      | x1 :: x2 :: rest -> Some(x2 +. x1 :: rest)
    )
  | Minus -> (
      match s with 
      | x1 :: x2 :: rest -> Some (x2 -. x1 :: rest)
      | _ -> None 
    )
  | Mult -> (
      match s with 
      | x1 :: x2 :: rest -> Some (x2 *. x1 :: rest)
      | _ -> None 
    )
  | Div -> (
      match s with 
      | x1 :: x2 :: rest -> Some (x2 /. x1 :: rest)
      | _ -> None 
    )
  | Sin -> (
      match s with 
      | x1 :: rest -> Some (sin(x1) :: rest)
      | _ -> None 
    )
  | Cos -> (
      match s with 
      | x1 :: rest -> Some (cos(x1) :: rest)
      | _ -> None 
    )
  | Exp -> (
      match s with 
      | x1 :: rest -> Some (exp(x1) :: rest)
      | _ -> None 
    )
    


(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 2.2; Float 3.3; Plus; Float 5.; Mult], Some (27.5));
  ([Float 0.0; Exp], Some (1.0));
  ([Float 2.2; Plus], None);
  ([], None)
]

let rec prog_help (instrs: instruction list) (s: stack option): float option = 
  match instrs with
  | [] -> (
      match s with
      | Some (result::_) -> Some result
      | _ -> None
    )
  | i :: rest -> 
      match s with
      | Some s1 -> (
          match instr i s1 with
          | Some s2 -> prog_help rest (Some s2)
          | None -> None
        )
      | None -> None
  

(* TODO: Implement prog. *)
let prog (instrs: instruction list) : float option = 
  prog_help instrs (Some [])
    
  