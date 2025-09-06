(*------------------ Q1------------------ *)
let rec parseExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseSExp
    toklist
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp
       | _ -> raise (Error "Expected a single semicolon"))

and parseSExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parsePExp toklist (fun toklist' exp -> 
      match toklist' with 
      | PLUS :: toklist'' -> parseSExp toklist'' (fun rest exp' -> sc rest (Sum (exp, exp')))
      | SUB :: toklist'' -> parseSExp toklist'' (fun rest exp' -> sc rest (Minus (exp, exp')))
      | _ -> sc toklist' exp)

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseAtom toklist (fun toklist' exp -> 
      match toklist' with 
      | TIMES :: toklist'' -> parsePExp toklist'' (fun toklist''' exp' -> sc toklist''' (Prod (exp, exp')))
      | DIV :: toklist'' -> parsePExp toklist'' (fun toklist''' exp' -> sc toklist''' (Div (exp, exp')))
      | _ -> sc toklist' exp)

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  match toklist with 
  | INT n :: toklist' -> sc toklist' (Int n)
  | LPAREN :: toklist' -> parseSExp toklist' (fun toklist'' exp -> 
      match toklist'' with 
      | RPAREN :: toklist'''' -> sc toklist'''' exp
      | _ -> raise (Error "Expected a right parenthesis")
    )
  | _ -> raise (Error "Not an atomic expression")

(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e)

(* ---------- Hamming Numbers ----------- *) 

let rec merge (s1: 'a str) (s2 : 'a str) : 'a str = 
  (* if they're equal, remove both heads to avoid duplicates *)
  if s1.hd = s2.hd then {hd = s1.hd; tl = delay (fun () -> merge (force s1.tl) (force s2.tl))}
  else if s1.hd < s2.hd then {hd = s1.hd; tl = delay (fun () -> merge (force s1.tl) s2)}
  else {hd = s2.hd; tl = delay (fun () -> merge s1 (force s2.tl))}

let rec hamming_series : int str = 
  (* Note I tried to use delay inside of the tail definition but kept getting this error: 
     This kind of expression is not allowed as right-hand side of `let rec'
     So I had to use Susp directly to make my code run without errors
  *)
  {hd = 1;
   tl = Susp (fun () -> merge (times 2 hamming_series) (merge (times 3 hamming_series) (times 5 hamming_series)))} 
