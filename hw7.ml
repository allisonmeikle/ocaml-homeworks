(* TODO: Write a good set of tests for unused_vars. *)
let unused_vars_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (Let ("x", I 1, I 5), ["x"]);
  (* regular recursive function with no unused variables *)
  (Rec ("sum", Arrow([Int], Int), 
        Fn([("x", Int)], 
           If (Primop (Equals, [Var "x"; I 0]), I 0, 
               Primop(Plus, [Var "x";
                             Apply (Var "sum", 
                                    [Primop (Minus, [Var "x"; I 1])])])))), []); 
  (* recursive function which doesn't call itself *)
  (Rec ("no_call", Arrow([], Bool), 
        Fn([], B false)), ["no_call"]); 
  (* normal function w/ no unused *)
  (ex1, []); 
  (* function w/ some unused *) 
  (Fn([("x", Int);("y", Int)], I 0), ["x"; "y"]);
  (* Apply w/ some unused *)
  (Apply (Fn([("x", Int);("y", Int)], I 0), [I 1; I 2]), ["x"; "y"])
]

(* TODO: Implement the missing cases of unused_vars. *)
let rec unused_vars =
  function
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) -> unused_vars e @ unused_vars e1 @ unused_vars e2
  | Primop (_, args) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] args
  | Let (x, e1, e2) ->
      let unused = unused_vars e1 @ unused_vars e2 in
      if List.mem x (free_variables e2) then
        unused
      else
        x :: unused 
  | Rec (x, _, e) ->
      if List.mem x (free_variables e) then unused_vars e
      else x :: unused_vars e 
  | Fn (xs, e) -> 
      let xs = List.map fst xs in 
      let free_vars = free_variables e in
      List.fold_left (fun acc var -> 
          if List.mem var free_vars then acc
          else var::acc) (unused_vars e) xs

  | Apply (e, es) -> (unused_vars e) @ (List.fold_left (fun acc exp -> acc @ unused_vars exp) [] es)
      
(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
   Let ("y", I 2, Primop (Plus, [Var "y"; I 1])));
  (* Test case for Rec *)
  (((I 3, "z"), (* [3/z] *)
    Rec ("f", Arrow([Int], Int), 
         Fn ([("x", Int)], 
             Primop (Plus, [Var "x"; Var "z"])))),
   (* Expected result: Rec with z substituted by 3 *)
   Rec ("f", Arrow([Int], Int), 
        Fn ([("x", Int)], 
            Primop (Plus, [Var "x"; I 3])))); 
  (* Test case for Fn *)
  (((I 2, "y"), (* [2/y] *)
    Fn ([("x", Int); ("y", Bool)], 
        Primop (Plus, [Var "x"; Var "y"]))),
   (* Expected result: y in the body remains because it's shadowed *)
   Fn ([("x", Int); ("y", Bool)], 
       Primop (Plus, [Var "x"; Var "y"])));
  (* Substitution: [I 3, "x"] *)
  (((I 3, "x"), 
    Fn ([("y", Bool)], 
        Primop (Plus, [Var "x"; Var "y"]))),
(* Expected Result: x is replaced with 3, y is unaffected *)
   Fn ([("y", Bool)], 
       Primop (Plus, [I 3; Var "y"])));
  (* Substitution: [Primop (Plus, [Var "z"; I 1]), "x"] *)
  (((Primop (Plus, [Var "z"; I 1]), "x"), 
    Fn ([("z", Int); ("y", Bool)], 
        Primop (Times, [Var "x"; Var "z"]))),
(* Expected Result: z is renamed to avoid capture, x is substituted *)
   Fn ([("z1", Int); ("y", Bool)], 
       Primop (Times, [Primop (Plus, [Var "z"; I 1]); Var "z1"])));
  (* Test case for Apply *)
  (((I 4, "a"), (* [4/a] *)
    Apply (Var "f", [Var "a"; Primop (Plus, [Var "a"; I 1])])),
   (* Expected result: a substituted with 4 *)
   Apply (Var "f", [I 4; Primop (Plus, [I 4; I 1])])); 
  (* Substitution: [10/x] *)
  (((I 10, "x"), 
    Rec ("x", Int, Primop (Plus, [Var "x"; Var "y"]))), 
   (* Expected Result: No substitution for shadowed "x" *)
   Rec ("x", Int, Primop (Plus, [Var "x"; Var "y"])));
  (* Substitution: [Var "f", "x"] *)
  (((Var "f", "x"), 
    Rec ("f", Int, Primop (Plus, [Var "x"; Var "z"]))),
(* Expected Result: "x" is replaced by "f", "f" is renamed *)
   Rec ("f1", Int, Primop (Plus, [Var "f"; Var "z"])));
  
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      let (y, e) = 
        if y = x || (List.mem y (free_variables e')) then 
          rename y e 
        else (y, e)
      in 
      Rec (y, t, subst s e)

  | Fn (xs, e) -> 
      let names = List.map fst xs in
      let types = List.map snd xs in 
      (* if any input variable is captured, easier to rename all bound variables *)
      let needs_renaming = List.exists (fun name -> (x = name || (List.mem name (free_variables e')))) names in 
        
      let (names, e) = 
        if needs_renaming then 
          rename_all names e
        else (names, e)
      in 
      (* put back the names with their types *)
      let xs = List.combine names types in 
      Fn (xs, subst s e) 
  | Apply (e, es) -> 
      let mapped = List.map (subst s) es in
      Apply((subst s e), mapped)

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs

(* TODO: Write a good set of tests for eval. *)
let eval_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec and Apply!
  *)
  (Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);
  (Apply (
      Apply (
        Fn ([("x", Int)], Fn ([("y", Int)], Primop (Plus, [Var "x"; Var "y"]))),  (* fn(x) => fn(y) => x + y *)
        [I 3]),  (* Call the outer function with 3 *)
      [I 4]),  (* Call the returned function with 4 *)
 (* Expected result: 3 + 4 = 7 *)
   I 7); 
  (* A recursive function that doesn't call itself *)
  (Rec ("noop", Arrow ([Int], Int),
        Fn ([("x", Int)], I 0)),  
 (* Expected result: The inner function *)
   Fn ([("x", Int)], I 0));
  (* Function that sums two arguments *)
  (Apply (
      Fn ([("x", Int); ("y", Int)], Primop (Plus, [Var "x"; Var "y"])),  (* fn(x, y) => x + y *)
      [Primop (Plus, [I 1; I 2]); I 5]),  (* Call with (1 + 2) and 5 *)
 (* Expected result: (1 + 2) + 5 = 8 *)
   I 8);
  (* Apply a zero-argument function *)
  (Apply (
      Fn ([], I 42),  (* fn() => 42 *)
      []),  (* Call with no arguments *)
 (* Expected result: 42 *)
   I 42) 
]

(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2)

  | Rec (f, _, e) -> eval (subst(exp, f) e)

  | Apply (e, es) -> 
      let eval_e = eval e in 
      match eval_e with 
      | Fn (args, e') -> 
          if (List.length args) = (List.length es) then 
            let vals = List.map (eval) es in 
            let input_names = List.map fst args in
            let subs = List.combine vals input_names in
            eval (subst_list subs e') 
          else raise (Stuck Arity_mismatch)
      | _ -> raise (Stuck Apply_non_fn)

(* TODO: Write a good set of tests for infer. *)
let infer_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (([("x", Int)], Var "x"), Int);
  (* rec (f: int -> int) => fun (x: int) => if x = 0 then 0 else f (x - 1) *)
  (([("x", Int)], Rec ("f", Arrow ([Int], Int), 
                       Fn ([("x", Int)], 
                           If (Primop (Equals, [Var "x"; I 0]),
                               I 0,
                               Apply (Var "f", [Primop (Minus, [Var "x"; I 1])]))))),
   Arrow ([Int], Int));
  (* fun (x: int) => x, applied to 42 *)
  (([("x", Int)], Apply (Fn ([("x", Int)], Var "x"), [I 42])),
   Int);
  (* fun (x: int, y: int) => x + y, applied to (1, 2) *)
  (([("x", Int); ("y", Int)], Apply (Fn ([("x", Int); ("y", Int)], Primop (Plus, [Var "x"; Var "y"])), [I 1; I 2])),
   Int);
  (* fun (x: int) => x + 1 *)
  (([("x", Int)], Fn ([("x", Int)], Primop (Plus, [Var "x"; I 1]))),
   Arrow ([Int], Int));
  (* fun (x: int, y: int) => x + y *)
  (([("x", Int); ("y", Int)], Fn ([("x", Int); ("y", Int)], Primop (Plus, [Var "x"; Var "y"]))),
   Arrow ([Int; Int], Int));
  (* fun (x: int) => fun (y: int) => x + y *)
  (([("x", Int)], 
    Fn ([("x", Int)], Fn ([("y", Int)], Primop (Plus, [Var "x"; Var "y"])))),
   Arrow ([Int], Arrow ([Int], Int)));
  (* fun () => 42, applied to no arguments *)
  (([], 
    Apply (
      Fn ([], I 42),  (* Zero-argument function returning 42 *)
      [])),           (* No arguments provided *)
   Int);
  (* fun () => 42 *)
  (([], Fn ([], I 42)), Arrow ([], Int))
]

(* TODO: Implement the missing cases of infer. *)
let rec infer ctx e =
  match e with
  | Var x ->
      begin
        try lookup x ctx
        with Not_found -> raise (TypeError (Free_variable x))
      end
  | I _ -> Int
  | B _ -> Bool

  | Primop (po, exps) ->
      let (domain, range) = primopType po in
      check ctx exps domain range

  | If (e, e1, e2) ->
      begin
        match infer ctx e with
        | Bool ->
            let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in
            if t1 = t2 then t1
            else type_mismatch t1 t2
        | t -> type_mismatch Bool t
      end

  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer (extend ctx (x, t1)) e2

  | Rec (f, t, e) -> 
      let t' = infer (extend ctx (f, t)) e in
      (* check that t = t' *) 
      if t = t' then t
      else type_mismatch t t'

  | Fn (xs, e) -> 
      let t = infer (extend_list ctx xs) e in 
      let types = List.map snd xs in 
      Arrow (types, t)
  
  | Apply (e, es) -> 
      let t' = infer ctx e in 
      match t' with 
      | Arrow (t_is, t) -> check ctx es t_is t 
      | _ -> raise (TypeError (Apply_non_arrow t'))

and check ctx exps tps result =
  match exps, tps with
  | [], [] -> result
  | e :: es, t :: ts ->
      let t' = infer ctx e in
      if t = t' then check ctx es ts result
      else type_mismatch t t'
  | _ -> raise (TypeError Arity_mismatch)

(* TODO: Implement type unification. *)
let rec unify (t1 : utp) (t2 : utp) : unit =
  match t1, t2 with
  (* unifying identical concrete types does nothing *)
  | UInt, UInt
  | UBool, UBool -> ()
  (* For type constructors, recursively unify the parts *)
  | UArrow (t1, t1'), UArrow (t2, t2') ->
      (* need to unify t1 with t2 and t1' with t2' *)
      unify t1 t2;
      (* will return unit so can just do sequencing *)
      unify t1' t2'
  | UTVar a, _ -> unifyVar a t2
  | _, UTVar b -> unifyVar b t1
  (* All other cases are mismatched types. *)
  | _, _ -> unif_error @@ UnifMismatch (t1, t2)

(* Unify a variable with a type *)
and unifyVar (a: utp option ref) (t: utp) =
  match !a with 
  | Some t' -> unify t' t
  | None -> 
      match t with 
      | UTVar b -> 
          begin 
            match !b with 
            | Some b_type -> unifyVar a b_type
            | None -> 
                (*comparing memory addresses*)
                if a == b then () 
                else a := Some (UTVar b) 
          end
      | _ -> 
          if occurs a t then unif_error UnifOccursCheckFails
          else a := Some t
                
