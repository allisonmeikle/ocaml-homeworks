(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*) 
let new_account (p: passwd) : bank_account =
  let pw = ref p  in 
  let bal = ref 0 in 
  let cnt = ref 0 in
  {
    update_passwd = 
      (fun pw_old pw_new -> 
         if pw_old = !pw then (cnt := 0 ; pw := pw_new)
         else (cnt := !cnt + 1 ; raise wrong_pass)
      );
    deposit = 
      (fun p amt -> 
         if !cnt >= 3 then raise too_many_attempts 
         else if p = !pw then (cnt := 0; bal := !bal + amt)
         else (cnt := !cnt + 1 ; raise wrong_pass)
      );
    retrieve = 
      (fun p amt -> 
         if !cnt >= 3 then raise too_many_attempts 
         else if p = !pw then (cnt := 0;
                               if !bal < amt then raise no_money
                               else bal := !bal - amt)
         else (cnt := !cnt +1 ; raise wrong_pass)
      ); 
    print_balance = 
      (fun p -> 
         if !cnt >= 3 then raise too_many_attempts
         else if p = !pw then (cnt := 0; !bal)
         else (cnt := !cnt + 1 ; raise wrong_pass)
      )
  }
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *) 
let rec fib_I (n: int) : fib_result =
  let num_calls = ref 0 in 
  
  let rec fib' (n: int) : int = (
    num_calls := !num_calls + 1 ;
    if n = 0 then 0
    else if n = 1 then 1
    else fib' (n-2) + fib' (n-1)
  )
  
  in 
  {
    num_rec = !num_calls ; 
    result = fib' n
  }
;;

(* Q 2.2 : Memoization with a global store *) 
let fib_memo (n: int) : int =
  let rec fib n =
    match Hashtbl.find_opt store n with 
    | None -> 
        if n = 0 then (Hashtbl.add store n 0 ; 0)
        else if n = 1 then (Hashtbl.add store n 1 ; 1)
        else let result = fib (n-2) + fib (n-1) in Hashtbl.add store n result ; result
    | Some v -> v
  in
  fib n
;;

(* Q 2.3 : General memoization function *) 
let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  let store : ('a, 'b) Hashtbl.t = Hashtbl.create 1000 in 
  let rec g (a : 'a) : 'b =
    match Hashtbl.find_opt store a with
    | None -> 
        let result = f g a in
        Hashtbl.add store a result;
        stats.entries := !(stats.entries) + 1;
        result
    | Some v -> 
        stats.lkp := !(stats.lkp) + 1;
        v
  in
  g
;;


(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)
let fibM_stats = { entries = ref 0 ; lkp = ref 0} ;; 
let fibM_ref = ref None ;;

let fibM (n: int) : (int * stats) = 
  match !fibM_ref with
  | None ->
      let memoized_fibM = memo 
          (fun g n ->
             if n = 0 then 0
             else if n = 1 then 1
             else g (n - 2) + g (n - 1)
          ) fibM_stats
      in
      fibM_ref := Some (memoized_fibM);
      (memoized_fibM n, fibM_stats)
  | Some (memoized_fibM) ->
      (memoized_fibM n, fibM_stats)
;;
