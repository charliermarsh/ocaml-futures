(* fib n calculates the nth value in the Fibonacci sequence. Used for
   testing speed. *)
open Future;;
open Sys;;

let rec fib n =
  match n with
      0 -> 0
    | 1 -> 1
    | _ -> fib (n-1) + fib (n-2)
;;

(* Beware: time () just calculates the time in the parent process,
   rather than the time in all processes combined (I think). So
   it's not really a good metric. For a better understanding, use
   "time ./future" from the command-line. *)
let test_multi () = 
  let n1 = 35 in
  let n2 = 37 in
  let f1 = future fib n1 in
  let f2 = future fib n2 in
  let r1 = force f1 in
  let r2 = force f2 in
  let t = time () in
  Printf.printf "Multi-threaded results: %d, %d\n" r1 r2;
  Printf.printf "Multi-threaded computation required %f seconds.\n" t;
;;

let test_single () = 
  let n1 = 35 in
  let n2 = 37 in
  let r1 = fib n1 in
  let r2 = fib n2 in
  let t = time () in
  Printf.printf "Single-threaded results: %d, %d\n" r1 r2;
  Printf.printf "Single-threaded computation required %f seconds.\n" t;
;;

let main =
  try
    let b = bool_of_string Sys.argv.(1) in
    if b then
      test_multi ()
    else
      test_single()
  with Invalid_argument(s) ->
    print_string "Please enter 'true' for multi-threading or 'false' else.\n"
;;
    
 
