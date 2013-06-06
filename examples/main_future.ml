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
let test_multi n m =
  let f1 = future fib n in
  let f2 = future fib m in
  let r1 = force f1 in
  let r2 = force f2 in
  Printf.printf "Multi-threaded results: %d, %d\n" r1 r2;
;;

let test_single n m =
  let r1 = fib n in
  let r2 = fib m in
  Printf.printf "Single-threaded results: %d, %d\n" r1 r2;
;;

let main =
  try
    let b = bool_of_string Sys.argv.(1) in
    let n = int_of_string Sys.argv.(2) in
    if b then
      test_multi n n
    else
      test_single n n
  with Invalid_argument(s) ->
    print_string "Please enter 'true' for multi-threading or 'false' else, followed by a value of n to compute.\n"
;;
