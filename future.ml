open Unix;;
open Sys;;

type 'a future = {t : Thread.t option ref; pid : int ref; value : 'a option ref};;

(* To generate a future, we use unix processes. Pipes are used to
   handle parent-child communication. The child process handles the
   computation and outputs the result to a shared file descriptor,
   to which the parent later reads. *)
let future f x : 'a future =
  (* establish future to be placed inside closure of sigchld *)
  let future = {t=ref None; pid=ref 0; value=ref None} in
  (* open pipe for communication between processes *)
  let (fd_in, fd_out) = pipe () in
  (* install signal handler for child process termination *)
  let write_result f =
    let _ = waitpid [] !(f.pid) in
    let result = input_value (in_channel_of_descr fd_in) in
    f.value := Some result in
  match fork () with
      (* for child process, write result to fd_out *)
      0 -> (
	close fd_in;
	let result = f x in
	let output = out_channel_of_descr fd_out in
	output_value output result;
        exit 0)
    (* for parent process, set process id *)
    | id -> (
      close fd_out;
      future.pid := id;
      future.t := Some (Thread.create write_result future);
      future)
;;

(* Force waits for the child thread and then reports the future's
   value. *)
let force (f:'a future) : 'a =
  match !(f.t) with
      Some t -> (Thread.join t;
		 match !(f.value) with
		   | Some v -> v
		   | None -> failwith "waited for thread, but no result")
    | None -> failwith "no thread available for future"
;;

(* fib n calculates the nth value in the Fibonacci sequence. Used for
   testing speed. *)
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
