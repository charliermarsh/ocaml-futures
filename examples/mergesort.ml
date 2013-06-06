open Future;;

(* sort a list of integers l with parallelism *)
let rec mergesort (l:int list) : int list =
  (* merge two sorted lists recursively *)
  let rec merge (l1,l2) =
    match (l1, l2) with
	([], []) -> []
      | (l, [])
      | ([], l) -> l
      | (hd1::tl1, hd2::tl2) ->
	if (hd1 < hd2) then
	  hd1::merge (tl1,l2)
	else
	  hd2::merge (l1,tl2) in
  (* split a list l in half *)
  let split l : int list * int list =
    match l with
	[] -> ([], [])
      | hd::[] -> (l, [])
      | _ -> let (l1, l2, _) = List.fold_left
	       (fun (l1, l2, n) nxt ->
		 if (n > 0) then (nxt::l1, l2, n-1)
		 else (l1, nxt::l2, n-1)) ([],[],(List.length l)/2) l in
	     (List.rev l1, List.rev l2) in
  (* perform mergesort with multiple threads *)
  match l with
      [] -> []
    | hd::[] -> [hd]
    | _ -> let (l1, l2) = split l in
	   let f = Future.future mergesort l1 in
	   let l2' = mergesort l2 in
	   let l1' = Future.force f in
	   merge (l1', l2')
;;

(* generate a random list of n integers between 0 and 100 *)
let rec random_list n =
  let max = 100 in
  if (n <= 0) then []
  else (Random.int max)::random_list (n-1)
;;

(* prompts the user to sort a single list with parallelism *)
let main =
  try
    let n = int_of_string Sys.argv.(1) in
    let _ = mergesort (random_list n) in
    ()
  with Invalid_argument(s) ->
    print_string "Please enter an integer to specify the length of list to sort. \n"
;;
  
