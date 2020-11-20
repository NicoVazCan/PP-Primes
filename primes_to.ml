let print_int_list  = 
	List.iter (fun n -> print_int n; print_newline ()) 
in
try 
   if Array.length Sys.argv < 2 
      then print_endline "primes_to: missing argument"
      else let n = int_of_string Sys.argv.(1) in
	       print_int_list (Primes.primes_till n)
 with _ -> print_endline "prev_prime: invalid argument"