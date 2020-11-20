try 
   if Array.length Sys.argv < 2 
      then print_endline "prev_prime: missing argument"
      else let n = int_of_string Sys.argv.(1) in
	       if n > 2 then 
                   (print_int (Primes.prev_prime n);
		            print_newline ())
	       else print_endline "prev_prime: no previous prime"
with _ -> print_endline "prev_prime: invalid argument"