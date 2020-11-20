let m = 4611686018427387847 (* prev_prime max_int *)
in
try 
   if Array.length Sys.argv < 2 
      then print_endline "next_prime: missing argument"
      else let n = int_of_string Sys.argv.(1) in
	       if n >= m then print_endline "next_prime: argument is too big"
		   else
           (print_int (Primes.next_prime n);
		    print_newline ())
with _ -> print_endline "next_prime: invalid argument"