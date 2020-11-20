val is_prime : int -> bool
(* is_prime n debe vale true para todo numero n primo mayor que 1 y 
   false en el resto de los casos *)
val next_prime : int -> int
(* next_prime n debe ser el menor int i, que satisfaga i > n && is prime i *)
val prev_prime : int -> int
(* prev_prime n debe ser el mayor int i, que satisfaga i < n && is prime i *)
val primes_between : int -> int -> int list 
(* primes_between m n debe ser las lista, en orden creciente, de todos los primos
mayores o iguales que m y menores o iguales que n *)
val primes_till : int -> int list
(* primes_till n debe ser la lista, en orden creciente, de todos los primos 
menores o iguales que n *)
