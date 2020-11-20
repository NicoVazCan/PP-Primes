(*Nicolas Vazquez Cancela*)

let is_prime n = 
	let r = int_of_float(sqrt (float_of_int n))
	in (*Calculamos la raiz de 'n' fuera para evitar hacer el
		cuadrado de 'd' en cada recursion.*)
	let rec not_div_from d = 
		d > r ||
		(n mod d <> 0 && not_div_from (d+2))
	in (*Sumo 2 a 'd' para saltarme los numeros pares ya 
		que obviamente ninguno de ellos es primo, salvo el 2.*)
	n > 1 &&
	(n = 2 || (n land 1 = 1 && not_div_from 3));;
		(*Por eso primero compruebo si 'n' es 2, en caso contrario
		comienzo a comprobar su divisiblidad con 3 si 'n' no es par,
		y como el bit de menor peso siempre es 0 para todos los 
		pares, hago un land de 'n' y 1 porque esta operacion es 
		mas rapida que un n mod 2 = 0 para la CPU.*)
	
let next_prime =
	let rec next x = 
		let x = x + 2 in
			if is_prime x then x else next x
	in (*Creo una funcion recursiva auxiliar para comprobar la
		la entrada, ya que si 'n' es menor a 2, el siguiente 
		primo siempre sera 2*)
	fun n -> if(n < 2) 
		then 2
		else if(n land 1 = 0)
			then next (n - 1)
			else next n;;
		(*Compruebo si 'n' es par para comenzar en el siguiente
		numero impar, ya que al hacer 'n - 1' y luego 'n + 2'
		el resultado final es 'n + 1'.*)

let prev_prime = 
	let rec prev x = 
		let x = x - 2 in
			if is_prime x then x else prev x
	in
	fun n -> if(n <= 2) 
		then raise (Not_found)
		(*Corrijo el problema de la anterior implementacion
		al no haber numeros primos menores que el 2.*)
		else if(n land 1 = 0 || n = 3)
			then prev (n + 1)
			else prev n;;
		(*Compruebo si 'n' es par para comenzar en el anterior
		numero impar.*)

let primes_between p g =
	let m = if(p <= 2) then 2 else p
	in
	(*Como 2 es el menor primo, comiezo con el si el
	minimo ('p') de el intervalo recivido es menor o igual que 
	este, evitandome asi recorrer todos los anteriores de 2.
	en caso contrario, el minimo sera un numero impar con el
	que podramos ya saltar los pares.*)
	let rec bucle l k = if(m > k)
		then if(m = 2) 
			then m::l (*Me aseguro de poner 2 en la cabeza si 
			este esta detro del intervalo recivido al terminar
			de buscar, ya que al saltar los pares nos lo 
			habremos olvidado.*)
			else l 
		else if(is_prime k)
			then bucle (k::l) (k - 2)
			else bucle l (k - 2)
	in if(m > g)
		then []
		else if(g land 1 = 0) (*Si el maximo ('g') del interbalo
			recivido es par entonces saltaremos al anterior impar*)
			then bucle [] (g - 1)
			else bucle [] g;;

let primes_till n = primes_between 2 n;;

(*FIN*)

(* Como en el enunciado de la practica pone que:
	"las funciones definidas en él sean lo más rápidas posible y que no puedan
	provocar un error por agotamiento de la pila de recursividad (“Stack_overflow”)."
	Rehice 'primes_between' de forma terminal, pero la original no es terminal,
	por lo que no se si os referis a todas las funciones deben ser terminales menos 
	esta o directamente todas.
	Además, conseguir un error de Stack_overflow con la version no terminal de
	la funcion 'primes_between' requeriría de una cantidad de tiempo absurda.
	Ante la duda os dejo esta version mejorada no terminal de 'primes_between' en 
	este comentario que os agradecería que sustituyerais por la terminal en el 
	caso de que no todas tengan que serlo, ya que es algo mas rapida.

let primes_between p n = 
	let rec next m =
		if m > n then [] else if(is_prime m) 
		then m::(next (m + 2))
		else next (m + 2)
	in if(p <= 2 && n >= 2)
		then 2::(next 3)
		else next p;;
	(*Como 2 es el menor primo, comiezo con el si el
	minimo ('m') de el intervalo recivido es menor o igual que 
	este, evitandome asi recorrer todos los anteriores de 2.
	en caso contrario, el minimo sera un numero impar con el
	que podramos ya saltar los pares.*)
*)