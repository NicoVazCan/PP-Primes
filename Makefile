primes:
	ocamlc -c primes.mli primes.ml
next_prime: primes
	ocamlc -o next_prime primes.cmo next_prime.ml
prev_prime: primes
	ocamlc -o prev_prime primes.cmo prev_prime.ml
primes_to: primes
	ocamlc -o primes_to primes.cmo primes_to.ml
all: next_prime prev_prime primes_to
clean_all:
	rm *.cmi *.cmo
	rm next_prime prev_prime primes_to