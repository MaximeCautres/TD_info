Question 1;;

let rec puissance x n = match n with
|0->1
|_->x*(puissance x (n-1));;

let rec puissanced x n = match n with
|1->x
|n when n mod 2=0->(puissanced x (n/2))*(puissanced x (n/2))
|_->x*(puissanced x (n-1));;

plus petit pour le second programme;;

Question 2;;

let rec fib1 n = match n with
|0->1
|1->1
|n->fib1 (n-1) + fib1 (n-2);;

on utilise fib1 n appels;;

let rec sum l = match l with
|[]->0
|t::q->t+sum q;;

let rec fib2 n = match n with
|0->[0;1]
|1->[1;1]
|n->[sum (fib2 (n-2)); sum (fib2 (n-1))];;

let rec fib3 n = match n with
|0->1
|1->1
|n->(fib3 (n/2)) * (fib3 (n-n/2))+(fib3 (n/2-1)) * (fib3 (n-n/2-1));;

Question 3;;

let rec pgcd a b = match b with 
|0 -> a
|_ -> pgcd b (a mod b);;

let ppcm p q = (p*q)/(pgcd p q);;

let ppcmmoche p q = let a = ref p and b = ref q in
	while !a != !b  do
 		if !a < !b then a := !a+p else b := !b+q;
 	done;
 	!a;;

let rec ppcmb l = match l with
|t::q::[]->ppcm t q;;


exercice 4.1 ;;

let rec fpp l x = match l with
|t::q::[]->q*t
|t::q::r->q*t+fpp ((t*x)::r) x;;

let fp l x = match l with
|[]->0
|_-> fpp (1::l) x;;

4.4;;

let mult p q = 
	let r = Array.make (Array.length p + Array.length q) 0 in 
	for k = 0 to Array.length r -1 do
		for i = 0 to k do
			r.[k] = r.[k]+p[i]*q[k-i]
		done;
	done;
	r;;


