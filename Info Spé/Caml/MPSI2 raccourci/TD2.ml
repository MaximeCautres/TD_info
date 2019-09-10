let rec quoi f = function
| []->[]
| t::q when f t ->t::(quoi f q)
| t::q->(quoi f q);;

let f x = x > 0;;
 
let rec sudan n x y = match n,x,y with
|n,_,_ when n = 0 -> x+y
|_,_,y when y = 0 -> x
|_-> sudan (n-1) (sudan n x (y-1)) (sudan n x (y-1) + y) ;;

let bin k n = facto n / (facto k * facto (n-k)) ;;
let rec facto n = match n with
|0 -> 1
|n -> n*facto (n-1);; 

let rec bij (x,y) = match (x,y) with
|(0,0) -> 0
|(_,0) -> 1 + bij(0, x-1)
|_ -> 1 + bij(x+1, y-1);;

bij (14,18);;
#trace bijmoch;;
let rec bijmoch (x,y) = match (x,y) with
|(0,0) -> 0
|(0,_) -> 1 + bijmoch(y-1, 0)
|(x,y) when y<x -> 1 + bijmoch(x, y+1)
|_-> 1 + bijmoch(x-1, y);;

bijmoch (14,18);; 

lexico graphique sur n2 avec x et y pour le premier 
lexicographique sur Nx(-N);; 

let bijtermoch (x, y) = (x + y)*(x + y+1)/2 + y;;
bijtermoch (14,18);;	

let rec impsum n = match n with 
|0-> 0
|_-> 2*n-1 + impsum (n-1);;

let bijexcrmoch (x,y) = if x > y then (x*x+2) - y else y*y + x;;
bijexcrmoch (14,18);;

let rec sum n f g= match n with
|0-> bin ;;


let rec a n = match n with
|0->0
|1-> 1
|_ -> let s = ref 0 in for k = 0 to n do s := !s + bin k n * a k * a n-k done; !s;;

let rec pgcd a b = match b with 
|0 -> a
|_ -> pgcd b (a mod b);;
pgcd 48 348;;

let pgcdmoch d e = 
	let a = ref d and b = ref e and c = ref 0 in
	while !b != 0 do 
		c := !a mod !b;
		a := !b;
		b := !c;
	done; !a;;
pgcdmoch 48 348;;

let rec pow a n = match n with
|0->1
|_->a*pow a (n-1);;

let rec convertbase n b = match n with
   | 0 -> []
   | _ -> convertbase (n / b) b @ [n mod b];;
convertbase 48 2;;

let convertmoche n b = let x = [] and i = ref 0 in
	let m = ref n in
	while !m > pow b !i+1 do 
		i := !i+1;
	done;
	for k = 0 to !i do
		let d = ref 0 in
		while !m - pow b !i-k > 0 do 
			m := !m - pow b !i-k;
			d := !d+1;
		done;
		x = x@[!d];
	done;x;;
convertmoche 39 3;;

let rec hanoi (n,d,a)  = match (n,d,a) with
|(0,_,_)->[0]
|(1,_,_)->[10*d+a]
|(_,_,_)-> hanoi (n-1,d,6-d-a) @ [10*d+a] @ hanoi (n-1,6-d-a,a);;

hanoi (4,1,3);;

let rec hanoik (n,d,a) = match (n,d,a) with
|(0,_,_)->[0]
|(1,_,_)->[10*d+(6-a-d);(6-a-d)*10+a]
|(_,_,_)-> hanoik (n-1,d,a) @ [10*d+(6-a-d)] @ hanoik (n-1,a,d)@ [(6-a-d)*10+a] @ hanoik (n-1,d,a);;
hanoik (3,1,3);;

let rec pow a n = match n with
|0->1
|_->a*pow a (n-1);;

let rec power (a,b) = match (a,b) with
|(_,0)->1
|(_,_)->pow a (power (a,b-1)) ;;

let rec knuth (a, b, n) = match (a, b, n) with
   | (_, _, 2) -> power (a, b)
   | (_, 0, _) -> a
   | (_, _, _) -> knuth (a, knuth (a, b - 1, n), n - 1);;
knuth (2,2,3);; 

let rec prime m n = match (m,n) with
|(_,1)->1
|(_,m)->prime (m,n-1)
|(_,_)->(m mod n)*prime (m,n-1);;

let rec eratahos n = le

prime 9 9;;

let prime n = let x = ref 0 in
	for i = 2 to n-1 do
	if n mod i = 0 then x := !x + 1 ;
	done;
	if !x = 0 then 
	begin
	print_string "premier"
	end 
	else
	begin
	print_string "nn premier"
	end;;

let rec primerec (p,n) = match (p,n) with
|(p,n) when p = n-> true
|(1,2)-> false
|(_,_)-> if p mod n = 0 then false else primerec (p,n+1) ;;

primerec (10,2);;

let rec goldbach (a,n) = match (a,n) with
|(0,4)->true
|(a,n) when n < a -> false 
|(_,_)-> if primerec(a,2) && primerec(n,2) then goldbach(0, a+n-2) else goldbach(a+1,n-1);;

goldbach (0,68);;

