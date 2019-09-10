let rec power a n = match n with
	|0 -> 1
	|n -> a * power a (n-1);;

power 2 4;;

let rec add l1 l2 = match l1 with
	|[]->l2
	|n::l1' -> n::(add l1' l2);;

let rec length l = match l with
	|[]->0
	|n::l'-> 1 + (length l');;

let rec member x l = match l with
	|[]-> false
	|n::l' when n = x -> true
	|_::l' -> member x l';;

let rec minimum l = match l with
	|[] -> failwith "failure: empty list"
	|[n] -> n
	|n::m::l' -> match n < m with
					|true -> minimum (n::l)
					|false -> minimum (m::l);;

let rec inserer x l = match l with
	|[] -> [x]
	|n::l' -> if n<x then n::(inserer x l') else x :: l;;

let rec tri_insertion l = match l with
	|[]->[]
	|n::l'-> inserer n (tri_insertion l');;

						
let separe l = let rec aux l1 l2 = if (length l1) > (length l2) then match l1 with
						|n::l1' -> aux l1' (n::l2)
						|[]-> failwith "c'est du beau code"
						else l1, l2 in aux l [] ;;

let rec fusion l1 l2 = match l1, l2 with
	|_, [] -> l1
	|[], _ -> l2
	|n::l1',m::l2' -> if n < m then n::(fusion l1' l2) else m::(fusion l1 l2');; 
	
let rec tri_fusion l = match l with
	|[]->[]
	|[n]->[n]
	|l -> let l1, l2 = separe l in fusion (tri_fusion l1) (tri_fusion l2);;

let union l1 l2 = let rec aux l l' = match l with
	|[] -> l'
	|n::tl -> if member n l' then aux tl l' else aux tl (n::l') in aux (add l1 l2) [];;


let intersection l1 l2 = let rec aux l1 l2 l3 = match l1 with
	|[] -> l3
	|n::l1' -> if member n l2 && not (member n l3) 
	then aux l1' l2 (n::l3) else aux l1' l2 l3 in aux l1 l2 [];;

let mirroir l = let rec aux l1 l2 = match l1 with
	|[]->l2
	|n::l1' -> aux l1' (n::l2) in aux l [];;
	
let parties l = let rec aux l1 l2 l3 = match l1, l2 with
	|[], _ -> l3
	|_::l1', [] -> aux l1' (l3) (l3)
	|n::_, list::l2' -> aux l1 l2' ((n::list)::l3) in aux l [[]] [[]];;

parties [2;3];;
 (* TD1 *)
 (* Q1 *)
 
 let donne_A n = let rec aux n x1 x2 = match n with
 	|0 -> []
 	|_ -> if 2*x1 < 3*x2 
 			then (2*x1+1)::aux (n-1) (x1+1) x2
 			else if 2*x1 = 3*x2 
 			then (2*x1+1)::aux (n-1) (x1+1) (x2+1)
 			else(3*x2+1)::aux (n-1) x1 (x2+1)
 			in print (aux n 0 0) ;;

let donne_B n = let rec aux n l1 l2 = match n, l1, l2 with
 	|1, _, _ -> []
 	|_, x1::l1', x2::l2' -> if 2*x1 < 3*x2 
 					then (2*x1+1):: (aux (n-1) (add l1' [(2*x1+1)]) (add l2 [(2*x1+1)]))
 					else (if 2*x1 = 3*x2 
 					then (2*x1+1):: (aux (n-1) (add l1' [(2*x1+1)]) (add l2' [(2*x2+1)]))  				
 					else (3*x2+1):: (aux (n-1) (add l1 [(3*x2+1)])(add l2' [(3*x2+1)])))
	|_,_,_ -> failwith "marche pas"
	in 1 :: aux n [1] [1] ;;

donne_B 12;;

(* Q2 *)

let rec reverse n = match n with
	|0 -> 0
	|_-> power 10 (int_of_float (log10 (float_of_int n))) * (n mod 10) + reverse (n/10);;

reverse 245;;

let est_palindrome n = if reverse n = n then true else false;;

def matrice_spirale n = 
	let rec aux n d f = match n with
	|1 -> let Array.make_matrix 1 1 f
	|2 -> let T = Array.make_matrix 2 2 0 in 
				let T.(0).(0)=f-3 and T.(0).(1)=f-2 
				and T.(1).(0)=f-1 and T.(1).(1)=f in T
	|k -> let T = Array.make_matrix k k 0 in 
			for i in range(k;;
			
(* Erathosthene *)
let eratosthene n = 
	let T = array.make n+1 true and
	T.(0) <- false and
	T.(1) <- false in
	for i = 2 to (Array.length T + 1) do
		if T.(i) then for j = 2 to n/T.(i) do T.(j*T.(i)) <- false
		done;
	T;;	
	
let search n p T min = 
	let l = [] in
	for i = 0 to n-p-1 do
		let s = ref 0 in
		for j = 0 to p+1 do
			s := !s + T.(i+j)
		if !s > min then i::l else []@l
	l;;
	
let sous l dl = let rec aux l dl s1 s2 s = match l, dl, s1 with
	|_,_, [] -> failwith "pas de sous suite"
	|_,[], _-> s
	|[],_,_-> failwith "pas de sous suite égale"
	|n::l',m::dl',o::s1' -> if n = m then aux l' dl' s1 s2 s else aux s1' s2 s1' s2 s+1
	in aux l dl l dl 0;;

let rec aux l = match l with
		|[]->[]
		|c::l'-> if match c with
			|x::y::[] -> if (x > -1 & x < 8 & y > -1 & y < 8) then [x;y]::(aux l') else aux l';;

let deplacmement x y = 
	let l = [[x+2;y+1];[x+2;y-1];[x-2;y+1];[x-2;y-1];[x+1;y+2];[x-1;y+2];[x+1;y-2];[x-1;y-2]] in aux l ;;

