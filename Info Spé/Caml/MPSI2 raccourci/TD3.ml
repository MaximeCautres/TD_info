question 1

version recursive;;

let rec long l = match l with
|[]->0
|t::q->1+long q;;

long [1;2;3;4;5];;

terminaison, on reduit la taille a chaque fois variable
 de n qui est bien ordonné
correction, on ne diminue de 1 la liste a chaque fois et
 l'on incremente n compteur de 1 donc la somme deux deux
reste constante donc a la condition d'arret quand l = [] on a le compteur qui vaut long l
la complexité est en o(n);;
 
 version récursive terminale;;
 
let rec longterm l = match l with
|t::[] -> t
|t::r::q -> longterm ((t+1)::q) ;;

let rec longterminal l = match l with
|t::q when t != 1 -> longterm (1::q)
|_->longterm l;;


longterminal [3;7;4;7;5];;

question deux 

version recursive;;

let rec sum l = match l with
|[]->0
|t::q->t + sum q;;
sum [3;7;4];;

let rec sumterminale l = match l with
|t::[] -> t
|t::q::r -> sum ((t+q)::r);;
sumterminale [3;7;4];;

question 3 

version recursive;;

let rec foldright f l acc = match l with
|t::[] -> f t acc
|t::q -> f t (foldleftrec f q acc);;
let f x y = x + y;;
foldright f [8;4] 2;;

let foldleft f acc l = foldleftrec f acc (reverse l);;
let rec foldleftrec f acc l = match l with
|t::[] -> f acc t
|t::q -> f (foldleftrec f q acc) t ;;
let rec reverse l = match l with
|[]->[]
|t::q-> reverse q @[t] ;;

foldleft f 6 [5;4;3;2;1];;

question 4
;;
let rec map f l = match l with
|[] -> []
|t::q -> (f t)::(map f q);;


question 5;;

let rec find f l = match l with
|[]->failwith "false"
|t::q when f t = true -> t
|t::q -> find f q;;

question 6;;

let rec max l = match l with
|h::[]->h
|h::t::q when t < h -> max (h::q)
|h::t -> max t;;

question 7;;

let count e l = match l with
|t::q when t = e -> countinterne e (1::q)
|t::q -> countinterne e (0::q);;

let rec countinterne e l = match l with
|t::[]->t
|t::r::q when r = e -> countinterne e ((t+1)::q)
|t::r::q -> countinterne e (t::q);;

count 3 [3;4;5;6;3;5;6;7;3;3;4];;

question 8 ;;

let more l x y = (count x l) > (count y l);;

moreter 3 4 [3;4;5;6;3;5;6;7;3;3;4];;

version recursive terminale:
;;

let moreter x y l = match l with
|x::y::q -> moetermint x y (1::1::q)
|_::y::q -> moetermint x y (0::1::q)
|x::_::q -> moetermint x y (1::0::q)
|_::_::q -> moetermint x y (0::0::q);;


let rec moetermint x y l = match l with
|t::q::[]->t>q
|t::r::x::q -> moetermint x y ((t+1)::r::q)
|t::r::y::q -> moetermint x y (t::(r+1)::q)
|t::r::z::q -> moetermint x y (t::r::q);;

question 9;;
deja recursif terminale;;
let rec concatenate l1 l2 = match l1, l2 with
|[], l2 -> l2
|t::q, l2 -> t::concatenate q l2;;

question 10;;

let rec inverse l = match l with
|[]->[]
|t::q-> (inverse q) @ [t];;

question 11;;

let rec croissante l = match l with
|t::[]-> true 
|t::q::r when q < t -> false
|t::q -> croissante q;;

question 12;;

let rec longuest_extra l = match l with
|t::q::r when t < q -> 1 + longuest_extra (q::r)
|t::q::r when t > q -> 10*longuest_extra (q::r)+1
|t::[] -> 1;;

longuest_extra [1;2;7;1;4;5;3;4;5;4];;

let rec w l max = match l with
	|[]->[]
	|max::q -> true :: w q max
	|t::q -> false :: w q max;;


let extrac l = match l with
	let lenght = inverse (longuest_extra l)
	let max = max l
	let truth = w lenght max
	
question 13;;

let rec equal l1 l2 = match l1, l2 with
|[],t when t != []-> false
|t::q, r::m when t=r -> equal q m
|t::q, r::m when t!= r -> false
|t,[] when t != []-> false
|[],[]-> true;;

question 14;;

let palindrome l1 = equal l1 (inverse l1) ;;

question 15;;


