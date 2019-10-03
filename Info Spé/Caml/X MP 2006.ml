
(* Part 1 *)

let nc = 27;;

let alpha l = if l = 0 then " " else Char.escaped (char_of_int (l + 96));;

let indice c = if c = '$' then 0 else int_of_char c - 96;;

let rec imprimer w = match w with
	|[] -> ()
	|l::w' -> (imprimer w'; print_string (alpha l));;

imprimer [1;2;3];;

let inverseDe w = 
	let rec aux w1 w2 = match w1 with
		|[] -> w2
		|m::w1' -> aux w1' (m::w2)
	in aux w [];;

inverseDe [1;2;3];;

(* Part 2 *)

type dictTab = VideT | NoeudT of dictTab array;;
let dict_vide = NoeudT(Array.make nc VideT);;
let dict_mot_vide = let d = Array.make nc VideT in d.(0) <- dict_vide; d;;

