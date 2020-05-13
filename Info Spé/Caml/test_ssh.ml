let rec facto n = match n with
  |0->1
  |_-> n * (facto (n-1));;

facto 10;;

let rec list_len l = match l with
  |[]->0
  |_::l'->1 + (list_len l');;

list_len [1;5;15;3];;
