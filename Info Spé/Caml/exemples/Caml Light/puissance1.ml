let base, big_digit_length = if sys__word_size = 64 then 1000000000, 9 else 10000, 4;;

let rec normalize =
  fun
    | [] -> []
    | [t] -> if t < base then [t] else [t mod base; t / base]
    | (t1 :: t2 :: r) -> if t1 < base then t1 :: normalize (t2 :: r) else (t1 mod base) :: normalize ((t2 + t1 / base) :: r)
;;

let prefix +/ l1 l2 =
   let rec add_aux l1' l2' =
      match (l1', l2') with
      | ([], _) -> l2'
      | (_, []) -> l1'
      | (t1 :: r1, t2 :: r2) -> t1 + t2 :: add_aux r1 r2
   in normalize (add_aux l1 l2)
;;

let rec prefix */ l1 l2 =
   let mul l x =
      let rec mul_aux =
         fun
         | [] -> []
         | (t :: r) -> t * x :: mul_aux r
      in
         normalize (mul_aux l)
   in
      match l2 with
      | [] -> []
      | t :: r -> let l = mul l1 t in l +/ (0 :: l1 */ r)
;;

let rec prefix **/ l n =
   if n < 0 then failwith "exposant entier positif ou nul attendu"
   else if n = 0 then [1]
   else let p = l **/ (n asr 1) in if (n land 1 = 0) then p */ p else l */ p */ p
;;

let rec string_of_big_num =
   let string_of_big_digit n =
      let s = string_of_int n
      in let l = string_length s
         in
            if l >= big_digit_length then s
            else
               let t = make_string (big_digit_length - l) `0`
               in t ^ s
   in
      fun
      | [] -> ""
      | [a] -> string_of_int a
      | (t :: q) -> string_of_big_num q ^ string_of_big_digit t
;;

let somme s =
  let valeur c = int_of_char c - 48 in
    let t = ref 0 in
      for k = 0 to string_length s - 1 do t := !t + valeur s.[k] done;
      !t
;;

let s = string_of_big_num ([4444] **/ 4444);;
somme s, string_length s;;

(*
let rec puissance l n =
   if n = 1 then l
   else let ll = l */ l in if (n mod 2 <> 0) then l */ (ll **/ (n / 2)) else ll **/ (n / 2)
;;

let s = string_of_big_num (puissance [4444] 4444);;
somme s;;

let rec big_num_of_string s =
   if string_length s <= big_digit_length then [int_of_string s]
   else let l = string_length s in
         int_of_string (sub_string s (l - big_digit_length) (big_digit_length)) :: big_num_of_string (sub_string s 0 (l - big_digit_length));;

let add_s s1 s2 =
   let n1 = big_num_of_string s1 and n2 = big_num_of_string s2 in
      string_of_big_num (n1 +/ n2);;

let multiply_s s1 s2 =
   let n1 = big_num_of_string s1 and n2 = big_num_of_string s2 in
      string_of_big_num (n1 */ n2);;
*)

