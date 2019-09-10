(************************************************************************************************
                  Bibliothèque big_vect (Jean Mouric , Denis Cazor le 03/03/2014)
           Pour simuler sur un système 32 bits des tableaux de tailles supérieures à 4 Mio 
                           Exemple d'utilisation dans tris.ml
************************************************************************************************)

let big_vect_chunk_log = sys__word_size - 11;; (* inférieur à sys__word_size - 10 *)
let big_vect_chunk_size = 1 lsl big_vect_chunk_log;; (* puissance de deux inférieure à sys__max_vect_length *)
let big_vect_chunk_maxi = big_vect_chunk_size - 1;;

let make_big_vect n ini =
   let q = n lsr big_vect_chunk_log and r = n land big_vect_chunk_maxi in
      let res = if r = 0 then fvect__make_vect q [||]
         else fvect__make_vect (q + 1) [||]
      in
         for i = 0 to q - 1 do
            (*res.(i) <- make_vect big_vect_chunk_size ini*)
            fvect__vect_assign res i (fvect__make_vect big_vect_chunk_size ini)
         done;
         if r <> 0 then
            (*res.(q) <- make_vect r ini;*)
            fvect__vect_assign res q (fvect__make_vect r ini);
         res
;;

let big_vect_item bv n =
   let q = n lsr big_vect_chunk_log and r = n land big_vect_chunk_maxi in
      (*bv.(q).(r)*)
      fvect__vect_item (fvect__vect_item bv q) r
;;

let big_vect_assign bv n x =
   let q = n lsr big_vect_chunk_log and r = n land big_vect_chunk_maxi in
      (*bv.(q).(r) <- x*)
      fvect__vect_assign (fvect__vect_item bv q) r x
;;

let big_vect_length bv =
   let q = fvect__vect_length bv - 1 in
      (*q * big_vect_chunk_size + vect_length bv.(q)*)
      q * big_vect_chunk_size + fvect__vect_length (fvect__vect_item bv q)
;;

let make_vect = make_big_vect;;
let vect_item = big_vect_item;;
let vect_assign = big_vect_assign;;
let vect_length = big_vect_length;;

#open "big_vect";;

(************************************************************************************************
                  Bibliothèque big_vect (Jean Mouric , Denis Cazor le 03/03/2014)
************************************************************************************************)