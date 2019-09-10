exception Erreur;;
exception Essai;;
exception Fin;;
exception Trouv�;;

let rec print_list l = match l with
    t :: r -> print_char t; print_list r;
    | [] -> print_string " | "
;;

let imprime t =
  print_string "\nr�sultat:\n";
  for i = 0 to 8 do
    for j = 0 to 8 do
      print_list t.(i).(j);
    done;
    print_string "\n-----------------------------------\n";
  done
;;

(*
let rec except e l1 = match l1 with
    | [] -> []
    | t :: q -> if t = e then except e q else t :: except e q
;;
*)

let copie t =
  let tt = make_matrix 9 9 [] in
    for i = 0 to 8 do
      for j = 0 to 8 do
        tt.(i).(j) <- t.(i).(j);
      done;
    done;
    tt
;;

let rec r�duis_aux t i j e =
  for k = 0 to 8 do
    if k <> j then
      begin
        let l1 = list_length t.(i).(k) in
          if l1 = 0 then raise Erreur
          else
            begin
              t.(i).(k) <- except e t.(i).(k);
              let l2 = list_length t.(i).(k) in
                if l2 = 1 && l1 <> l2 then r�duis_aux t i k (hd t.(i).(k));
            end;
      end;
    if k <> i then
      begin
        let l1 = list_length t.(k).(j) in
          if l1 = 0 then raise Erreur
          else
            begin
              t.(k).(j) <- except e t.(k).(j);
              let l2 = list_length t.(k).(j) in
                if l2 = 1 && l1 <> l2 then r�duis_aux t k j (hd t.(k).(j));
            end;
      end;
  done;
  let p = i / 3 * 3 and q = j / 3 * 3 in
    for u = p to p + 2 do
      for v = q to q + 2 do
        if u <> i && v <> j then
          begin
            let l1 = list_length t.(u).(v) in
              if l1 = 0 then raise Erreur
              else
                begin
                  t.(u).(v) <- except e t.(u).(v);
                  let l2 = list_length t.(u).(v) in
                    if l2 = 1 && l1 <> l2 then r�duis_aux t u v (hd t.(u).(v));
                end;
          end;
      done;
    done;
;;

let suivant t k =
  let m = ref 9 and n = ref (k + 1) in
    try
      for i = k + 1 to 80 do
        let l = list_length t.(i / 9).(i mod 9) in
          if l > 1 && l < !m then
            begin
              m := l;
              n := i;
              if l = 2 then raise Trouv�;
            end;
      done;
      !n
    with Trouv� -> !n
;;

let r�duis t i j e =
  try
    r�duis_aux t i j e;
  with Erreur -> ()
;;

let rec essai_erreur_aux f t k l = match l with
    h :: r -> let (i, j) = (k / 9, k mod 9) and tt = copie t in
          tt.(i).(j) <- [h];
          r�duis tt i j h;
          f tt (suivant tt k);
          essai_erreur_aux f t k r;
    | [] -> ()
;;

let erreur t =
  try
    for i = 0 to 8 do
      for j = 0 to 8 do
        let l = list_length t.(i).(j) in
          if l = 0 then raise Erreur
      done;
    done;
  with Erreur -> raise Erreur
;;

let essai t =
  try
    for i = 0 to 8 do
      for j = 0 to 8 do
        let l = list_length t.(i).(j) in
          if l > 1 then raise Essai
      done;
    done;
  with Essai -> raise Essai
;;

let rec essai_erreur t k =
  try
    erreur t;
    essai t;
    imprime t;
    raise Fin;
  with Erreur -> ()
    | Essai -> essai_erreur_aux essai_erreur t (k mod 81) t.((k mod 81) / 9).(k mod 9)
;;

let prepare t =
  for i = 0 to 8 do
    for j = 0 to 8 do
      if list_length t.(i).(j) = 1 then r�duis_aux t i j (hd t.(i).(j));
    done;
  done;
;;

(* t est un tableau de listes d'entiers: au d�part les cases connues du sudoku contiennent
une liste singleton  et les cases inconnues la liste compl�te [1;2;3;4;5;6;7;8;9] *)

let r�sous t =
  try
    prepare t;
    essai_erreur t 0;
    print_string "erreur dans sudoku";
    print_newline();
  with Fin -> ()
    | Erreur -> print_string "erreur dans sudoku";
        print_newline();
;;


(* fonction lisant et traitant une par une toutes les grilles archiv�es dans un fichier *)

let traite fichier =
  let t1 = sys__time() in
    let canalin = open_in fichier in
      try
        let d = ref "" in
          while true do
            print_newline();
            d := input_line canalin;
            let tab = make_matrix 9 9 [`1`; `2`; `3`; `4`; `5`; `6`; `7`; `8`; `9`] in
              print_string "grille:\n";
              for i = 0 to 8 do
                d := input_line canalin;
                for j = 0 to 8 do
                  let n = !d.[j] in
                    if n = `_` then print_char ` `
                    else
                      begin
                        print_char n;
                        tab.(i).(j) <- [n];
                      end;
                    print_string " | ";
                done;
                print_string "\n-----------------------------------\n";
              done;
              r�sous tab;
              d := input_line canalin;
          done;
      with End_of_file ->
            close_in canalin;
            print_string "dur�e: "; print_float (sys__time() -. t1); print_string " s";
            print_newline()
;;

cd "exemples/sudokus";;
traite "listesudokus.txt";;
