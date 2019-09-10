#open "graphics";;
open_graph " 280x300";;
set_window_title "Очи чëрные";;

let x = ref 285;;
moveto 5 !x;;

let base = "exemples/Caml Light/unicode/";;

let f = open_in (base ^ "Очи чëрные.txt")
in
  try
    while true do
    let s = input_line f in
      print_endline s;
      x := !x - 15;
      moveto 5 !x;
      draw_string s;       
    done
  with End_of_file -> close_in f
;;

x := !x - 30;;
moveto 5 !x;;
draw_string "Et bien sûr les caractères accentués";;
x := !x - 15;;
moveto 5 !x;;
draw_string "devraient apparaître correctement";;
x := !x - 15;;
moveto 5 !x;;
draw_string "dans cette fenêtre graphique!";;
