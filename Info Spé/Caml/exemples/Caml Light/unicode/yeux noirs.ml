let base = "exemples/Caml Light/unicode/";;

let f = open_in (base ^ "yeux noirs.txt")
in
  try
    while true do
      print_endline (input_line f);
    done
  with End_of_file -> close_in f
;;

let f = open_in (base ^ "Очи чëрные.txt")
in
  try
    while true do
      print_endline (input_line f);
    done
  with End_of_file -> close_in f
;;
