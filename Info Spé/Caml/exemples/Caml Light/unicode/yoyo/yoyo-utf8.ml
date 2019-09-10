let int_of_hexbyte s =
  let int_of_hexdigit x =
    if int_of_char x < int_of_char `9` then
      int_of_char x - int_of_char `0`
    else int_of_char x - int_of_char `A` + 10
  in 16 * int_of_hexdigit s.[0] + int_of_hexdigit s.[1]
;;

let hex s =
  let hexdigit_of_int i = let c = if i < 10 then char_of_int (i + int_of_char `0`) else char_of_int (i - 10 + int_of_char `A`)
    in string_of_char c
  in
    let rec hex_of_int n =
      if n >= 16 then hex_of_int (n / 16) ^ (hexdigit_of_int (n mod 16))
      else (hexdigit_of_int n)
    in
      for i = 0 to string_length s - 1 do
        print_string (hex_of_int (int_of_char s.[i]));
        print_string " ";
      done;
      print_newline ()
;;

let base = "exemples/Caml Light/unicode/yoyo/";;

let f = open_out (base ^ "yoyo-mixte.txt") in
  let ox s = output_byte f (int_of_hexbyte s)
  and space () = output_byte f (int_of_char ` `) in
    ox "EB";
    space ();
    ox "65"; ox "CC"; ox "88";
    space ();
    ox "D0"; ox "B5"; ox "CC"; ox "88";
    close_out f
;;

let f = open_in (base ^ "yoyo-mixte.txt") in
let s = input_line f
in
  print_endline s;
  close_in f;
  hex s
;;

let f = open_out (base ^ "yoyo-utf8_1.txt") in
  let ox s = output_byte f (int_of_hexbyte s)
  and space () = output_byte f (int_of_char ` `) in
    ox "C3"; ox "AB";
    space ();
    ox "65"; ox "CC"; ox "88";
    space ();
    ox "D0"; ox "B5"; ox "CC"; ox "88";
    close_out f
;;

let f = open_out (base ^ "yoyo-utf8_2.txt")
in
  output_string f "ë ë ё";
  close_out f
;;

let f = open_in (base ^ "yoyo-utf8_1.txt") in
let s = input_line f
in
  close_in f;
  hex s;
  print_endline s
;;

let f = open_in (base ^ "yoyo-utf8_2.txt") in
let s = input_line f
in
  close_in f;
  hex s;
  print_endline s
;;

let ox s = string_of_char (char_of_int (int_of_hexbyte s))
in
  (ox "EB") ^ " " ^ (ox "C3" ^ ox "AB") ^ " " ^ (ox "65" ^ ox "CC" ^ ox "88") ^ " " ^ (ox "D0" ^ ox "B5" ^ ox "CC" ^ ox "88")
;;

"\235 \195\171 \101\204\136 \208\181\204\136";;
