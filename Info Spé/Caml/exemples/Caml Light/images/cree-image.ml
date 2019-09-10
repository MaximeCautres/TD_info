#open "graphics";;

include "exemples/Caml Light/Mandelbrot.ml";;
let sz = size_x();;
let image = get_image 0 0 sz sz;;
clear_graph();;

include "exemples/Caml Light/tores.ml";;
let sz = size_x();;
let image1 = get_image 0 0 sz sz;;
close_graph();;

let save_as_dumped path dumpedimage =
let canal_out = open_out_bin path
    in
      output_value canal_out dumpedimage;
      close_out canal_out;
;;

let save_as_ppm path dumpedimage =
  let f = open_out_bin (path ^ ".ppm") in
    output_string f "P6";
    output_char f `\n`;
    let w = vect_length (dumpedimage.(0)) and h = vect_length dumpedimage in
      output_string f (string_of_int w ^ " " ^ string_of_int h);
      output_char f `\n`;
      output_string f "255";
      output_char f `\n`;
      for i = 0 to h - 1 do
        for j = 0 to w - 1 do
           let b = dumpedimage.(i).(j) in
            let g = b lsr 8 in
              let r = g lsr 8 in
                output_byte f r;
                output_byte f g;
                output_byte f b;
        done;
      done;
      close_out f
;;

let str = string_of_int (2 * sz)
in
  open_graph (" " ^ str ^ "x" ^ str);
  draw_image image 0 0;
  draw_image image1 sz 0;
  draw_image image1 0 sz;
  draw_image image sz sz;
  let v = dump_image (get_image 0 0 (2 * sz) (2 * sz))
  in let path = "exemples/Caml Light/images/image"
  in
    save_as_dumped path v;
    save_as_ppm path  v;    
;;
