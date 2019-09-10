#open "unix";;
#open "graphics";;

include "exemples/Caml Light/Mandelbrot.ml";;
let sz = size_x();;
let image = get_image 0 0 sz sz;;
clear_graph();;
include "exemples/Caml Light/tores.ml";;
let image1 = get_image 0 0 sz sz;;
close_graph();;
let str = string_of_int (2 * sz) in
  open_graph (" " ^ str ^ "x" ^ str);;
draw_image image 0 0;;
draw_image image1 400 0;;
draw_image image1 0 400;;
draw_image image 400 400;;
let v = dump_image (get_image 0 0 (size_x()) (size_x()))
in
  let canal_out = open_out_bin "exemples/Caml Light/images/image"
  in
    output_value canal_out v;
    close_out canal_out
;;

