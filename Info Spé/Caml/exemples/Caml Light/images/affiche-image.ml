#open "unix";;
#open "graphics";;

let canal_in = open_in_bin "exemples/Caml Light/images/image"
in
  let v = (input_value canal_in: color vect vect)
  in
    let sx = string_of_int (vect_length v.(0))
    and sy = string_of_int (vect_length v)
    in
      open_graph (" " ^ sx ^ "x" ^ sy);
      draw_image (make_image v) 0 0;
      close_in canal_in
;;
