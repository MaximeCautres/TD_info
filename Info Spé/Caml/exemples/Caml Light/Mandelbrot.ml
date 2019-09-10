#open "graphics";;

let c = make_vect 9 0;;

c.(0) <- rgb 0 0 120;;
c.(1) <- rgb 0 0 150;;
c.(2) <- rgb 0 0 180;;
c.(3) <- rgb 0 0 238;;
c.(4) <- rgb 0 120 0;;
c.(5) <- rgb 0 150 0;;
c.(6) <- rgb 0 180 0;;
c.(7) <- rgb 0 238 0;;
c.(8) <- rgb 255 0 0;;

let pixelcolor a b =
   let x = ref a and y = ref b and n = ref 0 in
      while !x *. !x +. !y *. !y <=. 4. && !n <= 63 do
         let t = ref (!x *. !x -. !y *. !y +. a) in
            y := 2. *. !x *. !y +. b;
            x := !t;
            n := !n + 1
      done;
      if !n = 0 then set_color c.(0)
      else if !n = 1 then set_color c.(1)
      else if !n = 2 then set_color c.(2)
      else if !n >= 3 && !n < 6 then set_color c.(3)
      else if !n >= 6 && !n < 11 then set_color c.(4)
      else if !n >= 11 && !n < 16 then set_color c.(5)
      else if !n >= 16 && !n < 31 then set_color c.(6)
      else if !n >= 31 && !n < 61 then set_color c.(7)
      else set_color c.(8);
      let u = floor ((a +. 2.) *. 100.) and v = floor ((b +. 2.) *. 100.) in
         plot (int_of_float u) (int_of_float v);;

let main () =
   let x = ref (- 2.) and dx = 0.01 and dy = 0.01 in
      while !x <=. 2. do
         let y = ref (- 2.) in
            while !y <=. 2. do
               pixelcolor !x !y;
               y := !y +. dy;
            done;
            x := !x +. dx;
      done;;

open_graph " 400x400";;
set_window_title "Ensemble de Mandelbrot";;
main ();;
