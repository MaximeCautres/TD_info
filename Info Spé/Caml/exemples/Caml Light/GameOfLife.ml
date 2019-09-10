#open "graphics";;

let draw t u =
   let width = vect_length t.(0) and height = vect_length t in
      for j = 1 to width - 2 do
         for i = 1 to height - 2 do
         let x =t.(i).(j)in
            if x > 0 then
            begin
               if x = 1 then
               begin
                  set_color green;
                  u.(i).(j) <- true
               end
               else
               begin
                  let c = if x < 255 then 255 - x else 0 in
                     set_color (rgb c 0 0)
               end;
               fill_rect (5 * i) (5 * j) 5 5
            end
            else
            if u.(i).(j) then
            begin
               u.(i).(j) <- false;
               set_color white;
               fill_rect (5 * i) (5 * j) 5 5
            end
         done
      done
;;

let next t =
   let w = vect_length t.(0) and h = vect_length t in
      let s = make_matrix h w 0 in
         for j = 1 to w - 2 do
            for i = 1 to h - 2 do
               let n = t.(i).(j - 1) land 1 + t.(i).(j + 1) land 1
                  + t.(i - 1).(j - 1) land 1 + t.(i - 1).(j) land 1 + t.(i - 1).(j + 1) land 1
                  + t.(i + 1).(j - 1) land 1 + t.(i + 1).(j) land 1 + t.(i + 1).(j + 1) land 1
               in

                  if n < 2 or n > 3 then s.(i).(j) <- 0
                  else let x = t.(i).(j) in
                        if n = 3 then s.(i).(j) <- if x = 0 then 1 else if x < 255 then x + 2 else 255
                        else s.(i).(j) <- if x = 0 then 0 else if x < 255 then x + 2 else 255
            done
         done;
         for j = 1 to w - 2 do
            for i = 1 to h - 2 do
               t.(i).(j) <- s.(i).(j)
            done
         done
;;

open_graph " 800x600";;
set_window_title "Game of life";;

let keyPressed () =
   (wait_next_event [Poll]).keypressed
   && (wait_next_event [Key_pressed]).keypressed
in
   clear_graph ();
   let t = make_matrix 160 120 0 and u = make_matrix 160 120 false in
      for i = 30 to 60 do
         for j = 30 to 60 do
            t.(i).(j) <- random__int 2
         done
      done;
      while not (keyPressed ()) do
         draw t u;
         next t
      done
;;
