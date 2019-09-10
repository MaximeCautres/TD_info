#open "graphics";;

let rec vonKoch (a,b) (c,d) =
let norme (x,y) = sqrt (x *. x +. y *. y) in
if norme (c -. a,d -. b) <. 5. then
begin 
	moveto (int_of_float a) (int_of_float b);
	lineto (int_of_float c) (int_of_float d)
end
else
begin
	vonKoch (a,b) (a +. (c -. a) /. 3. , b +. ( d -. b) /. 3.);
	vonKoch (a +. (c -. a) /. 3. , b +. ( d -. b) /. 3.) ((a +. c) /. 2. -. (d -. b) /. (2. *. sqrt 3.) , (b +. d) /. 2. +. (c -. a) /.(2. *. sqrt 3.));
	vonKoch ((a +. c) /. 2. -. (d -. b) /. (2. *. sqrt 3.) , (b +. d) /. 2. +. (c -. a) /. (2. *. sqrt 3.)) (a +. 2. *. (c -. a) /. 3. , b +. 2. *. (d -. b) /. 3.);
	vonKoch (a +. 2. *. (c -. a) /. 3. , b +. 2. *. (d -. b) /. 3.) (c,d)
end;;

open_graph " 560x200";;
set_window_title "Courbe de Von Koch";;
set_color red;;
vonKoch (10.,10.) (550.,10.);;
