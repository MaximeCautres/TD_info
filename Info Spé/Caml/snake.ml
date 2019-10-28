#load "graphics.cma" ;;
open Sys;;
open Graphics;;

let u = 30;;
let size = 16;;
let format = (size * u);;
let format_str = string_of_int format;;

type 'a vect = {mutable x : 'a; mutable y :'a};;
let nw x_ y_ = {x = x_; y = y_};;
let (++) v w = {x = v.x + w.x; y = v.y + w.y};;

let snake () = 
	let p = nw (size / 2) (size / 2) 
	and map = Array.make (format*format) p
	and head = ref 0 and tail = ref 0 and d = (nw 0 0) 
	and tm = 0.1 in
	let inmap () = p.x > 0 && p.x < size && p.y > 0 && p.y < size in 
	let collision p = 
		let rec aux h = match p = map.(h), h = !tail with
			|true, _ -> false
			|_, true -> true
			|_, false -> aux (h - 1) in 
		aux (!head - 1) in
	let rec newfruit () = let f = {x = Random.int size, y = Random.int size} in
		if f = p || collision f then newfruit () else (f; set_color red;
				fill_rect (map.(fruit).x * u) (map.(fruit).y * u) u u ) in
	let eval p = if p then 1 else 0 in
	let graph () = 
		open_graph (format_str^"x"^format_str^"+50+50");
		set_window_title "Snake_2.0";
		set_color black;
		fill_rect 0 0 format format in
	graph () in 
	let beg = time () and fruit = newfruit () and togrow = ref 0 in
	while inmap () and not collision p do
		if Key_pressed () then begin
			d.x <- (eval (key = 'd')) - (eval (key = 'q'));
			d.y <- (eval (key = 'z')) - (eval (key = 's')) end
		if (time () -. beg) > !t then begin
			t := !t +. tm;
			p.x <- (p ++ d).x; p.y <- (p ++ d).y;
			head := (!head + 1) mod (format*format);
			map.(head) <- p;
			set_color green;
			fill_rect (map.(head).x * u) (map.(head).y * u) u u ;
			if np = fruit then begin
				!togrow := !togrow + 1;
				let fruit = newfruit () end
			if !togrow <> 0 
				then !togrow := !togrow - 1
				else begin
				set_color black;
				fill_rect (map.(tail).x * u) (map.(tail).y * u) u u ;
				tail := (!tail + 1) mod (format*format);
				end; end;
	done;;


