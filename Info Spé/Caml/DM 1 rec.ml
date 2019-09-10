let rec p x n =
	if n = 0 then 1
	else x * p x (n-1);;
let xn n =
	for k = 0 to p 2 n - 1 do	
		for i = 1 to n do
			let r = (k + 1 - p 2 (i-1)) mod p 2 i in
			if r = 0 then 
			begin
				let q = (k + 1 - p 2 (i-1)) / p 2 i in
				if q mod 2 = 0 then print_int 0
				else print_int 1 ;
			end;
		done;
	done;;
xn 3;;

let rec nb n s =
	if n = 0 then s
	else begin if n = 1 then 2*s+1
	else nb (n-1) (2*s+1) end;;
let rec nb n = match n with
|0->0
|_->2*(nb (n-1)) + 1;;

nb 2;;
let t01 n =
	let b = Array.make (nb n + 1) 0 in
	for i = 0 to (nb n /2) do b.(2*i+ 1)<- 1 done;
	for i = 0 to nb n do print_int b.(i) done;;

t01 3;;
let t01 n = let p = nb n in
let t = Array.make (p+1) 0 in
for i = 0 to (p-1)/2 do t.(2*i + 1)<-1 done; t;;

	
let fusion t n= let b = t01 n in  
	let xnn = Array.make (nb n * 2 + 1) 0 in
	for i = 1 to (nb n) do 
		xnn.(2*i-1)<-t.(i-1);
		xnn.(2*i)<- b.(i);
	done; xnn ;;
fusion [|0;0;1|] 2;;

let rec recfusion t n j=
	if j < n+1 then begin
		let b = Array.make (nb j 0 + 1) 0 in
		for i = 0 to (nb j 0 /2) do
			b.(2*i+1)<- 1; 
		done ;
		let xnn = Array.make (nb j 0 * 2 + 1) 0 in
		for i = 1 to (nb j 0) do 
			xnn.(2*i-1)<-t.(i-1);
			xnn.(2*i)<- b.(i);
		done;
		for i =0 to (Array.length xnn - 1) do print_int xnn.(i) done;
		print_endline " " ;
		recfusion xnn n (j+1);
	end ;;
recfusion [|0|] 5 1;;

let rec pliage n = match n with 
|0->[||]
|1->[|1|]
|_->fusion (pliage (n-1)) (n-1);;
pliage 3;;
let rec bin n l = 
	 if n < 2 then l@[n mod 2]
	 else bin (n/2) (l@[n mod 2]);;

let rec bin n = match n with
|n when n = 0 || n = 1 -> [n]
|_-> n mod 2 :: bin (n/2);;

bin 48;;

let rec bit k l = 
	if List.hd l = 1 then print_int (k mod 2) 
	else bit (k/2) ([k mod 2]@l);;
bit 3 [0];;

let rec bit k = match k with
|0->failwith "Le chiffre n'existe pas"
|1->0
|_->let b1::b2::f = bin k in if b1 = 0 then bit (k/2) else b2 ;;
bit 4;;




let xn1 n = 
for i = 1 to p 2 n - 1 do bit i [0] done;;
xn1 3;;

let l = [4;5;6;8;0] in
l@[3];;

let rec m e s =
if e = [] then print_int 1
else 
begin
s@[List.hd e];
m (List.tl e) s;
end;; 

let rec c l = match l with
|[]->[]
|t::q-> (1-t)::(c q);;
c [0;0;1];;

let rec m l = match l with
|[]->[]
|t::q ->(c q)@[t];;
m [3;2;1];;
m (c [0;0;1]);; 
let xnn xn = xn@[0]@(m(c xn));;
xnn [];;


let rec pliage n = match n with
|0->[]
|_->pliage (n-1) @ [0] @ (m(c (pliage (n-1))));;
pliage 4;;

