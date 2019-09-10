type rationelle = { num: int; den: int };;

let rec pgcd a b = match b with 
|0 -> a
|_ -> pgcd b (a mod b);;

let (+) x y = 
let nume = x.num*y.den + y.num*x.den and deno=x.den*y.den in 
let m = pgcd nume deno in
{num = (nume / m);den = (deno / m)};;

let mult x y = 
let nume = x.num*y.num and deno=x.den*y.den in 
let m = pgcd nume deno in
{num = (nume / m);den = (deno / m)};;

let ( / ) x y = if y.num = 0 then failwith "ERROR DIVISION BY ZERO" 
else {num = x.num * y.den ; den = y.num * x.den};;

let rec power x n = match n with
|0->{num=1;den=1}
|_->mult x (power x (n-1));;

let rec u n = match n with
|0 -> {num = 0;den = 1}
|_ -> mult {num = 1;den = 3} ((mult {num = 4;den = 1} (power (u (n-1)) 3)) + {num = 1;den = 2});;

u 3 ;;
"{num = 4; den = 8} * {num = 12; den = 2};;
{num = 4; den = 8} + {num = 12; den = 2};;
";;

type polynome = plein of int array | creux of (int*int) list ;;

let print poly = 
if poly.(0) = 0 then let a = ref "" else let a = ref string_of_int(poly.(0)) in
for i = 1 to Array.length(poly)-2 do
if not (poly.(i) = 0) then
a := (!a)^string_of_int(poly.(i))^"X^"^string_of_int(i)^"+";
done;
a := (!a)^string_of_int(poly.(Array.length(poly)-1))^"X^"^string_of_int(Array.length(poly)-1);
!a;;

print [|2;3;0;0;4;5|];;