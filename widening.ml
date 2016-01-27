(* test widening *)
(* to compile: ocamlbuild -pkgs apron.apron,apron.boxMPQ,apron.octMPQ widening.native -- *)
open Apron;;
open Mpqf;;
open Format;;

(* Function declaration *)

let print_infty (valore: Interval.t) =
	if (Interval.is_top valore)
		then printf"[-oo; +oo] "
	else 
	if (Interval.is_bottom valore)
		then printf"[+oo; -oo] "
	else
		printf"%a " Interval.print valore;;

let print_variable_as_interval var abs man =
		let interval = Abstract1.bound_variable man abs var in
			printf"%a:" Var.print var;
			print_infty interval;;


let print_abs_domain_as_box abs man =
	let env = Abstract1.env abs in
		let variabili = Environment.vars env in
			let (a, b) = variabili in
				printf "\nInteger:";
				printf "{ ";
				if Array.length a != 0 then
					for i = 0 to Array.length a - 1 do
						print_variable_as_interval a.(i) abs man;
					done;
				printf "}"; print_newline();
				printf "Real:";
				printf "{";
				if Array.length b != 0 then
					for i = 0 to Array.length b - 1 do
						print_variable_as_interval a.(i) abs man;
					done;
				printf "}"; print_newline();;

(* Inizialize variable *)
let var_x = Var.of_string "x";;


(* Box manager*)
let manBox = Box.manager_alloc();;


(* Making an environment from a set of integer and real variables (empty) *)
 let env = Environment.make
    [| var_x|]
    [||];;

(* Inizialize intervals *)
let interval_for_x =  Interval.of_scalar (Scalar.of_int 3) (Scalar.of_int 5);;
let other_interval_for_x =  Interval.of_scalar (Scalar.of_int 1) (Scalar.of_int 3);;

(* Create abstract domain *)
let abs1 = Abstract1.of_box manBox env [|var_x|]
    [|
      interval_for_x;
    |];;

let abs2 = Abstract1.of_box manBox env [|var_x;|]
    [|
      other_interval_for_x;
    |];;


let abs_top = Abstract1.top manBox env;;
let abs_bottom = Abstract1.bottom manBox env;;

(* Test widening *)
let abs_widening = Abstract1.widening manBox abs1 abs2;;
let abs_join = Abstract1.join manBox abs1 abs2;;

let abs_widening_abs1_top = Abstract1.widening manBox abs1 abs_top;;
let abs_join_abs1_top = Abstract1.join manBox abs1 abs_top;;

let abs_widening_abs1_bottom = Abstract1.widening manBox abs_bottom abs1;;
let abs_join_abs1_bottom = Abstract1.join manBox abs_bottom abs1;;

let abs_widening_top_bottom = Abstract1.widening manBox  abs_bottom abs_top;;
let abs_join_top_bottom = Abstract1.join manBox abs_top abs_bottom;;


(* Print the results *)
printf"\n";;

printf "abs1: ";;
print_abs_domain_as_box abs1 manBox;;
printf"\n";;

printf "abs2: ";;
print_abs_domain_as_box abs2 manBox;;
printf"\n";;

printf "Join abs1 abs2: ";;
print_abs_domain_as_box abs_join manBox;;
printf"\n";;

printf "Widening abs1 abs2: ";;
print_abs_domain_as_box abs_widening manBox;;
printf"\n";;

printf "Join abs1 top: ";;
print_abs_domain_as_box abs_join_abs1_top manBox;;
printf"\n";;

printf "Widening abs1 top: ";;
print_abs_domain_as_box abs_widening_abs1_top manBox;;
printf"\n";;

printf "Join bottom top: ";;
print_abs_domain_as_box abs_join_top_bottom manBox;;
printf"\n";;

printf "Widening bottom top: ";;
print_abs_domain_as_box abs_widening_top_bottom manBox;;
printf"\n";