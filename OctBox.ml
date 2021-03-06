(* to compile: ocamlbuild -pkgs apron.apron,apron.boxMPQ,apron.octMPQ OctBox.native -- *)

(* Test of interval of Box and Oct *)

open Apron;;
open Mpqf;;
open Format;;
(* Print functions *)

let print_infty (valore: Interval.t) =
	if (Interval.is_top valore)
		then printf"[-oo ;+oo] "
	else 
	if (Interval.is_bottom valore)
		then printf"[+oo ;-oo] "
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


(* Definition of variables *)
let var_x = Var.of_string "x"
let var_y = Var.of_string "y"
let var_z = Var.of_string "z"


(* Box manager *)
let manBox = Box.manager_alloc();;

(* Oct manager*)
let manOct = Oct.manager_alloc();;


let manPolka = Polka.manager_alloc_loose();;

(* Creating environment from a set of integer and real variables (empty) *)
 let env = Environment.make
    [| var_y |]
    [||];;

(* Defining interval for y *)
let interval_for_y =  Interval.of_scalar (Scalar.of_int 2) (Scalar.of_int 7);;

(* Defining an other interval for y *)
let other_interval_for_y =  Interval.of_scalar (Scalar.of_int 4) (Scalar.of_int 10);;

(* Making abstract domains of box *)
let abstract_domain_box = Abstract1.of_box manBox env 
	[| var_y |]
    [| interval_for_y|];;

let other_abstract_domain_box = Abstract1.of_box manBox env 
	[| var_y|]
    [| other_interval_for_y |];;

(* Making abstract domain of Oct *)
let abstract_domain_oct = Abstract1.of_box manOct env 
	[| var_y|]
    [| interval_for_y |];;

let other_abstract_domain_oct = Abstract1.of_box manOct env 
	[| var_y |]
    [| other_interval_for_y |];;

(* Making abstract domain of Polka *)
let abstract_domain_polka = Abstract1.of_box manPolka env 
	[| var_y|]
    [| interval_for_y |];;

let other_abstract_domain_polka = Abstract1.of_box manPolka env 
	[| var_y |]
    [| other_interval_for_y |];;


(* Print all the variables in the environment *)
printf "abs box: ";;
print_abs_domain_as_box abstract_domain_box manBox;;
print_abs_domain_as_box other_abstract_domain_box manBox;;


printf "abs oct: ";;
print_abs_domain_as_box abstract_domain_oct manOct;;
print_abs_domain_as_box other_abstract_domain_oct manOct;;

printf "abs polka: ";;
print_abs_domain_as_box abstract_domain_polka manPolka;;
print_abs_domain_as_box other_abstract_domain_polka manPolka;;

(* Print the least upper bound *)
let leastUpperBoundBox = Abstract1.join manBox abstract_domain_box other_abstract_domain_box in
	printf "Least upper bound Box:";
	print_abs_domain_as_box leastUpperBoundBox manBox;;

let leastUpperBoundOct = Abstract1.join manOct abstract_domain_oct other_abstract_domain_oct in
	printf "Least upper bound Oct:";
	print_abs_domain_as_box leastUpperBoundOct manOct;
	printf "\n";
	printf "print as Oct: %a"
	Abstract1.print leastUpperBoundOct;
	printf "\n";;
let leastUpperBoundPolka = Abstract1.join manPolka abstract_domain_polka other_abstract_domain_polka in
	printf "Least upper bound Oct:";
	print_abs_domain_as_box leastUpperBoundPolka manPolka;
	printf "\n";
	printf "print as Polka: %a"
	Abstract1.print leastUpperBoundPolka;;
