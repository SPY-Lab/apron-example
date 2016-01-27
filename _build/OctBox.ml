
open Apron;;
open Mpqf;;
open Format;;

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



let var_x = Var.of_string "x"
let var_y = Var.of_string "y"
let var_z = Var.of_string "z"


(* Box manager *)
let manBox = Box.manager_alloc();;

(* Oct manager*)
let manOct = Oct.manager_alloc();;

(* Making an environment from a set of integer and real variables (empty) *)
 let env = Environment.make
    [| var_y |]
    [||];;

let interval_for_y =  Interval.of_scalar (Scalar.of_int 2) (Scalar.of_int 7);;


let other_interval_for_y =  Interval.of_scalar (Scalar.of_int 4) (Scalar.of_int 10);;

(* Making abstract domain of box *)
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




(* Print all the variables in the environment *)
printf "abs box: ";;
print_abs_domain_as_box abstract_domain_box manBox;;
print_abs_domain_as_box other_abstract_domain_box manBox;;


printf "abs oct: ";;
print_abs_domain_as_box abstract_domain_oct manOct;;
print_abs_domain_as_box other_abstract_domain_oct manOct;;


let leastUpperBoundBox = Abstract1.join manBox abstract_domain_box other_abstract_domain_box in
	printf "Least upper bound Box:";
	print_abs_domain_as_box leastUpperBoundBox manBox;;

let leastUpperBoundOct = Abstract1.join manOct abstract_domain_oct other_abstract_domain_oct in
	printf "Least upper bound Oct:";
	print_abs_domain_as_box leastUpperBoundOct manOct;
	print_newline;
	printf "print ad Oct: %a"
	Abstract1.print leastUpperBoundOct;;

(*let greatestLowerBound = Abstract1.meet manBox abstract_domain other_abstract_domain in
	printf "Greatest lower bound:";
	printf "%a" Abstract1.print greatestLowerBound;
	print_abs_domain_as_box greatestLowerBound manBox;;*)
