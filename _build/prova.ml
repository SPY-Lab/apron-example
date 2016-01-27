
open Apron;;
open Mpqf;;
open Format;;

let var_x = Var.of_string "x";;


(* box manager*)
let manBox = Box.manager_alloc();;


(*Making an environment from a set of integer (empty) and real variables.*)
 let env = Environment.make
    [||]
    [|var_x|];;


let interval1 =  Interval.of_scalar (Scalar.of_int 0) (Scalar.of_infty 1);;


     let abs1 = Abstract1.of_box manBox env [|var_x|]
    [|
      interval1;
    |];;


	(*vedere l'intervallo di una data variabile*)
	let intervallo = Abstract1.bound_variable manBox abs1 var_x;;


printf "abs1=%a@."
    Abstract1.print abs1
;;		

			let print_infty (valore: Scalar.t) =
					if (Scalar.equal (Scalar.of_infty(1)) valore)
						then printf"+oo"
					else
						if (Scalar.equal (Scalar.of_infty(-1)) valore)
							then printf"-oo"
						else
							printf"%a" Scalar.print valore;;




(*let print_box abs man =
		(* si potrebbe far il chech che sia box *)
		let env = Abstract1.env abs in
			let variabili = Environment.vars env in
				let dim = Array.length variabili in
					printf"[| "
						variabili.iter (stampa_variabili abs man);;
*)

let stampa_variabili var abs man =
		let intervallo = Abstract1.bound_variable man abs var in
			printf"%a: [" Var.print var;
			print_infty intervallo.inf;
			printf ",";
			print_infty intervallo.sup;
			printf "]";;

stampa_variabili var_x abs1 manBox;;