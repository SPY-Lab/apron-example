(* ocamlbuild -pkgs apron.boxMPQ,apron,apron.apron,apron.octMPQ test.native -- *)

open Apron;;
open Mpqf;;
open Format;;

(*vars definition*)
let var_x = Var.of_string "x";;
let var_y = Var.of_string "y";;


(* box manager*)
let manBox = Box.manager_alloc();;

(* oct manager *)
let manOct = Oct.manager_alloc();;

(*Making an environment from a set of integer (empty) and real variables.*)
 let env = Environment.make
    [||]
    [|var_x; var_y|];;

(*how to create an abstract domain TOP*)
let top1 = Abstract1.top manBox env;;

(*how to create an abstract domain BOTTOM*)
let bottom1 = Abstract1.bottom manBox env;;

(*how to create an interval TOP*)
let top2 = Interval.top;;

(*how to create an interval BOTTOM*)
let bottom2 = Interval.bottom;;

(*how to create an intervall of integer*)
let interval1 =  Interval.of_int 4 12;;

(*Scalar.of_infty moltiplica il valore per un infinititesimo, rendendolo quindi +inf o -inf*)
let interval1 =  Interval.of_scalar (Scalar.of_int 0) (Scalar.of_infty 1);;

(*cambia il valore di un intervallo*)
Interval.set_infsup interval1  (Scalar.of_int 0) (Scalar.of_infty 1);;

(*genera il valore astratto delle variabili sotto forma di box*)
     let abs1 = Abstract1.of_box manBox env [|var_x;var_y|]
    [|
      interval1;
      Interval.of_int 4 12;
    |];;

     let abs2 = Abstract1.of_box manBox env [|var_x;var_y|]
    [|
      Interval.of_int 0 14;
      Interval.of_int 4 12;
    |];;

	(*join*)
    let joinBox = Abstract1.join manBox abs1 abs2;;
    (*meet*)
	let meetBox = Abstract1.meet manBox abs1 abs2;;

	(*vedere l'intervallo di una data variabile*)
	let intervallo = Abstract1.bound_variable manBox abs1 var_x;;

(*genera il valore astratto delle variabili sotto forma di box*)
     let abs3 = Abstract1.of_box manOct env [|var_x;var_y|]
    [|
      interval1;
      Interval.of_int 4 12;
    |];;

     let abs4 = Abstract1.of_box manOct env [|var_x;var_y|]
    [|
      Interval.of_int 0 14;
      Interval.of_int 4 12;
    |];;

	(*join*)
    let joinOct = Abstract1.join manOct abs3 abs4;;
    (*meet*)
	let meetOct = Abstract1.meet manOct abs3 abs4;;


	(*stampa box*)
	printf "abs1=%a@.abs2=%a@.joinBox=%a@.meetBox=%a"
    Abstract1.print abs1
    Abstract1.print abs2
    Abstract1.print joinBox
    Abstract1.print meetBox
	;;
	printf("\n");;
    printf "Intervallo var_x: %a"
    Interval.print intervallo
  ;;	
  printf("\n");;

   printf "sup: %a"
 	Scalar.print intervallo.sup;;

(*stampa oct*)
	printf "abs3=%a@.abs4=%a@.joinOct=%a@.meetOct=%a"
    Abstract1.print abs3
    Abstract1.print abs4
    Abstract1.print joinOct
    Abstract1.print meetOct
	;;
	printf("\n");;
    printf "Intervallo var_x: %a"
    Interval.print intervallo
  ;;	
  printf("\n");;



