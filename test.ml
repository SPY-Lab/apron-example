(* ocamlbuild -pkgs apron.boxMPQ,apron,apron.apron,apron.octMPQ test.native -- *)

(* Tests of Apron's functions *)

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

(* Scalar.of_infty  Create a scalar of type Float with the value multiplied
 by infinity (resulting in minus infinity, zero, or infinity *)
let interval1 =  Interval.of_scalar (Scalar.of_int 0) (Scalar.of_infty 1);;

(* Change interval *)
Interval.set_infsup interval1  (Scalar.of_int 0) (Scalar.of_infty 1);;

(* Create an abstract domain Box *)
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

(* Join *)
let joinBox = Abstract1.join manBox abs1 abs2;;

(* Meet *)
let meetBox = Abstract1.meet manBox abs1 abs2;;

(* Watch the interval of a variable *)
let intervallo = Abstract1.bound_variable manBox abs1 var_x;;

(* Create an Abstract Domain Oct *)
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

(* Join *)
let joinOct = Abstract1.join manOct abs3 abs4;;

(* Meet *)
let meetOct = Abstract1.meet manOct abs3 abs4;;


(* Print box *)
printf "abs1=%a@.abs2=%a@.joinBox=%a@.meetBox=%a"
  Abstract1.print abs1
  Abstract1.print abs2
  Abstract1.print joinBox
  Abstract1.print meetBox
;;

printf("\n");;
printf "Intervallo var_x: %a" Interval.print intervallo;;	
printf("\n");;

printf "sup: %a"
Scalar.print intervallo.sup;;

(* Print oct *)
printf "abs3=%a@.abs4=%a@.joinOct=%a@.meetOct=%a"
  Abstract1.print abs3
  Abstract1.print abs4
  Abstract1.print joinOct
  Abstract1.print meetOct
;;

printf("\n");;
printf "Intervallo var_x: %a" Interval.print intervallo;;	

printf("\n");;



