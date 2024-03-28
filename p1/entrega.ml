#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;


(* Ejercicio 1 *)

(* Elementos para las pruebas *)

(* let afne = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;";; *)
(* let afd = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c;";; *)
(* let afn = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a";; *)

(* funciones auxiliares *)
let rec e_transicion= function 
  | [] -> false
  | Arco_af (_, _, Terminal "") :: _ -> true
  | _ :: resto ->  e_transicion resto;;


let rec indeterminismosE estado valor res= function
	| [] -> false
	| Arco_af (_, _, Terminal "") :: tl -> indeterminismosE estado valor res tl
	| Arco_af (Estado est, Estado estRes, Terminal v) :: tl -> if est = estado && v = valor && estRes != res then true else indeterminismosE estado valor res tl
	| _ :: tl -> indeterminismosE estado valor res tl;;

let rec indeterminismosL = function
	| [] -> false
	| Arco_af (Estado estado, Estado res, Terminal valor) :: tl -> if indeterminismosE estado valor res tl then true else indeterminismosL tl
	| _ :: tl -> indeterminismosL tl;;




let es_afne (Af (_, _, _, arcos, _)) =
  e_transicion (list_of_conjunto arcos);;
  
let es_afn af =
	match af with 
	| Af (_, _, _, arcos, _) -> indeterminismosL (list_of_conjunto arcos);;
	
let es_afd af = 
	not (es_afn af) && not (es_afne af);;
	

(*-----------------------------------------------------------------------------------------------*)

(* Ejercicio 2 *)








	
