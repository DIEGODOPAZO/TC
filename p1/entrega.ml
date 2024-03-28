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


(* Funciones ejercicio *)

let es_afne (Af (_, _, _, arcos, _)) =
  e_transicion (list_of_conjunto arcos);;
  
let es_afn af =
	match af with 
	| Af (_, _, _, arcos, _) -> indeterminismosL (list_of_conjunto arcos);;
	
let es_afd af = 
	not (es_afn af) && not (es_afne af);;
	

(*-----------------------------------------------------------------------------------------------*)

(* Ejercicio 2 *)


(*-----------------------------------------------------------------------------------------------*)

(* Ejercicio 3 *)


(* Elementos para las pruebas *)
(* let afd = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c;";; *)
(* let afn = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a;";; *)
(* let afnn = af_of_string "0 1 2 3; a b c; 0; 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a;";; *)
(* let nn = [Terminal "a"; Terminal "b"];; *)
(* let an = [Terminal "a"; Terminal "a"; Terminal "c"];; *)
(* let nd = [Terminal "a"; Terminal "b"; Terminal "a"];; *)
(* let ad = [Terminal "a"; Terminal "b"; Terminal "b"; Terminal "a"; Terminal "c"];; *)

(* funciones auxiliares *)

let avanza simbolo estados (Af (_, _, _, Conjunto arcos, _)) =

   let rec aux destinos = function

        [] -> destinos

      | Arco_af (origen, destino, s) :: t ->
           if (s = simbolo) && (pertenece origen estados) then
              aux (agregar destino destinos) t
           else
              aux destinos t

   in
      aux conjunto_vacio arcos
   ;;
   
let rec avanzaDeterminista (Terminal simbolo) (Estado estado) = function
	| [] -> ((Estado estado), None)
	| Arco_af (Estado est, Estado estRes, Terminal v) :: tl -> if est = estado && v = simbolo then ((Estado estRes), Some v) else avanzaDeterminista (Terminal simbolo) (Estado estado) tl
	| _ :: tl -> avanzaDeterminista (Terminal simbolo) (Estado estado) tl;;
		

(* Funciones del ejercicio *)
let escaner_afn cadena (Af (_, _, inicial, _, finales) as a) = 
	let rec aux = function
	| (Conjunto [], _) -> false
	| (actuales, []) -> not (es_vacio (interseccion actuales finales))
	| (actuales, s::ss) -> aux (avanza s actuales a, ss)
	in aux ((Conjunto [inicial]), cadena);;
	
	
let escaner_afd cadena (Af (_, _, inicial, arcos, finales)) =
	let rec aux fin arcos= function
		| [] -> (pertenece fin finales)
		| s::ss -> let estado, tvalue = (avanzaDeterminista s fin arcos) in 
			match tvalue with
				| Some v -> aux estado arcos ss
				| None -> false 
		in aux inicial (list_of_conjunto arcos) cadena;;

	
