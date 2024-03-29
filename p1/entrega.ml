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
0 1 a; 0 2 b; 0 3 c; 1 3 c; 1 1 b; 1 2 a; 2 3 c; 2 1 a; 2 2 b; 3 3 a; 3 3 b; 3 3 c;";; *)
(* let afnnn = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 0 2 b; 0 3 c; 1 3 c; 1 1 b; 1 2 a; 2 3 c; 2 1 a; 2 2 b;";; *)
(* let afnn = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 0 2 b; 0 3 c; 1 3 c; 1 1 b; 1 2 a; 2 3 c; 2 1 a;";; *)
(* let afn = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a;";; *)

(* funciones auxiliares *)
let rec e_transicion= function 
  | [] -> false
  | Arco_af (_, _, Terminal "") :: _ -> true
  | _ :: resto ->  e_transicion resto;;


let rec indeterminismosE estado valor res valoresE valoresT estV= function
	| [] ->  if (igual valoresE valoresT) then false else (if (pertenece estado estV) then false else true)
	| Arco_af (_, _, Terminal "") :: tl -> indeterminismosE estado valor res valoresE valoresT estV tl (* or false si no se quiere q tenga e arcos*)
	| Arco_af (Estado est, Estado estRes, Terminal v) :: tl -> if est = estado && v = valor && estRes != res then true else (if est = estado then indeterminismosE estado valor res (agregar (Terminal v) valoresE) valoresT estV tl else indeterminismosE estado valor res valoresE valoresT estV tl)
	| _ :: tl -> indeterminismosE estado valor res valoresE valoresT estV tl;;


let rec indeterminismosL estadosT valoresT estV= function
	| [] -> not (igual estadosT estV)
	| Arco_af (Estado estado, Estado res, Terminal valor) :: tl -> if indeterminismosE estado valor res (conjunto_of_list [Terminal valor]) valoresT estV tl then true else indeterminismosL estadosT valoresT (agregar estado estV) tl
	| _ :: tl -> indeterminismosL estadosT valoresT estV tl;;

let estado_to_string (Estado s) = s;;
let rec estados_to_strings res = function
  | [] -> res
  | h::t -> estados_to_strings (agregar (estado_to_string h) res) t;;
(* Funciones ejercicio *)

let es_afne (Af (_, _, _, arcos, _)) =
  e_transicion (list_of_conjunto arcos);;
  
let es_afn af =
	match af with 
	| Af (estadosT, valoresT, _, arcos, _) -> indeterminismosL (estados_to_strings (conjunto_of_list []) (list_of_conjunto estadosT)) valoresT (conjunto_of_list []) (list_of_conjunto arcos);;
	
let es_afd af = 
	not (es_afn af) && not (es_afne af);;
	

(*-----------------------------------------------------------------------------------------------*)

(* Ejercicio 2 *)


(*-----------------------------------------------------------------------------------------------*)

(* Ejercicio 3 *)


(* Elementos para las pruebas *)
(* let afd = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c;";; *)
(* let afn = af_of_string "0 1 2 3; a b c ws; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a; 0 3 ws;";; *)
(* let afn2 = af_of_string "0 1 2 3 4; a b c ws; 0; 4;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a; 0 4 ws;";; *)
(* let afnn = af_of_string "0 1 2 3; a b c; 0; 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a;";; *)
(* let nn = [Terminal "a"; Terminal "b"];; *)
(* let an = [Terminal "a"; Terminal "a"; Terminal "c"];; *)
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

	
