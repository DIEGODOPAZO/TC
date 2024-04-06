#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;


(* Ejercicio 1 *)

(* Elementos para las pruebas *)

(* let afne = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;";; 
let afd = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 0 2 b; 0 3 c; 1 3 c; 1 1 b; 1 2 a; 2 3 c; 2 1 a; 2 2 b; 3 3 a; 3 3 b; 3 3 c;";; 
let afnnn = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 0 2 b; 0 3 c; 1 3 c; 1 1 b; 1 2 a; 2 3 c; 2 1 a; 2 2 b;";;
let afnn = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 0 2 b; 0 3 c; 1 3 c; 1 1 b; 1 2 a; 2 3 c; 2 1 a;";; 
let afn = af_of_string "0 1 2 3; a b c; 0; 1 3;
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

(* Elementos para probar*)

(* Los tres primeros son equivalentes entre ellos, los dos ultimos no son equivalentes a ninguno*)

(*
let eq_afne = Af (
    Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"; Estado "4"],

    Conjunto [Terminal "a"; Terminal "b"],

    Estado "0",

    Conjunto [
      Arco_af (Estado "0", Estado "3", Terminal "a");
      Arco_af (Estado "0", Estado "1", Terminal "");
      Arco_af (Estado "1", Estado "2", Terminal "");
      Arco_af (Estado "1", Estado "2", Terminal "a");
      Arco_af (Estado "3", Estado "4", Terminal "b");
      Arco_af (Estado "4", Estado "0", Terminal "b");
      Arco_af (Estado "4", Estado "1", Terminal "")
    ],

    Conjunto [Estado "2"]
  );;

let eq_afn = Af (
    Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"; Estado "4"],

    Conjunto [Terminal "a"; Terminal "b"],

    Estado "0",

    Conjunto [
      Arco_af (Estado "0", Estado "3", Terminal "a");
      Arco_af (Estado "0", Estado "2", Terminal "a");
      Arco_af (Estado "1", Estado "2", Terminal "a");
      Arco_af (Estado "3", Estado "1", Terminal "b");
      Arco_af (Estado "3", Estado "2", Terminal "b");
      Arco_af (Estado "3", Estado "4", Terminal "b");
      Arco_af (Estado "4", Estado "1", Terminal "b");
      Arco_af (Estado "4", Estado "2", Terminal "a");
      Arco_af (Estado "4", Estado "2", Terminal "b");
      Arco_af (Estado "4", Estado "0", Terminal "b");
      ],

    Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "4";]
  );;


let eq_afd = Af (
    Conjunto [Estado "0"; Estado "23"; Estado "124"; Estado "2"; Estado "012"],

    Conjunto [Terminal "a"; Terminal "b"],

    Estado "0",

    Conjunto [
      Arco_af (Estado "0", Estado "23", Terminal "a");
      Arco_af (Estado "23", Estado "124", Terminal "b");
      Arco_af (Estado "124", Estado "2", Terminal "a");
      Arco_af (Estado "124", Estado "012", Terminal "b");
      Arco_af (Estado "012", Estado "23", Terminal "a");
      ],

    Conjunto [Estado "0"; Estado "23"; Estado "124"; Estado "2"; Estado "012"]
  );;
  
let neq_afd = Af (
    Conjunto [Estado "0"; Estado "23"; Estado "124"; Estado "2"; Estado "012"; Estado "fin"],

    Conjunto [Terminal "a"; Terminal "b"],

    Estado "0",

    Conjunto [
      Arco_af (Estado "0", Estado "23", Terminal "a");
      Arco_af (Estado "23", Estado "fin", Terminal "b");
      Arco_af (Estado "124", Estado "2", Terminal "a");
      Arco_af (Estado "124", Estado "012", Terminal "b");
      Arco_af (Estado "012", Estado "23", Terminal "a");
      ],

    Conjunto [Estado "0"; Estado "23"; Estado "124"; Estado "2"; Estado "012"]
  );;
 let neq = Af (
    Conjunto [Estado "0"; Estado "23"; Estado "124"; Estado "2"; Estado "012"; Estado "fin"],

    Conjunto [Terminal "a"; Terminal "b"],

    Estado "0",

    Conjunto [
      Arco_af (Estado "0", Estado "23", Terminal "a");
      Arco_af (Estado "23", Estado "fin", Terminal "b");
      Arco_af (Estado "012", Estado "23", Terminal "a");
      ],

    Conjunto [Estado "0"; Estado "23"; Estado "124"; Estado "2"; Estado "012"]
  );;
*)


(* Funciones auxiliares *)

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
  

let agregarV estados destinos = 
	let aux = function
		| [] -> destinos
		| es :: tl -> agregar es destinos
	in aux (list_of_conjunto estados);;
	
let sameFinals est1 est2 fin1 fin2 = 
	let tieneFinales1 = ((cardinal (interseccion est1 fin1)) > 0) in let tieneFinales2 = ((cardinal (interseccion est2 fin2)) > 0) in
	if tieneFinales1 && tieneFinales2 then true
		else if not tieneFinales1 && not tieneFinales2 then true
  		else false;;
 
let get_E_transiciones (Af (_, alfabeto, inicial, arcos, finales)) estado= 
	let arc = list_of_conjunto arcos in
	let rec aux iniciales = function
		| [] -> iniciales
		| Arco_af (origen, destino, Terminal "") :: tl -> if (pertenece origen iniciales) && not (pertenece destino iniciales) 
				then aux (agregar destino iniciales) arc else aux iniciales tl
		| _ :: tl -> aux iniciales tl
		in aux estado arc;; 

let allVisited est1 est2 vis1 vis2 = 
	if (incluido est1 vis1) && (incluido est2 vis2) then true else false ;;	
(* Funcion del ejercicio*)

let equivalentes (Af (estados1, alfabeto1, inicial1, arcos1, finales1) as a) (Af (estados2, alfabeto2, inicial2, arcos2, finales2) as b) = 
	let alf = list_of_conjunto (union alfabeto1 alfabeto2) in
	let rec aux vis1 vis2 estt1 estt2 level alf = function
		| [] -> true
		| s::tl -> let est1 = get_E_transiciones a estt1 in let est2 = get_E_transiciones b estt2 in
			 if sameFinals est1 est2 finales1 finales2 then (
				if ((not (allVisited est1 est2 vis1 vis2)) && ((level < (cardinal arcos1)) && (level < (cardinal arcos2)))) then (
					if (aux (agregarV est1 vis1) (agregarV est2 vis2) (avanza s est1 a) (avanza s est2 b) (level + 1) alf alf)
					then (aux conjunto_vacio conjunto_vacio estt1 estt2 level alf tl) else false )else (
					true ) )			 
			 else (false)
	in aux (conjunto_of_list []) (conjunto_of_list []) (conjunto_of_list [inicial1]) (conjunto_of_list [inicial2]) 0 alf alf;;
		
(*-----------------------------------------------------------------------------------------------*)

(* Ejercicio 3 *)


(* Elementos para las pruebas *)
(* let afd = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c;";; 
let afn = af_of_string "0 1 2 3; a b c ws; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a; 0 3 ws;";; 
 let afn2 = af_of_string "0 1 2 3 4; a b c ws; 0; 4;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a; 0 4 ws;";; 
 let afnn = af_of_string "0 1 2 3; a b c; 0; 3;
0 1 a; 1 1 b; 1 2 a; 2 3 c; 0 0 a;";; 
let nn = [Terminal "a"; Terminal "b"];; 
let an = [Terminal "a"; Terminal "a"; Terminal "c"];; 
let an = [Terminal "a"; Terminal "a"; Terminal "c"];; 
let nd = [Terminal "a"; Terminal "b"; Terminal "a"];;
let ad = [Terminal "a"; Terminal "b"; Terminal "b"; Terminal "a"; Terminal "c"];; *)

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

	
