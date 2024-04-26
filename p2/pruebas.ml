#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;
open Array;;

(* Pruebas de Badillo*)

let klk = Gic
(Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"; No_terminal "C"],
 Conjunto [Terminal "a"; Terminal "b"],
  Conjunto [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
   Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
    Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
    Regla_gic (No_terminal "A", [Terminal "a"]);
    Regla_gic (No_terminal "B", [No_terminal "C"; No_terminal "C"]);
    Regla_gic (No_terminal "B", [Terminal "b"]);
    Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "C", [Terminal "a"])], No_terminal "S");;

let klk2 = Gic
(Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"; No_terminal "C"],
 Conjunto [Terminal "a"; Terminal "b"],
  Conjunto [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
   Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
   Regla_gic (No_terminal "S", [Terminal ""]);
    Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
    Regla_gic (No_terminal "A", [Terminal "a"]);
    Regla_gic (No_terminal "B", [No_terminal "C"; No_terminal "C"]);
    Regla_gic (No_terminal "B", [Terminal "b"]);
    Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "C", [Terminal "a"])], No_terminal "S");;
    
let klk3 = Gic
(Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"; No_terminal "C"],
 Conjunto [Terminal "a"; Terminal "b"],
  Conjunto [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
   Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
   Regla_gic (No_terminal "S", [Terminal ""]);
    Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
    Regla_gic (No_terminal "A", [Terminal ""]);
    Regla_gic (No_terminal "B", [No_terminal "C"; No_terminal "C"]);
    Regla_gic (No_terminal "B", [Terminal "b"]);
    Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "C", [Terminal "a"])], No_terminal "S");;
    
let klk4 = Gic
(Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"; No_terminal "C"],
 Conjunto [Terminal "a"; Terminal "b"],
  Conjunto [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
   Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
    Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
    Regla_gic (No_terminal "A", [Terminal "a"]);
    Regla_gic (No_terminal "B", [No_terminal "C"; Terminal "C"]);
    Regla_gic (No_terminal "B", [Terminal "b"]);
    Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "C", [Terminal "a"])], No_terminal "S");;
    
let klk5 = Gic
(Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"; No_terminal "C"],
 Conjunto [Terminal "a"; Terminal "b"],
  Conjunto [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
   Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
    Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
    Regla_gic (No_terminal "A", [Terminal "a"]);
    Regla_gic (No_terminal "B", [No_terminal "C"; No_terminal "C"]);
    Regla_gic (No_terminal "B", [No_terminal "A"]);
    Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "C", [Terminal "a"])], No_terminal "S");;
    
let klk6 = Gic
(Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"; No_terminal "C"],
 Conjunto [Terminal "a"; Terminal "b"],
  Conjunto [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
   Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
    Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
    Regla_gic (No_terminal "A", [Terminal "a"]);
    Regla_gic (No_terminal "B", [No_terminal "C"; No_terminal "C"]);
    Regla_gic (No_terminal "B", []);
    Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "C", [Terminal "a"])], No_terminal "S");;
    
let klk7 = Gic
(Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"; No_terminal "C"],
 Conjunto [Terminal "a"; Terminal "b"],
  Conjunto [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
   Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
    Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
    Regla_gic (No_terminal "A", [Terminal "a"]);
    Regla_gic (No_terminal "B", [No_terminal "C"; No_terminal "C"; No_terminal "A"]);
    Regla_gic (No_terminal "B", [Terminal "b"]);
    Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "C", [Terminal "a"])], No_terminal "S");;
    
es_fnc klk;;
(*true*)
es_fnc klk2;;
(*true*)
es_fnc klk3;;
(*false*)
es_fnc klk4;;
(*false*)
es_fnc klk5;;
(*false*)
es_fnc klk7;;
(*false*)
es_fnc klk7;;
(*false*)

cyk [Terminal "b"; Terminal "b"; Terminal "a"; Terminal "b"] klk;;
(*true*)
cyk [Terminal "a"; Terminal "a"; Terminal "a"] klk;;
(*true*)
cyk [Terminal "b"; Terminal "a"] klk;;
(*true*)
cyk [Terminal "a"; Terminal "b"; Terminal "a"; Terminal "b"; Terminal "a"] klk;;
(*true*)
cyk [Terminal "b"; Terminal "b"; Terminal "b"; Terminal "b"] klk;;
(*false*)


(* Ejercicio 1 funciones auxiliares*)
let areTerminal lr= 
	let rec aux = function
		| Terminal _  :: tl-> true
		| No_terminal _ :: tl-> aux tl
		| [] -> false
	in aux lr;;

let isNotTerminal nt lr s = 
	let rec aux = function
		| No_terminal _ :: tl -> true
		| Terminal "" :: tl -> if (No_terminal nt) = s then aux tl else true
		| Terminal _ :: tl -> aux tl
		| [] -> false
	in aux lr;; 

let not_fnc nt lr s=
	let len = (List.length lr) in if len = 2 then areTerminal lr else ( if len = 1 then isNotTerminal nt lr s else true);; 

(* Ejercicio 1 *)

let es_fnc (Gic (n, t, p, s) as gic) =
	let rec aux = function 
		| (Regla_gic (No_terminal nt, lr))::tl -> if not_fnc nt lr s then false else aux tl
		| [] -> true
		| _ -> false 
	in aux (list_of_conjunto p);;
	
	
	
(* Ejercicio 2 funciones auxiliares*)
let crear_matriz_de_listas n =
  Array.init n (fun _ -> Array.init n (fun _ -> []));;
  
  
	
let getNotTerminals s (Gic (_, _, p, _)) = 
  let rec aux res = function
    | [] -> (list_of_conjunto res)
    | Regla_gic (nt, symbols)::tl -> 
        if List.mem s symbols then
          aux (agregar nt res) tl
        else
          aux res tl
  in aux conjunto_vacio (list_of_conjunto p);;
	

let initialiceFirstRow matrix simbolos (Gic (n, t, p, s) as gic) = 
	let rec aux index = function
		| [] -> None
		| hd :: tl -> let res = getNotTerminals hd gic in 
								let rec add = function
									|[] -> None
									|hdd :: tll -> matrix.(1).(index) <- hdd::matrix.(1).(index); add tll
								in add res; 
								aux (index + 1) tl;
		in aux 1 simbolos;;  


let producto_cartesiano lista1 lista2 =
  let resultado = ref [] in
  for i = 0 to List.length lista1 - 1 do
    for j = 0 to List.length lista2 - 1 do
      resultado := (List.nth lista1 i, List.nth lista2 j) :: !resultado
    done;
  done;
  !resultado

let getNotTerminalNt (t, tt) p =
	let rec aux res= function
		|[] -> res
		|Regla_gic (nt, [No_terminal x; No_terminal y])::tl -> if ( (No_terminal x) = t) && ( (No_terminal y) = tt) then aux (nt::res) tl else aux res tl
		| _ :: tl -> aux res tl
	in aux [] (list_of_conjunto p);;


let getAFromBC (Gic (n, t, p, s)) b c= 
	let rec aux res = function
		|[] -> (list_of_conjunto res)
		|h::tl -> let nres = (union res (conjunto_of_list (getNotTerminalNt h p))) in aux nres tl	
	in aux conjunto_vacio (producto_cartesiano b c);;
		

(* Ejercicio 2 *)

let cyk simbolos (Gic (n, t, p, s) as gic) = let len = (List.length simbolos) in
	if (not (es_fnc gic)) || (len = 0) then raise (Failure "Invalid GIC") else (
		let matrix = crear_matriz_de_listas (len + 1) in let x = initialiceFirstRow matrix simbolos gic in
			for j = 2 to (len) do
				for i = 1 to (len - j + 1) do
					for k = 1 to (j - 1) do
						let values = (getAFromBC gic matrix.(k).(i) matrix.((j - k)).((i + k))) in
							let rec app = function
								| [] -> ()
								| hd :: tl -> matrix.(j).(i) <- hd::matrix.(j).(i); app tl 
							in app values;
					done;
				done;
			done;
			print_matrix matrix;
			let rec aux = function
				|[] -> false
				| (No_terminal hd)::tl -> print_endline ("hd: " ^ hd); if (No_terminal hd) = s then true else aux tl
			in aux matrix.(len).((1))
	) ;;


let print_matrix matrix =
  Array.iter (fun row ->
    Array.iter (fun elem ->
      List.iter (fun symbol ->
        match symbol with
        | Terminal s -> print_string ("Terminal " ^ s ^ " ")
        | No_terminal s -> print_string ("No_terminal " ^ s ^ " ")
      ) elem;
      print_string " | "   (* Separador entre elementos de la fila *)
    ) row;
    print_endline ""   (* Nueva línea al final de cada fila *)
  ) matrix;;

let print_symbol_list symbol_list =
  List.iter (fun symbol ->
    match symbol with
    | Terminal s -> print_string ("Terminal " ^ s ^ " ")
    | No_terminal s -> print_string ("No_terminal " ^ s ^ " ")
  ) symbol_list;
  print_endline "";;
  
  
