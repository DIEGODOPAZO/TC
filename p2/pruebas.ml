#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

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
cyk [Terminal "b"; Terminal "b"; Terminal "b"; Terminal "b"] klk;;
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
	










