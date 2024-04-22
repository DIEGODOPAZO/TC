#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(* Pruebas de Badillo*)

let klk = Gic
 (Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"; No_terminal "C"],
  Conjunto [Terminal "a"; Terminal "b"],
  Conjunto
   [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
    Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
    Regla_gic (No_terminal "A", [Terminal "a"]);
    Regla_gic (No_terminal "B", [No_terminal "C"; No_terminal "C"]);
    Regla_gic (No_terminal "B", [Terminal "b"]);
    Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
    Regla_gic (No_terminal "C", [Terminal "a"])],
  No_terminal "S");;
  
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




let es_fnc gic =
	










