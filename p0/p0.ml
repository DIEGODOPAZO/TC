(* Ejercicio 1*)
let rec mapdoble f ff lista =

	let rec aux i = function

		| [] -> []

		| h::t ->

			if i mod 2 = 0 then

				(f h) :: aux (i + 1) t

		        else

				(ff h) :: aux (i + 1) t

		  in

		  aux 0 lista

		;; 



(*tipo:  val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>*)



(* mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];; --> ERROR*)

(*let y = function x -> 5 in mapdoble y;; -->   - : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>*)		




(*Ejercicio 2*)
let rec primero_que_cumple f l = 

	let rec aux fa = function

		| [] ->raise(Not_found)

		| h::t ->

			if (fa h) then h else aux fa t

		in aux f l;;

		

(*val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun>*)



let existe predicado lista =

  try

    ignore (primero_que_cumple predicado lista);

    true

  with

  | Not_found -> false

;;


let asociado lista clave =
  let predicado (k, _) = k = clave in
  let (k, v) = primero_que_cumple predicado lista in
  v;;



(* Ejercicio 3*)

type 'a arbol_binario =
  Vacio
  | Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

let rec in_orden = function
  | Vacio -> []
  | Nodo (value, izq, der) -> in_orden(izq) @ [value] @ in_orden(der);;

let rec pre_orden = function
  | Vacio -> []
  | Nodo (value, izq, der) -> [value] @ pre_orden(izq) @ pre_orden(der);;

let rec post_orden = function
  | Vacio -> []
  | Nodo (value, izq, der) -> post_orden(izq) @ post_orden(der) @ [value];; 

let anchura t =
  let rec aux = function
    | [] -> []
    | Vacio :: restante -> aux restante
    | Nodo (valor, izq, der) :: restante -> valor:: (aux (restante @ [izq; der]))
  
in aux [t];;




(*Ejercicio 4*)

type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = Conjunto [];;

let rec pertenece  element (Conjunto conjunto) =
  List.mem element conjunto;;

let agregar element (Conjunto conjunto) =
  if pertenece element  (Conjunto conjunto) then Conjunto conjunto else  Conjunto (element :: conjunto);;

let conjunto_of_list l = 
  List.fold_right (fun element acc -> agregar element acc) l conjunto_vacio;;

let suprimir element (Conjunto conjunto) =  
  Conjunto (List.filter(fun el -> el <> element) conjunto);;

let cardinal (Conjunto conjunto) = 
  List.length conjunto;;

let union (Conjunto conjunto1) (Conjunto conjunto2) =
  conjunto_of_list (conjunto1 @ conjunto2);;

let interseccion (Conjunto conjunto1) (Conjunto conjunto2) = 
  Conjunto (List.filter (fun el -> pertenece el (Conjunto conjunto2)) conjunto1);;

let diferencia (Conjunto conjunto1) (Conjunto conjunto2) =
  Conjunto (List.filter (fun el -> not (pertenece el (Conjunto conjunto2))) conjunto1);;

let incluido (Conjunto conjunto1) (Conjunto conjunto2) =
  List.for_all(fun el -> pertenece el (Conjunto conjunto1)) conjunto2;;

let igual (Conjunto conjunto1) (Conjunto conjunto2) =
  incluido (Conjunto conjunto1) (Conjunto conjunto2) && incluido (Conjunto conjunto2) (Conjunto conjunto1);;

let producto_cartesiano (Conjunto conjunto1) (Conjunto conjunto2) =
  conjunto_of_list (List.concat(List.map (fun ele1-> List.map (fun ele2 -> (ele1, ele2)) conjunto2) conjunto1));;


let list_of_conjunto (Conjunto conjunto) = conjunto;;



