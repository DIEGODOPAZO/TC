let rec primero_que_cumple f l = 

	let rec aux fa = function

		| [] ->raise(Not_found)

		| h::t ->

			if (fa h) then h else aux fa t

		in aux f l;;

		

(*val primero_que_cumple : ('a list -> bool) -> 'a list list -> 'a list = <fun>*)



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