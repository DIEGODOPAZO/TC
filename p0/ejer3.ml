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
