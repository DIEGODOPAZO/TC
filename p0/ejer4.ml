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
