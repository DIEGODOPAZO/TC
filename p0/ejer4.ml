type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = Conjunto [];;

let rec pertenece  element (Conjunto conjunto) =
  List.mem element conjunto;;

let agregar element (Conjunto conjunto) =
  if pertenece element  (Conjunto conjunto) then Conjunto conjunto else  Conjunto (element :: conjunto);;

let conjunto_of_list l = 
  List.fold_right (fun element acc -> agregar element acc) l conjunto_vacio;;

