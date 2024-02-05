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

