type etatN = {acceptN : bool ; tN : char -> int list};;

type afn = {sigmaN: char list; (* l'alphabet *)
			nN: int; (* Q est l'ensemble {1..N} *)
			initN: int list; (* les états initiaux *)
			eN : int -> etatN};; 
			
			
exception  PasTransition ;;

let transitN = fun (aut, i, c) ->
	try (aut.eN(i)).tN(c) 
	with Match_failure _-> raise PasTransition;;
	

	
(*  initialiser le nombre  d'etats ainsi que l'etat acceptant  *)
let autoVide  (sigma:char list) (w:string) ={sigmaN= sigma ; nN = 4; initN =1 ; 
			eN = function(etats)-> 	{accept;tN=function(_)-> raise PasTransition}};;
(* val autoVide : afn -> afn = <fun> *)




['a','b','c']