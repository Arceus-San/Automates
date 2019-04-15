(* Bibliothèque sur les listes et les chaînes de caractères en fin de page *)

(* 		Un type pour les AFN 	*)

type etatN = {acceptN : bool ; tN : char -> int list};;

type afn = {sigmaN: char list; (* l'alphabet *)
			nN: int; (* Q est l'ensemble {1..N} *)
			initN: int list; (* les états initiaux *)
			eN : int -> etatN};; 
			
			
(*		Lecture d'un mot par un AFN			*)

exception PasTransition;;
		
let test  = {sigmaN= ['a';'b';'c'] ; nN = 4; initN = [1] ; 
			eN = function	
			    1 -> {acceptN = false ;
				      tN = function 
					       'a'->[1;2]
						   |'b'->[1]
						   |'c'->[1]
							}
				|2 -> {acceptN = false ;
				      tN = function 
					       'b'->[3] }		   
				|3 -> {acceptN = false ;
				      tN = function 
						   'b'->[4]   }
				|4 -> {acceptN = true ;
				      tN = function
						   _->raise PasTransition   }
		};;


(* ******************************************************************* *)
(*				Bibliothèque sur les chaînes de caractères 					 *)


let string_of_char = String.make 1 ;;

let tetec = function
| "" -> failwith "Erreur : chaine vide"
| s -> s.[0] ;;
(* val tetec : string -> char = <fun> *)

let tetes = fun s -> string_of_char (tetec(s));;

let reste = function 
| "" -> failwith "Erreur : chaine vide"
| s -> String.sub s 1  ((String.length s) - 1 ) ;;
(* val reste : string -> string = <fun> *)


(an1.eN(2)).tN 'b';;
(* - : int list = [2; 3] *)

(* Fait passer une liste d'état à l'état suivant en fonction d'une lettre et d'un afn *)

let rec change_etat = function
	(x::l,a,c)-> (try ((a.eN(x)).tN c) with PasTransition ->[])@change_etat(l,a,c)
	|([],_,_)->[];;
(* val change_etat : int list * afn * char -> int list = <fun> *)
	
change_etat([1;2;3],an1,'a');;
(* - : int list = [2; 2; 2] *)
change_etat([1;2;3],an1,'b');;
(* - : int list = [2; 3; 3] *)

(an1.eN(2)).acceptN;;
(* - : bool = true *)

(* Vérifie si un état acceptant est présent dans la liste *)

let rec est_accept = function
	(a::l,af)->(af.eN(a)).acceptN || est_accept(l,af)
	|([],_)->false;;
(* val est_accept : int list * afn -> bool = <fun> *)

est_accept([1;2;3],an1);;
(* - : bool = true *)


let rec parcours (an:afn) (etats:int list) (c:string) = match c with
	""->false
	|m->let new_etats=change_etat(etats,an,tetec(m)) in
			if(est_accept(new_etats,an)) then true 
			else (parcours an new_etats (reste(m)));;
	
parcours test [1] "abbcbab";;

(* parcours <-- {sigmaN = ['a'; 'b'; 'c']; nN = 4; initN = [1]; eN = <fun>}
parcours --> <fun>
parcours* <-- [1]
parcours* --> <fun>
parcours** <-- "abbcbab"
parcours <-- {sigmaN = ['a'; 'b'; 'c']; nN = 4; initN = [1]; eN = <fun>}
parcours --> <fun>
parcours* <-- [1; 2]
parcours* --> <fun>
parcours** <-- "bbcbab"
parcours <-- {sigmaN = ['a'; 'b'; 'c']; nN = 4; initN = [1]; eN = <fun>}
parcours --> <fun>
parcours* <-- [1; 3]
parcours* --> <fun>
parcours** <-- "bcbab"
parcours** --> true 	(lecture a)
parcours** --> true 	(lecture b)
parcours** --> true 	(lecture b)
- : bool = true *)

parcours test [1] "ccabbcb";;


(* parcours <-- {sigmaN = ['a'; 'b'; 'c']; nN = 4; initN = [1]; eN = <fun>}
parcours --> <fun>
parcours* <-- [1]
parcours* --> <fun>
parcours** <-- "ccabbcb"
parcours <-- {sigmaN = ['a'; 'b'; 'c']; nN = 4; initN = [1]; eN = <fun>}
parcours --> <fun>
parcours* <-- [1]
parcours* --> <fun>
parcours** <-- "cabbcb"
parcours <-- {sigmaN = ['a'; 'b'; 'c']; nN = 4; initN = [1]; eN = <fun>}
parcours --> <fun>
parcours* <-- [1]
parcours* --> <fun>
parcours** <-- "abbcb"
parcours <-- {sigmaN = ['a'; 'b'; 'c']; nN = 4; initN = [1]; eN = <fun>}
parcours --> <fun>
parcours* <-- [1; 2]
parcours* --> <fun>
parcours** <-- "bbcb"
parcours <-- {sigmaN = ['a'; 'b'; 'c']; nN = 4; initN = [1]; eN = <fun>}
parcours --> <fun>
parcours* <-- [1; 3]
parcours* --> <fun>
parcours** <-- "bcb"
parcours** --> true 	(lecture c)
parcours** --> true 	(lecture c)
parcours** --> true 	(lecture a)
parcours** --> true 	(lecture b)
parcours** --> true 	(lecture b)
- : bool = true *)

