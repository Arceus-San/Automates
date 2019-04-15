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
let rec longueur = function 
("")->0
|(m)->1+longueur(String.sub m 1(String.length m -1) );;

(* val longueur : string -> int = <fun> *) 

	
longueur "abc";; (*- : int = 3 *)

let autoVide  (sigma:char list) (w:string) = let l = longueur w +1 in  {sigmaN= sigma ; nN = l; initN =[1] ; 
			eN = function
			(1)->{acceptN=false ;tN=function(_)->[1]}
			|(etats)-> if etats=l then 	{acceptN=true ;tN=function(_)-> raise PasTransition}
						else {acceptN=false ;tN=function(_)-> raise PasTransition}

				};;
(* val autoVide : afn -> afn = <fun> *)




let alp=['a';'b';'c'];;

let an1=autoVide alp "abb";;


(an1.eN(1)).acceptN ;;(* - : bool = false *)
(an1.eN(2)).acceptN ;;(*- : bool = false  *)
(an1.eN(3)).acceptN ;;(* - : bool = false *)
(an1.eN(4)).acceptN ;;(* - : bool = true *)

(an1.eN(1)).tN('a') ;;(* - : int list = [1] *)
(an1.eN(1)).tN('b') ;;(* - : int list = [1] *)
(an1.eN(1)).tN('c') ;;(* - : int list = [1] *)
(an1.eN(2)).tN('a') ;;(* Exception: PasTransition. *)
 
let rec creetransition (i:int) = function (* i est l'indice du premier etat *)
("")->[]
|((s:string))-> (i,s.[0],(i+1))::creetransition (i+1) (String.sub s 1(String.length s -1));;


(*val creetransition : int -> string -> (int * char * int) list = <fun> *) 

creetransition 1 "abb";;
(* !!!!!!!!!!!!!! erreur sur rajoute une !!!!!!!!!!!!!!!!!!!*)


let rajouteUne (a: afn) ((q , c , p) : (int * char * int)) = {sigmaN= a.sigmaN;
															nN = a.nN; initN = a.initN;
																eN = function
																	etat -> if etat = q then {acceptN = (let z = a.eN(q) in z.acceptN );
																											tN = (function
																														car -> if car = c then let z = a.eN(q) in ( try let x=z.tN(c) in
																																									[p]@x with _ -> [p])
																																else let z = a.eN(q) in z.tN(car) )}
																			else a.eN(etat) } ;;

(an1.eN(4)).acceptN ;;(* - : bool = true *)
let test= rajouteUne an1 (1,'a',2);;

(test.eN(4)).acceptN ;;

let rec rajoutePlusieurs (an:afn) = function
(((i:int),(lettre:char),(j:int))::liste)-> rajoutePlusieurs (rajouteUne an (i,lettre,j)) liste
|_->an;;


let creeUnAutomate  (sigma:char list) (w:string)  = rajoutePlusieurs (autoVide sigma  w ) (creetransition 1 w) ;;


let anAbb=creeUnAutomate alp "abb";;


(anAbb.eN(1)).tN('a') ;;(* - : int list = [2; 1] *)
(anAbb.eN(1)).tN('b') ;;(* - : int list = [1] *)
(anAbb.eN(1)).tN('c') ;;(* - : int list = [1] *)
(anAbb.eN(2)).tN('a') ;;(* Exception: PasTransition. *) 
(anAbb.eN(2)).tN('b') ;;(*- : int list = [3] *)
(anAbb.eN(3)).tN('b') ;;(* - : int list = [4] *)
(anAbb.eN(4)).tN('b') ;;(* Exception: PasTransition. *)
(anAbb.eN(4)).acceptN ;;(* - : bool = true *)


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



parcours anAbb [1] "abbbbcbab";;  (*- : bool = true*)

parcours anAbb [1] "abcbcbbcbab";; (*- : bool = false*)