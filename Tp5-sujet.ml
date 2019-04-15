(*                      Bibliothèque sur les listes                             *)  

let rec appartient = function 
(a,b::l)-> if a=b then true else appartient(a,l)
|(_,[])-> false;;
(*appartient : 'a * 'a list -> bool = <fun>*)

let rec union l = function 
(a::l2)-> if appartient(a,l) then union l l2 else a:: (union l l2)
| []->l;;
(*union : 'a list -> 'a list -> 'a list = <fun>*)

let rec enleve a = function
 x::q -> if x = a then q else x::(enleve a q)
 | [] -> [] ;;

let rec intersection l1 = function
	| [] -> []
	| a :: l2 -> if appartient(a,l1) then a::(intersection (enleve a l1) l2) else intersection l1 l2 ;;

let rec long = function
(_::l)->1+long(l)
|[]-> 0;;						


						(* RAPPELS *)
							
(* Représentation des automates non-déterministes *)
type etatN = {acceptN : bool ; tN : char -> int list};;
		
type afn = {sigmaN: char list; (* l'alphabet *)
			nN: int; (* Q est l'ensemble {1..N} *)
			initN: int list; (* les états initiaux *)
			eN : int -> etatN};;
			

(* Fonction transitN *)
exception  PasTransition ;;

let transitN = fun (aut, i, c) ->
	try (aut.eN(i)).tN(c) 
	with Match_failure _-> raise PasTransition;;
	
(* Automate exemple *)
let an1  = {sigmaN= ['a';'b'] ; nN = 6; initN = [1] ; 
			eN = function	
			    1 -> {acceptN = false ;
				      tN = function 
					       'a'->[3]}
				|2 -> {acceptN = true ;
				      tN = function 
					       'a'->[2] 
						   |'b'-> [1] }		   
				|3 -> {acceptN = true ;
				      tN = function 
					       'a'->[4]
						   |'b'->[5]   }	
				|4 -> {acceptN = true ;
					   tN = function
							'a' -> [3]}
				|5 -> {acceptN = false ;
						tN = function 
							'a' -> [5]
							|'b' -> [6]}
				|6 -> {acceptN = false ;
						tN = function 
							'a' -> [5]
							|'b' -> [6]}
		};;
(an1.eN(2)).tN('a');;

exception PasTransition ;;
		
let transitN = function (aut, i, c) ->
	try (aut.eN(i)).tN(c) 
	with Match_failure _-> raise PasTransition;;
	
(* Alphabet augmenté *)	

let rec  eAccessiblesAux = function 
(an,i,c::lis)-> (try  let tr=transitN(an,i,c) in tr@eAccessiblesAux(an,i,lis) 
				with PasTransition -> eAccessiblesAux(an,i,lis))
|_->[];;

eAccessiblesAux(an1,1,['a';'b']);;
(* val eAccessiblesAux : afn * int * char list -> int list = <fun> *)
				

let sigmaAug = ['a';'b';'é'];;

let eAccessibles (an:afn) (l:int) = let x=eAccessiblesAux(an,l,an.sigmaN) in if appartient(l,x) then x 
									else l::x ;;
	
		
					 
	
(* val eAccessibles : afn -> int -> int list = <fun> *)
	
eAccessibles an1 1;;
(* - : int list = [1; 3] *)
eAccessibles an1 3;;
(* - : int list = [3; 4; 5] *)

let rec eAccessiblesListe (an:afn) (li:int list)  = match li with
(a::liste)-> (union (eAccessibles an a ) (eAccessiblesListe an liste) )
|_->[];;
(* val eAccessiblesListe : afn -> int list -> int list = <fun> *)

eAccessiblesListe an1 [2;6];;

let rec longueur = function
(_::l)->1+longueur(l)
|[]-> 0;;

let rec auxParcours (an:afn) (li:int list)= let lt1=(eAccessiblesListe an li) in 
								if longueur lt1 = longueur li then li 
								else (auxParcours an (union lt1 li) );; 
(* val auxParcours : afn -> int list -> int list = <fun> *)

auxParcours an1 [1];;
(* - : int list = [1; 5; 6; 4; 3] *)
auxParcours an1 [2;3];;
(* - : int list = [2; 1; 6; 3; 4; 5] *)


let etatsAccessibles (an:afn)= auxParcours an an.initN ;;
(* val etatsAccessibles : afn -> int list = <fun> *)

etatsAccessibles an1;;
(* - : int list = [1; 5; 6; 4; 3] *)

(* 2. Construction de l’automate inverse, recherche des sommets co-accessibles *)

let autoVide (an:afn ) ={sigmaN= an.sigmaN ; nN = an.nN; initN = an.initN ; 
			eN = function(etats)-> 	{acceptN=(an.eN(etats)).acceptN;tN=function(_)-> raise PasTransition}};;
(* val autoVide : afn -> afn = <fun> *)
let test1= autoVide an1;;
(* val test1 : afn = {sigmaN = ['a'; 'b']; nN = 6; initN = [1]; eN = <fun>} *)
(test1.eN(1)).tN('a');;

let rajouteUne (an:afn) = function
((i:int),(lettre:char),(j:int))-> {sigmaN= an.sigmaN ; nN = an.nN; initN = an.initN ; 
			eN = function(ori)-> (if  ori = i then	{acceptN=(an.eN(i)).acceptN; tN=function(c)-> (if c= lettre then
							 (try let l= transitN(an,i, lettre) in j::l  with
									PasTransition-> [j])
								else (an.eN(i)).tN(c) ) }
								
								else {acceptN=(an.eN(i)).acceptN; tN=(an.eN(ori)).tN}
								)
				
							};;
			(* val rajouteUne : afn -> int * char * int -> afn = <fun> *)
			
let test2 =rajouteUne test1 (1,'a',3);;
(test2.eN(1)).tN('a') ;;
(test2.eN(2)).tN('a') ;;
(* - : int list = [3]              *)
let test3 = rajouteUne test2 (1,'a',4);;
(test3.eN(1)).tN('a');;
(* - : int list = [4; 3]    *)

let rec rajoutePlusieurs (an:afn) = function
(((i:int),(lettre:char),(j:int))::liste)-> rajoutePlusieurs (rajouteUne an (i,lettre,j)) liste
|_->an;;

(* val rajoutePlusieurs : afn -> (int * char * int) list -> afn = <fun> *)

let test4 = rajoutePlusieurs test1 [(3,'a',1);(4,'a',3);(5,'b',3)];;
(* val test4 : afn = {sigmaN = ['a'; 'b']; nN = 6; initN = [1]; eN = <fun>} *)

(test4.eN(3)).tN('a');; (* - : int list = [1]   *)        
(test4.eN(4)).tN('a');; (* - : int list = [3]     *)           
(test4.eN(1)).tN('a');; (* Exception: PasTransition. *)              
(test4.eN(5)).tN('a');; (* Exception: PasTransition. *)             
(test4.eN(5)).tN('b');; (* - : int list = [3]    *)           

let rec liste_triplet (i:int) (lettre:char) = function
((j:int)::liste)-> (j,lettre,i)::liste_triplet i lettre liste 
|_-> [];;
(* val liste_triplet : int -> char -> int list -> (int * char * int) list =<fun> *)
liste_triplet 1 'a' [2;3];;        
(* - : (int * char * int) list = [(2, 'a', 1); (3, 'a', 1)] *)

let rec resultat an i =function
(a::l) ->( try let res=transitN(an,i, a) in liste_triplet i a res @ resultat an i l with 
			PasTransition -> resultat an i l)
|_->[];;

resultat an1 6 an1.sigmaN;;

let rec listeDeTriplet (an:afn) = function 
(0)->[]
|(i)->resultat an i an.sigmaN @ listeDeTriplet an (i-1);;

listeDeTriplet an1 an1.nN;;
				

let autoInverse (an:afn)= rajoutePlusieurs (autoVide an) ( listeDeTriplet an an.nN );;
 