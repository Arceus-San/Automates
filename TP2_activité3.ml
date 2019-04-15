(* Bibliothèque sur les listes et les chaînes de caractères en fin de page *)

							(* **** Piste Rouge **** *)

(* 		Un type pour les AFN 	*)

type etatN = {acceptN : bool ; tN : char -> int list};;

type afn = {sigmaN: char list; (* l'alphabet *)
			nN: int; (* Q est l'ensemble {1..N} *)
			initN: int list; (* les états initiaux *)
			eN : int -> etatN};; 
			
			
(*		Lecture d'un mot par un AFN			*)

let an1  = {sigmaN= ['a';'b'] ; nN = 3; initN = [1] ; 
			eN = function	
			    1 -> {acceptN = false ;
				      tN = function 
					       'a'->[2] }
				|2 -> {acceptN = true ;
				      tN = function 
					       'a'->[2] 
						   |'b'-> [2;3] }		   
				|3 -> {acceptN = false ;
				      tN = function 
					       'a'->[2]
						   |'b'->[3]   }					   
		};;
		
		
(*Automate Non deterministe à 2 états initiaux ( Animation : lecture AFN) *)
		let an2  = {sigmaN= ['a';'b'] ; nN = 4; initN = [1;3] ; 
			eN = function	
			    1 -> {acceptN = false ;
				      tN = function 
					       'a'->[2;3] }
				|2 -> {acceptN = true ;
				      tN = function 
					       'a'->[4]
						   |'b'->[2] }		   
				|3 -> {acceptN = false ;
				      tN = function 
						    'a'->[3]
						   |'b'->[4] }
				|4 -> {acceptN = false ;
				      tN = function 
						    'b'->[1;4] }					   
		};;
		
		(* val an2 : afn = {sigmaN = ['a'; 'b']; nN = 4; initN = [1; 3]; eN = <fun>} *)
(* ******************************************************************* *)	 


an1.initN;;(* - : int list = [1] *)

(an1.eN(2)).tN('b');;
(* - : int list = [2; 3] *)

 (an1.eN(1)).tN('b');;
 Exception: Match_failure ("//toplevel//", 9, 11).
	 
	


(* ******************************************************************* *)
 (* Bibliothèque sur les listes *)  
let rec appartient = function 
(a,b::l)-> if a=b then true else appartient(a,l)
|(_,[])-> false;;
appartient : 'a * 'a list -> bool = <fun>

let rec union l = function 
(a::l2)-> if appartient(a,l) then union l l2 else a:: (union l l2)
| []->l;;
union : 'a list -> 'a list -> 'a list = <fun>

let rec long = function
(_::l)->1+long(l)
|[]-> 0;;

(* Fin de la biliothèque sur les listes *)
(* ******************************************************************* *)
(*				Bibliothèque sur les chaînes de caractères 					 *)


let string_of_char = String.make 1 ;;

let tetec = function
| "" -> failwith "Erreur : chaine vide"
| s -> s.[0] ;;
(*val tetec : string -> char = <fun>*)

let tetes = fun s -> string_of_char (tetec(s));;

let reste = function 
| "" -> failwith "Erreur : chaine vide"
| s -> String.sub s 1  ((String.length s) - 1 ) ;;
(*val reste : string -> string = <fun>*)



(* ******************************************************************* *)
									(* 2. Lecture d’un mot par un AFN *)

let rec conversion = function 
("")->[]
|(m)->m.[0]::conversion(String.sub m 1(String.length m -1) );;
(* val conversion : string -> char list = <fun> *)

conversion("bonjour");; 




let rec accept_mot= function
(a1,a::l1,q1::l2)-> (try  accept_mot(a1,l1,(a1.eN(q1)).tN(a)) or accept_mot(a1,a::l1,l2)   with 
				   Match_failure _ ->  accept_mot(a1,a::l1,l2) ) 
				   
|(a1,a::l1,[])->false

				   
|(a1,[],q::q2::l)-> (a1.eN(q)).acceptN or  accept_mot(a1,[],q2::l)

|(a1,[],[q])-> (a1.eN(q)).acceptN;;

(* val accept_mot : afd * char list * int -> bool = <fun> *)



let accept a1 w = accept_mot(a1,conversion(w),a1.initN);;(*  val accept : afd -> string -> bool = <fun> *)

(* val accept : afn -> string -> bool = <fun> *)



accept an1 "abba" ;;
(* - : bool = true;; *)

accept an1 "bba" ;;
(* - : bool = false *)


accept an2 "abb" ;;
(* - : bool = true;; *)

accept an2 "abab" ;;
(* - : bool = false *)
accept an2 "ababababab" ;;
(* - : bool = true;; *)

