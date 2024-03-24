let rec concat l1 l2 = 
    match l1 with
        [] -> l2
        | x::r -> x::concat r l2;;

let l1 = [0;1;2;3;4;3;5];;
let l2 = [0;0;0;3;3;3;5];;
let listeDeListe = [[0;1];[2];[3;4];[5]];;
let coupleDeListe = ([0;1;2],[3;4;5]);;
let listeDeCouple = [(0,1);(2,3);(4,5)];;

let rec npremiers l n =
    match (l,n) with
    [], _ -> []
    | _, 0 -> []
    | x::r, _ -> x::npremiers r (n-1);;

(*
npremiers l1 3;;*)

let rec met_a_plat ll =
    match ll with
    [] -> []
    | x::r -> concat x (met_a_plat r);;

(*
met_a_plat listeDeListe;;*)

let rec paire_vers_liste couple =
    match couple with
    ([],[]) -> []
    | (_::_,[]) -> failwith "pas la meme taille" 
    | ([], _::_) -> failwith "pas la meme taille" 
    | (x1::r1, x2::r2) -> (x1,x2)::paire_vers_liste (r1,r2);;

(*
paire_vers_liste coupleDeListe;;*)

let rec liste_vers_paire lDeCouple =
    match lDeCouple with
    [] -> [],[]
    | (a,b)::r -> let l1,l2 = liste_vers_paire r in a::l1, b::l2;;

(*
liste_vers_paire listeDeCouple;;*)

let rec supprime1 l x = 
match l with 
    [] -> []
    | x2::r -> if(x=x2) then r else x2::supprime1 r x;;

let rec supprime2 l x = 
match l with 
    [] -> []
    | x2::r -> if(x=x2) then supprime2 r x else x2::supprime2 r x;; 

(*supprime2 l1 3;;*)

let minimum a b = if(a<b) then a else b;;

minimum 5 3;;

let rec min_liste l =
match l with
    [] -> failwith "erreur liste vide"
    | [x] -> x
    | x::r -> minimum x (min_liste r);;

(*
min_liste l1;;*)

let rec compteOcc l x =
    match l with
    [] -> 0
    | y::r -> if(y=x) then 1 + (compteOcc r x) else compteOcc r x;;

let rec doublon l = 
match l with 
    [] -> []
    | [x] -> [x]
    |   x::r -> if (compteOcc l x)>1 then doublon r else x::(doublon r);;

doublon l2;;

let rec inserer_tete a ll =
    match ll with
    [] -> []
    | x::r -> (a::x)::(inserer_tete a r);;

let rec parties l = 
    match l with
    [] -> [[]]
    | x::r -> let parties_r = parties r in
            parties_r@(inserer_tete x parties_r);;

let rec sous_listes n l = 
    match (n,l) with
    (0,_) -> [[]]
    |(_, []) -> []
    | (_, x::r) -> let l1 = inserer_tete x (sous_listes (n-1) r) in
    let l2 = sous_listes n r in l1@l2;;


sous_listes 2 [1;2;3;4];;

(*----------------PARTIE 4----------------*)

let inserer_teteBIS x ll =
    List.map (function l -> x::l) ll;;

(*inserer_teteBIS 5 [[1];[2];[3];[]];;*)

let rec partiesBIS l = 
    match l with
    [] -> [[]]
    | x::r -> let parties_r = partiesBIS r in
            parties_r@(List.map (function l -> x::l) parties_r);;

(*partiesBIS [1;2;3];;*)

(*----------------PARTIE 5----------------*)

let foldLongueur l =
    List.fold_left (fun n x -> n+1) 0 l;;

foldLongueur [1;2;3;4;5];;

let foldConcat l1 l2 =
    List.fold_left (fun n x -> x::n) l2 l1;;

foldConcat [1;2;5] [3;4;5];;

let foldMet_a_Plat ll =
    List.fold_left (fun n x -> foldConcat n x) [] ll;;

foldMet_a_Plat listeDeListe;;

let foldSupprime2 a l = 
    List.fold_left (fun n x -> if x <> a then x::n else n) [] l;;

foldSupprime2 2 [1;2;3;2;3;2];;

let foldMap f l =
    List.fold_right (fun x n -> (f x)::n) l [];;

foldMap (fun x -> x*2) [1;2;3];;

