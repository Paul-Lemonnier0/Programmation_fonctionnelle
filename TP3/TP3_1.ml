type 'a abrBin =  F of 'a | Noeud of ('a abrBin * 'a abrBin);; 

Noeud(Noeud(F(1), F(2)), Noeud(F(3), F(4)));;

let rec nbNoeuds n =
  match n with
  F(_) -> 0
  | Noeud(n1,n2) -> 1 + nbNoeuds n1 + nbNoeuds n2;;

let arbre = Noeud(Noeud(F(1), F(2)), Noeud(Noeud(F(4), F(5)), F(3)));;
let arbre2 = Noeud(Noeud(F(1), F(2)), Noeud(F(2), F(3)));;

nbNoeuds arbre;;

let rec nbFeuilles n =
  match n with
  F(_) -> 1
  | Noeud(n1,n2) -> nbFeuilles n1 + nbFeuilles n2;;

nbFeuilles arbre;;

let max a b =
  if(a>b) then a else b;;

let rec profondeur n =
  match n with
  F(_) -> 0;
  | Noeud(n1,n2) -> max (1 + (profondeur n1)) (1 + (profondeur n2));;

profondeur arbre;;

let rec memeForme a1 a2 =
  match (a1,a2) with
  (Noeud(sag1, sad1), F(_)) -> false
  |(F(_), Noeud(sag2, sad2)) -> false
  |(F(_), F(_)) -> true
  |(Noeud(sag1, sad1), Noeud(sag2, sad2)) -> (memeForme sag1 sag2) && (memeForme sad1 sad2);;

memeForme arbre arbre2;;

let rec listeValeur arbre =
  match arbre with
  F(x) -> [x]
  | Noeud(a1,a2) -> (listeValeur a1)@(listeValeur a2);;

listeValeur arbre;;

let rec mapArbre f arbre =
  match arbre with
  F(x) -> F(f x)
  | Noeud(a1,a2) -> Noeud(mapArbre f a1, mapArbre f a2);;

let increment x = x*5;;

mapArbre increment arbre;;