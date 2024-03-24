type operateur_bin = Mult | Add;;
type operateur_un = Moins;;
type arbreType = Const of int | 
             Var of string | 
             Noeud1 of (operateur_un * arbreType) | 
             Noeud2 of (operateur_bin * arbreType * arbreType);;

let arbre = Noeud2(Mult, Noeud2(Add, Const 1, Var "x"), Noeud2(Add, Const 3, Const 4));;


let rec affOperation arbre =
  match arbre with
  Const x -> string_of_int x
  |Var(x) ->  x
  |Noeud1(Moins, n) -> "(-" ^ (affOperation n) ^ ")"
  |Noeud2(Mult, n1, n2) -> "(" ^ (affOperation n1) ^ "*" ^ (affOperation n2) ^ ")"
  |Noeud2(Add, n1, n2) -> "(" ^ (affOperation n1) ^ "+" ^ (affOperation n2) ^ ")";;

let arb = Noeud2(Add, Const 1, Const 2);;

affOperation arbre;;

let association = [("x", 2); ("y", 3); ("z", 4)];;

let rec appartient a l =
  match l with
  [] -> false
  |((x1,x2)::r) -> if(x1=a) then true else appartient a r;;

let rec estClos arbre l =
  match arbre with
  Const x -> true
  |Var(x) -> appartient x l
  |Noeud1(Moins, n) -> (estClos n l)
  |Noeud2(_, n1, n2) -> (estClos n1 l) && (estClos n2 l);;

estClos arbre association;;

let rec associe a l =
  match l with
  [] -> 0
  |((x1,x2)::r) -> if(x1=a) then x2 else associe a r;;


let rec evalE arbre l = 
  if estClos arbre l then
    match arbre with
    Const x -> x
    |Var x -> (associe x l)
    |Noeud1(Moins, n) -> -(evalE n l)
    |Noeud2(Mult, n1, n2) -> (evalE n1 l) * (evalE n2 l)
    |Noeud2(Add, n1, n2) -> (evalE n1 l) + (evalE n2 l)
  
  else failwith "Arbre pas clos donc pas possible nullos";;

evalE arbre association;;