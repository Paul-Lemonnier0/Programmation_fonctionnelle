type operateur = Mult | Plus | Moins;;
type arbreType = C of int | N of (operateur * arbreType list);;

let arbre = N(Plus, [C 1; N(Mult,[C 5; C 2])]);;

let rec nbConstantes arbre =
  match arbre with
  C y -> 1
  |N(_, aList) -> List.fold_left (fun n x -> n + (nbConstantes x)) 0 aList;;

nbConstantes arbre;;