
let rec longueur l =
    match l with
        [] -> 0
        | x::r -> 1 + longueur r;;

 let liste1 = [0;1;2;3;4;5;6;7;8;9;10];;
 let liste2 = [11;12;13];;

(*
 longueur liste1;;
*)

 let liste1 = [0;1;2;3];;
 let liste2 = [11;12;13];;

(*


 let rec concat l1 l2 = 
    match l2 with
        [] -> l1
        | x::r -> x::concat l1 r;;

concat liste2 liste1;;

*)

let rec nieme l n =
    match (l,n) with
    [],_ -> failwith "erreur, liste trop petite"
    | x::r, 1 -> x
    | x::r, _ -> nieme r (n-1);;

(*
nieme liste1 2;;*)



    
