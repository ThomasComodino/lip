let rec lang1 l1 = match l1 with
[] -> false
|[a] -> if(a = 0 || a = 1) then true else false
|a::tl -> if(a = 0 || a = 1) then lang1 tl else false

let rec lang2 l2 = match l2 with
[] -> true
|['1'] -> lang2 tl
|['0'] -> List.for_all(fun x -> x <> '0') tl
|_ -> false

let rec aux  l = match l with
[] -> false
|['0'] -> true
|_::tl -> aux tl


let rec lang3 l3 = match l3 with
[] -> false
|0::tl -> if(aux tl) then List.for_all(fun x -> x = 1 || x = 0) tl else false 
|_ -> false

let lang4 l4 = ((List.fold_left(fun aux2 x -> if(x = 1) then aux2 +1 else aux2) 0 l4 ) = 2)
 && (List.for_all(fun x -> x = 1 || x = 0 ) l4)

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
