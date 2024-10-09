let rec lang1 l1 = match l1 with
[] -> false
|[a] -> if(a = 0 || a = 1) then true else false
|a::tl -> if(a = 0 || a = 1) then lang1 tl else false

let rec lang2 l2 = match l2 with
[] -> true
|['1'] -> lang2 tl
|['0'] -> List.for_all(fun x -> x <> '0') tl
|_ -> false

let lang3 _ = failwith ""

let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
