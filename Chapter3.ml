(** Exercise: list expressions [*] *)
let _ = [1;2;3;4;5]
let _ = 1 :: 2 :: 3 :: 4 :: 5 :: []
let _ = [1] @ [2;3;4] @ [5]

(** Exercise: product [**] *)
let rec product t = 
    let rec product_acc acc = function
        | [] -> acc
        | h :: t -> product_acc (acc*h) t
    in product_acc 1 t


