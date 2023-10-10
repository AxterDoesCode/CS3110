(** Exercise: list expressions [*] *)
let _ = [1;2;3;4;5]
let _ = 1 :: 2 :: 3 :: 4 :: 5 :: []
let _ = [1] @ [2;3;4] @ [5]

(** Exercise: product [**] *)
let product t = 
    let rec product_acc acc = function
        | [] -> acc
        | h :: t -> product_acc (acc*h) t
    in product_acc 1 t

(** Exercise: concat [**] *)
let concat t =
    let rec concat_acc acc = function
        | [] -> acc
        | h :: t -> concat_acc (acc ^ h) t    
    in concat_acc "" t

(** Exercise: patterns [***] *)
let first_is_bigred = function
    | "bigred" :: _ -> true
    | _ -> false
let two_or_four = function
    | _ :: _ :: [] -> true
    | _ :: _ :: _ :: _ :: [] -> true
    | _ -> false
let two_equal = function
    | x :: y :: _ -> x = y
    | _ -> false

(** Exercise: library [***] *)
let fifth_ele x = if List.length x >= 5 then List.nth x 4 else 0
let sort_desc lst = List.rev (List.sort (Stdlib.compare) lst)

(** Exercise: library puzzle [***] *)
let last_ele lst = List.nth lst (List.length lst - 1)
let any_zeroes lst = List.exists (fun x -> x = 0) lst 

(** Exercise: take drop [***] *)
let rec take n lst = if n = 0 then []
    else match lst with 
        | [] -> [] 
        | x :: xs -> x :: take (n-1) xs

let rec drop n lst = if n = 0 then lst
    else match lst with
        | [] -> [] 
        | x :: xs -> drop (n-1) xs

(** Exercise: take drop tail [****] *)
let take_tr n lst = 
    let rec take_rec acc n lst = if n = 0 then acc else 
    match lst with
        | [] -> acc
        | x :: xs -> take_rec (x :: acc) (n-1) xs
    in take_rec [] n lst

let drop_tr = drop

(**drop above is inherently tail recursive**)

(** Exercise: unimodal [***] *)
let rec is_mon_dec = function
    | [] | [_] -> true
    | h1 :: h2 :: t -> h1 >= h2 && is_mon_dec t

let rec is_mon_inc_dec = function
    | [] | [_] -> true
    | h1 :: h2 :: t -> if h1 <= h2 then is_mon_inc_dec t else is_mon_dec t

(** Exercise: powerset [***] *) (**Need to read over again later**)
let rec powerset = function
    | [] -> [ [] ]
    | x :: s -> let p = powerset s in
        List.map (List.cons x) p @ p

(** Exercise: print int list rec [**] *)
let rec print_int_list = function
    | [] -> ()
    | x :: xs -> print_endline (string_of_int x); print_int_list xs

(** Exercise: print int list iter [**] *)
let print_int_list' lst =
    List.iter (fun x -> (print_endline (string_of_int x))) lst


