(** Exercise: twice no arguments [*] *)
let double x = 2*x
let square x = x*x
let twice f x = f (f x)
let quad = twice double (* This is a function because twice is a higher order function *)
let fourth = twice square 

(** Exercise: mystery operator 1 [*] *)
(* let ( $ ) f x = f x This operator is just function application, applying its first argument to its second*)

(** Exercise: mystery operator 2 [*] *)
(* let ( @@ ) f g x = x |> g |> f This operator simply performs function composition*)

(** Exercise: repeat [**] *)
let rec repeat f n x = match n with
    | 0 -> x
    | _ -> repeat f (n-1) (f x)

let rec repeat' f n x = 
    if n = 0 then x else repeat' f (n - 1) (f x)

(** Exercise: product [*] *)
let product_left = List.fold_left ( *. ) 1.
let product_right lst = List.fold_right ( *. ) lst 1.

(** Exercise: terse product [**] *)
(*above solution for product_left is already the most terse*)
let product_right' = ListLabels.fold_right ~f:( *. ) ~init:1.

(** Exercise: sum cube odd [**] *)
(** Exercise: sum cube odd pipeline [**] *)
let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j
let odd x = x mod 2 = 1
let cube x = x * x * x
let sum_cube_odd n =
    0 -- n
    |> List.filter odd 
    |> List.map cube
    |> List.fold_left ( + ) 0

(** Exercise: exists [**] *)
let rec exists_rec p = function
    | [] -> false
    | h :: t -> p h || exists_rec p t

let exists_fold p lst = List.fold_left (fun acc elt -> acc || p elt) false lst

let exists_lib = List.exists

(** Exercise: account balance [***] *)
let account_balance_fold_l balance = List.fold_left (fun acc el -> acc - el) balance

let account_balance_fold_r balance = ListLabels.fold_right ~f:(fun el acc -> acc - el) ~init:balance

let rec account_balance_rec balance = function
    | [] -> balance
    | h :: t -> account_balance_rec (balance - h) t

(** Exercise: library uncurried [**] *)
let uncurried_list_append (a, b) = List.append a b
let uncurried_char_compare (a, b) = Char.compare a b
let uncurried_max (a, b) = Stdlib.max a b

(** Exercise: map composition [***] *)
(*
  List.map f (List.map g lst)
  = List.map (fun x -> f(g x)) lst
*)

(** Exercise: more list fun [***] *)
let filter3 = List.filter (fun x -> x > 3)
let add1 = List.map (fun el -> el +. 1.)
let join sep = List.fold_left (fun acc el -> if acc = "" then el else acc ^ sep ^ el) ""
let join' sep = function
    | [] -> ""
    | x :: xs -> 
        List.fold_left (fun combined s -> combined ^ sep ^ s) x xs

(** Exercise: association list keys [***] *)
let keys lst = 
    lst
    |> List.rev_map fst
    |> List.sort_uniq Stdlib.compare

(** Exercise: valid matrix [***] *)
let valid_matrix = function
    | [] -> false
    | r :: rows ->
        let m = List.length r in
        m > 0 && List.for_all (fun r' -> List.length r' = m) rows

(** Exercise: row vector add [***] *)
let vector_add = List.map2 ( + )

(** Exercise: matrix add [***] *)
let matrix_add = List.map2 vector_add
