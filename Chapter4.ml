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
