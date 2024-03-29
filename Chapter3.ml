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


(** Exercise: student [**] *)
type student = {first_name : string; last_name : string; gpa : float}
let _ = {first_name = "Alex"; last_name = "Chau"; gpa = 4.;}
let student_name = function
    | {first_name;last_name;_} -> (first_name, last_name)
let new_student fn ln gpa = {first_name = fn; last_name = ln; gpa}

(** Exercise: pokerecord [**] *)
type poketype = Normal | Fire | Water
type pokemon = {name : string; hp : int; ptype: poketype}
let p1 = {name = "Charizard"; hp = 78; ptype = Fire}
let p2 = {name = "Squirtle"; hp = 44; ptype = Water}

(** Exercise: safe hd and tl [**] *)
let safe_hd = function
    | [] -> None
    | x :: xs -> Some x

let safe_tl = function
    | [] -> None
    | x :: xs -> Some xs

(** Exercise: pokefun [***] *)
let p3 = {name = "Groudon"; hp = 100; ptype = Fire}
let rec max_hp = function
    | [] -> None
    | x :: xs -> begin
        match max_hp xs with
            | None -> Some x
            | Some poke -> Some (if x.hp >= poke.hp then x else poke)
    end
(*Tail Recursive implementation cos why not*)
let max_hp' = function
    | [] -> None
    | poke1 :: pt -> begin
        let rec max_hp_tr acc lst = match lst with
            | [] -> Some acc
            | x :: xs -> max_hp_tr (if acc.hp >= x.hp then acc else x) xs
        in max_hp_tr poke1 pt
    end

(** Exercise: date before [**] *)
type date = int * int * int
let is_before date1 date2 = 
    let (y1, m1, d1) = date1 in
    let (y2, m2, d2) = date2 in
    y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

(** Exercise: earliest date [***] *)
let rec earliest = function
    | [] -> None
    | d1 :: xs -> begin
        match earliest xs with
            | None -> Some d1
            | Some d2 -> Some (if is_before d1 d2 then d1 else d2)
    end

(*Tail recursive implementation of earliest*)
let earliest' = function
    | [] -> None
    | d1 :: xs -> begin
        let rec earliest_tr acc lst = match lst with
            | [] -> Some acc
            | d2 :: ds -> earliest_tr (if is_before acc d2 then acc else d2) ds
        in earliest_tr d1 xs
    end

(** Exercise: assoc list [*] *)

let insert k v lst = (k, v) :: lst

let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t

(** Exercise: cards [**] *)
type suit = Club | Heart | Spade | Diamond
type card = {suit : suit ; rank : int}
let _ = {suit = Club ; rank = 1}
let _ = {suit = Heart ; rank = 12}
let _ = {suit = Diamond ; rank = 2}
let _ = {suit = Spade ; rank = 7}

(** Exercise: matching [*] *)

(*Some x :: tl*)
let lst1 : int option list = [None; Some 0]

(*[Some 3110; None]*)
let lst2 : int option list = [Some 123; None]

(*[Some x; _]*)
let lst3 : int option list = [None; Some 0]

(*h1 :: h2 :: tl*)
let lst4 : int option list = [None]

(*h :: tl*)
(*Impossible this binds to everything that is non zero length you can only not match it with the empty list []*)

(** Exercise: quadrant [**] *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign =
    if x < 0 then Neg else
    if x > 0 then Pos else Zero

let quadrant : int*int -> quad option = fun (x,y) ->
  match (sign x, sign y) with
    | (Pos, Pos) -> Some I
    | (Neg, Pos) -> Some II
    | (Neg, Neg) -> Some III
    | (Pos, Neg) -> Some IV
    |  _ -> None

(** Exercise: quadrant when [**] *)
let quadrant_when : int*int -> quad option = function
    | x,y when x > 0 && y > 0 -> Some I
    | x,y when x < 0 && y > 0 -> Some II
    | x,y when x > 0 && y < 0 -> Some III
    | x,y when x < 0 && y < 0 -> Some IV
    | _ -> None

(** Exercise: depth [**] *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
let rec depth = function
    | Leaf -> 0
    | Node (_, left, right) -> 1 + max (depth left) (depth right)

(** Exercise: shape [***] *)
let rec shape t1 t2 = match (t1, t2) with 
    | (Leaf, Leaf) -> true
    | (Node (_, l1, r1), Node (_, l2, r2)) -> shape l1 l2 && shape r1 r2
    | _ -> false

(** Exercise: list max exn [**] *)
let list_max_exn = function
    | [] -> failwith "list_max"
    | x :: xs -> begin
        let rec list_max_tr acc = function
            | [] -> acc
            | x2 :: x2s -> list_max_tr (max acc x2) x2s
        in list_max_tr x xs
    end


(** Exercise: list max exn string [**] *)
let list_max_string = function
    | [] -> "empty"
    | x :: xs -> begin
        let rec list_max_string_tr acc = function
            | [] -> acc
            | hd :: t -> list_max_string_tr (max acc hd) t
        in string_of_int (list_max_string_tr x xs)
    end

let list_max_string' lst = 
    try string_of_int (list_max_exn lst) with
        | Failure _ -> "empty"

(** Exercise: list max exn ounit [*] *)
(* you'll need to put this code in a separate file, as usual, to run the tests *)
(*
open OUnit2

let tests = "suite" >::: [
  "empty" >:: (fun _ -> assert_raises (Failure "list_max") (fun () -> list_max []));
  "nonempty" >:: (fun _ -> assert_equal 8 (list_max [3; 1; 4; 8]))
]

let _ = run_test_tt_main tests
*)

(** Exercise: is_bts [****] TBD*)
(** Exercise: quadrant poly [**] *)
let sign_poly x =
    if x < 0 then `Neg else
    if x > 0 then `Pos else `Zero

let quadrant_poly x y = 
  match (sign_poly x, sign_poly y) with
    | (`Pos, `Pos) -> Some `I
    | (`Neg, `Pos) -> Some `II
    | (`Neg, `Neg) -> Some `III
    | (`Pos, `Neg) -> Some `IV
    |  _ -> None
