(** Exercise: values [*] This can be demonstrated using manual type annotations*)
let _: int = 7 * (1 + 2 + 3)
let _: string = "CS " ^ string_of_int 3110

(** Exercise: operators [**] *)

let _ = 42 * 10
let _ = 3.14 /. 2.
let _ = 4.2 ** 7.

(** Exercise: equality [*] *)

let _ = 42 = 42
let _ = "hi" = "hi"
let _ = "hi" == "hi"

(** Exercise: assert [*] *)
let _ = assert (2110 <> 3110)

(** Exercise: if [*] *)
let _ = if 2 > 1 then 42 else 7

(** Exercise: double fun [*] *)
let double x = x * 2
let _ = assert (double 12 = 24)

(** Exercise: more fun [*] *)
let cube x = x *. x *. x
let sign x = if x > 0 then 1 else if x = 0 then 0 else -1
let circle r = Float.pi *. r ** 2.

let close_enough a b = Float.abs (a -. b) < 1e-5

let _ = assert (close_enough (circle 1.) Float.pi)

(** Exercise: RMS [**] *)
let rms x y = ((x ** 2.) +. (y ** 2.)) /. 2. |> Float.sqrt
let _ = assert (close_enough (rms 2. 2.) 2.)

(** Exercise: date fun [***] *)
let datefun d m = if m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec" 
    then 1 <= d && d <= 31
    else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov"
    then 1 <= d && d <= 30
    else if m = "Feb"
    then 1 <= d && d <= 28
    else false

(** Exercise: fib [**] *)

let rec fib n = if n <= 2 then 1
        else fib (n - 1) + fib (n - 2)

(** Exercise: fib fast [***] *)

let rec h n pp p = if n = 0 then p
    else h (n - 1) p (pp + p)

let fib_fast n = if n=0 then 0
    else h n 0 1

(** Exercise: poly types [***] *)

let f x = if x then x else x
(* bool -> bool *)

let g x y = if y then x else x
(* 'a -> bool -> 'a *)

let h x y z = if x then y else z
(* bool -> 'a -> 'a -> 'a *)

let i x y z = if x then y else y
(* bool -> 'a -> 'b -> 'a *)

(** Exercise: divide [**] *)
let divide ~numerator:x ~denominator:y = x /. y

(** Exercise: associativity [**] *)
let add x y = x + y

(* add 5 1 produces an integer
   add 5 produces a function due to partial functions
   (add 5) 1 produces an integer
   add (5 1) produces an error since 1 cannot be applied to 5 since 5 isn't a function*)

(** Exercise: average [**] *)

let ( +/. ) x y = (x +. y) /. 2.

(** Exercise: hello world [*] *)

(* print_endline adds a \n character to the output whereas print_string doesn't*)

