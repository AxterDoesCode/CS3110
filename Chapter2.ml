(** Exercise: values [*] 
    This can be demonstrated using manual type annotations*)
let _: int = 7 * (1 + 2 + 3)
let _: string = "CS " ^ string_of_int 3110

(** Exercise: operators [**]
   *)

let _ = 42 * 10
let _ = 3.14 /. 2.
let _ = 4.2 ** 7.

(** Exercise: equality [*]
   *)

let _ = 42 = 42
let _ = "hi" = "hi"
let _ = "hi" == "hi"

(** Exercise: assert [*]
   *)
let _ = assert (2110 <> 3110)

(** Exercise: if [*]
   *)
let _ = if 2 > 1 then 42 else 7

(** Exercise: double fun [*]
   *)
let double x = x * 2
let _ = assert (double 12 = 24)

(** Exercise: more fun [*]
   *)
let cube x = x *. x *. x
let sign x = if x > 0 then 1 else if x = 0 then 0 else -1
let circle r = Float.pi *. r ** 2.

let close_enough a b = Float.abs (a -. b) < 1e-5

let _ = assert (close_enough (circle 1.) Float.pi)

(** Exercise: RMS [**]
   *)
let rms x y = ((x ** 2.) +. (y ** 2.)) /. 2. |> Float.sqrt
let _ = assert (close_enough (rms 2. 2.) 2.)

(** Exercise: date fun [***]
   *)
let datefun d m = if m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec" 
    then 1 <= d && d <= 31
    else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov"
    then 1 <= d && d <= 30
    else if m = "Feb"
    then 1 <= d && d <= 28
    else false
