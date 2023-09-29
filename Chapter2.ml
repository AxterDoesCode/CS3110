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

