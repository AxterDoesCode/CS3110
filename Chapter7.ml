(** Exercise: mutable fields [*] *)
type student = {name: string; mutable gpa: float}
let s = {name = "Alice"; gpa = 3.7}
let () = s.gpa <- 4.0

(** Exercise: refs [*] *)
let ( _ : bool ref) = ref true
let ( _ : int list ref) = ref [1;2;3]
let ( _ : int ref list) = [ref 1; ref 2; ref 3]

(** Exercise: inf fun [*] *)
let inc = ref (fun x -> x + 1)
let _ = !inc 3109

(** Exercise: addition assignment [**] *)
let ( +:= ) x y = x := !x + y

(** Exercise: physical equality [**] *)
let x = ref 0
let y = x
let z = ref 0
(*
# x == y;; true
# x == z;; false
# x = y;; true
# x = z;; true
# x := 1;; ()
# x = y;; true
# x = z;; false
*)

(** Exercise: norm [**] *)
type vector = float array
let norm (x : vector) = Array.map (fun a -> a *. a) x |> Array.fold_left ( +. ) 0. |> sqrt

(** Exercise: normalize [**] *)
let normalize (v: vector) = let n = norm v in Array.iteri (fun i x -> v.(i) <- x /. n) v

(** Exercise: norm loop [**] *)
let norm_loop x = 
    let norm = ref 0. in
    for i = 0 to (Array.length x) - 1 do
        norm := !norm +. (x.(i) ** 2.)
    done;
    sqrt !norm

(** Exercise: normalize loop [**] *)
let normalize_loop x =
    let n = norm_loop x in
    for i = 0 to Array.length x - 1 do
        x.(i) <- x.(i) /. n
    done

(** Exercise: init matrix [***] *)
let init_matrix n o f =
   Array.init n (fun i -> Array.init o (fun j -> f i j))
