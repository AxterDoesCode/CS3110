(** Exercise: complex synonym [*] *)
module type ComplexSig = sig
    type t = float * float
    val zero : t
end

(** Exercise: complex encapsulation [**] *)
(*Removing zero from the structure violates the signature as it requires a val of zero*)
(*Removing add from the signature causes an error as Complex specifies a function which is not specified by ComplexSig*)
(*Changing zero to 0,0 violates the signature as it requires a tuple of float*)

(** Exercise: big list queue [**] *)
module ListQueue = struct
    type 'a queue = 'a list
    let empty = []
    let enqueue x q = q @ [x]
end

let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

(** Exercise: big batched queue [**] *)
module BatchedQueue = struct
    type 'a t = {o: 'a list; i: 'a list}
    let empty = {o = []; i = []}
    let enqueue x = function
        | {o = []} -> {o = [x]; i = []}
        | {o;i} -> {o; i = x :: i}
end

let fill_batchedqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (BatchedQueue.enqueue n q) in
  loop n BatchedQueue.empty

(** Exercise: queue efficiency [***] *)
(*ListQueue.enqueue is linear time in the length of the queue as it uses the @ operator
This operator requires traversing the entire length of the list so that it can append to the end.
This means that adding n elements is quadratic in n*)
(*BatchedQueue.enqueue is constant time this is because it uses the cons operator, this means it's linear in n
to add n elements as each item has a constant time*)

(** Exercise: binary search tree map [****] *)
module type Map = sig
    type ('k,'v) t
    val empty : ('k, 'v) t
    val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
    val lookup : 'k -> ('k, 'v) t -> 'v
end

module BstMap:Map = struct
    type 'a tree =
        | Leaf
        | Node of 'a * 'a tree * 'a tree
    type ('k, 'v) t = ('k * 'v) tree
    let empty = Leaf
    let rec insert k v = function
        | Leaf -> Node((k, v), Leaf, Leaf)
        | Node((k', v'), l, r) -> 
          if (k = k') then Node((k', v'), l, r)
          else if (k < k') then Node((k', v'), insert k v l, r)
          else Node((k', v'), l, insert k v r)

    let rec lookup k = function
        | Leaf -> failwith "Doesn't exist"
        | Node((k', v'), l, r) ->
            if k = k' then v'
            else if k < k' then lookup k l else lookup k r
end

(** Exercise: fraction [***] *)
module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (** [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction : Fraction = struct
    type t = int * int

    let make n d = assert (d != 0); (n, d)

    let numerator = fst

    let denominator = snd
    
    let to_string t = string_of_int (fst t) ^ "/" ^ string_of_int (snd t)

    let to_float (n, d) = float_of_int n /. float_of_int d

    let add (n1, d1) (n2, d2) =
        if d1 = d2 then (n1+n2, d1)
        else (n1 * d2 + n2 * d1, d1 * d2)

    let mul (n1, d1) (n2, d2) = (n1*n2, d1*d2)
end

let rec gcd x y =
    if x = 0 then y
    else if (x < y) then gcd (y - x) x
    else gcd y (x - y)

(** Exercise: fraction reduced [***] *)
module RedFraction : Fraction = struct
    type t = int * int


    let make n d = 
        assert (d != 0); 
        (n, d)

    let numerator = fst

    let denominator = snd
    
    let to_string t = string_of_int (fst t) ^ "/" ^ string_of_int (snd t)

    let to_float (n, d) = float_of_int n /. float_of_int d

    let add (n1, d1) (n2, d2) =
        if d1 = d2 then (n1+n2, d1)
        else (n1 * d2 + n2 * d1, d1 * d2)

    let mul (n1, d1) (n2, d2) = (n1*n2, d1*d2)
end

(** Exercise: make char map [*] *)
(*module CharMap = Map.Make(Char)*)
(*[empty] is an empty character map, it is of type 'a t since it is a map whose value type is not yet known*)
(*[add] takes a key of type [key] a value of 'a and a map of type 'a t and returns a map of the same type*)
(*[remove] takes a key of type [key] and a map of 'a t and returns a map of the same type*)

(** Exercise: char ordered [*] *)
(*Char can be used in Map.Make since it fulfills the Map.OrderedType interface/module type, it has a type t and a compare function*)

(** Exercise: use char map [*] *)
module CharMap = Map.Make(Char)
let map = CharMap.(empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra" |> add 'V' "Victor")
let e = CharMap.find 'E' map
let map' = CharMap.remove 'A' map
let _ = CharMap.mem 'A' map'
let bindings = CharMap.bindings map'

