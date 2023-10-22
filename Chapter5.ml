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

