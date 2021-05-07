module Fsharp_book_exercises.chapterNine_partTwo

// 9.12

type BinTree<'a when 'a : comparison> =
     | Leaf
     | Node of BinTree<'a> * 'a * BinTree<'a>;;

let rec preOrder = function
    | Leaf          -> []
    | Node(tl,x,tr) -> x :: (preOrder tl) @ (preOrder tr);;
    
let preOrderC t =
    let rec aux c =
        function
            | Leaf -> c []
            | Node(tl,x,tr) ->  aux (fun p1 -> aux (fun p2 -> c(x::(p1@p2))) tr) tl
    aux id t
(*
> preOrder t15;;
Real: 00:00:39.581, CPU: 00:00:39.578, GC gen0: 3733, gen1: 1219, gen2: 10

> preOrderC t15;;
Real: 00:00:43.628, CPU: 00:00:43.656, GC gen0: 4287, gen1: 1340, gen2: 5
*)
let t1 = Node(Node(Leaf, -3, Leaf),0,Node(Leaf, 2, Leaf))
let t2 = Node(t1, 5, Node(Leaf, 7, Leaf))
let t3 = Node(t1, 5, Node(t2, 7, Leaf))
let t4 = Node(t3, 5, Node(t2, 7, t2))
let t5 = Node(t4,5,Node(t4,0,t4))
let t6 = Node(Node(t5,0,t5),7,Node(t5,0,t5))
let t7 = Node(Node(t6,0,t6),7,Node(t6,0,t6))
let t8 = Node(Node(t7,0,t7),7,Node(t7,0,t7))
let t9 = Node(Node(t8,0,t8),7,Node(t8,0,t8))
let t10 = Node(Node(t9,0,t9),7,Node(t9,0,t9))
let t11 = Node(Node(t10,0,t10),7,Node(t10,0,t10))
let t12 = Node(Node(t11,0,t11),7,Node(t11,0,t11))
let t13 = Node(Node(t12,0,t12),7,Node(t12,0,t12))
let t14 = Node(Node(t13,0,t13),7,Node(t13,0,t13))
let t15 = Node(Node(t14,0,t14),7,Node(t14,0,t14))