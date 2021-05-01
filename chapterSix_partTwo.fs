module Fsharp_book_exercises.chapterSix_partTwo

type BinTree<'a when 'a : comparison> =
     | Leaf
     | Node of BinTree<'a> * 'a * BinTree<'a>;;
// 6.6
// I understood the assignemt as delete the smallest element at return the rest of the tree
let delete =
    fun t ->
    let firstValue = fun (Node(_,x,_)) -> x 
    let rec aux x =
            function
                | Node(Leaf,a,Leaf) -> Leaf
                | Node(Leaf,a,tr) when x<=a -> tr
                | Node(tl,a,tr) when x<=a -> Node(aux x tl,a,tr)
                | Node(tl,a,tr) when x>a -> Node(tl,a,aux x tr)
    aux (firstValue t) t