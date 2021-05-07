module Fsharp_book_exercises.chapterNine

// 9.2
(*
The function gcd is iterative as it follows the scheme defined on page 209
*)
let rec gcd (a,b) = if a=0 then b else gcd (b%a,a)
// 9.3
let rec sum (m,n,acc) = if n>0 then sum (m,n,(m+n)) else acc
// 9.4
let lstLength =
    fun lst -> 
    let rec aux acc = 
        function
            | [] -> acc
            | _::ac -> aux (acc+1) ac
    aux 0 lst
// 9.5


// 9.6
let factC =
   let rec aux c  =
       function
           | 0 -> c 1
           | a ->  aux (fun res -> c(a*res)) (a-1)
   aux id
(*
The running time for factC
    > for i in xs16 do let _ = factC i in ();;
    Real: 00:00:00.107, CPU: 00:00:00.109, GC gen0: 62, gen1: 1, gen2: 0
    val it : unit = ()
*)
// 9.7

let fibA =
    let rec aux ac1 ac2 =
        function
            | 0 -> ac1
            | 1 -> ac2
            | a -> aux (ac2) (ac1+ac2) (a-1) 
    aux 0 1
    
let fibC x =
    let rec aux c =
        function
            | 0 -> c 0
            | 1 -> c 1
            | a -> aux (fun ac1 -> aux (fun ac2 -> c(ac1+ac2)) (a-2) ) (a-1) 
    aux id x
// 9.8
type BinTree<'a> =
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a> 
let countA a =
    let rec aux acc subtrees =
        function    
            | Leaf -> match subtrees with
                        | [] -> acc
                        | a::ac -> aux acc ac a 
            | Node (tL, _ , tR) -> aux (acc+1) (tR::subtrees) tL
    aux 0 [] a
    
// 9.10
let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k (res))
(*
This function returns stack overflow because of the way that the continuation function is setup
it doesn't accumilate the result in the continuation it appends 1 to the result of the continuation
making it no better than a regular recursive function. To fix the problem change this "1::k(res)" ->
"k(1::res)", this wil make sure that there is no dangling computation   
*)
// 9.11
let leftTree i =
    let rec aux acc =
        function
            | 0 -> acc
            | a -> aux (Node(acc,(),Leaf)) (a-1)
    aux Leaf i
let rightTree i =
    let rec aux acc =
        function
            | 0 -> acc
            | a -> aux (Node(Leaf,(),acc)) (a-1)
    aux Leaf i

