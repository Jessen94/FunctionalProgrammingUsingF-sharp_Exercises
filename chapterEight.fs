module Fsharp_book_exercises.chapterEight

(*
// 8.1
x -> loc1      loc1: 1
y -> loc2      loc2: (1,2)
z -> (1,2)
x -> loc1      lo1: 7

// 8.2
Because the list "a" is mutable the F# compiler will not hinder you in
writing the function. Though if a was declared as an array this wouldn't
be possible as array are already mutable, hence all the index in the array
gets memory locations upon initialisation, meaning the value in the indexes
can be mutated but the array can't be extended.

Although if you don't declare f(1), it will complain, because you are assigning
a polymorphic type as mutable, which is not allowed
Alternative declaration could be
let mutable (a: int list) = []
As here you tell the compile that, your are only gonna use it for integers

let mutable a = []
let f x  = a <- (x::a)
f(1)

// 8.3
Env                             Store
x |-> { a|-> loc2}              loc1: {a |-> loc2 }: loc2: 1
y |-> { b |-> 1, c |-> loc2 }   loc3: {c |-> loc2}
                                loc2: 3
                              
*)


// 8.6
let whileFib n =
    let mutable n1 = 0
    let mutable n2 = 1
    let mutable i = 0
    while n>i do
        let n3 = n2 + n1 
        n1 <- n2
        n2 <- n3
        i <- i+1
    n2
        
  