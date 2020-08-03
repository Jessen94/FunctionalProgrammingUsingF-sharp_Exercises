module ChapterOne
open System
//1.1
let g n = n + 4;;
//1.2
let h (x,y) = Math.Sqrt (x**2.0)+(y**2.0);;
//1.4
let rec f n =
    if n > 0 then n + f(n-1)
    else 0;;
//1.5
let rec fib n = match n with
                | 0 -> 0
                | 1 -> 1
                | _ when n > 0 -> fib (n-1) + fib (n-2) ;;
//1.6
let rec sum (m,n) = match (m,n) with
                    | (m,0) -> 0
                    | (m,n) -> m + sum (m,n-1);;
//1.7
//1: float * int
//2: int -> int
//3: float * int -> float
//4: float * int

//1.8
//f 3 = 3 + 1 = 4

//g 3 = f 3 + 5
//    = 4 + 5
//    = 9