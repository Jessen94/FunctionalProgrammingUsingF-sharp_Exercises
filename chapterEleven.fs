module Fsharp_book_exercises.chapterEleven

// 11.1
let odd1 =
    let rec aux i =
        seq {
             if i%2<>0 then
                yield i
                yield! aux (i+1)
             else
                yield! aux (i+1)
        }
    aux 1
let odd2 = Seq.initInfinite id |> Seq.where (fun i -> i%2<>0)

// 11.2

let fac1 =  (1,1) |> Seq.unfold(fun (a,b) -> Some(a,(a*b,b+1)))
// 11.3

// 11.4
let fromTo i n s = 
         s |> Seq.indexed
           |> Seq.filter(fun (m,c)  -> i<m && m>n)
           |> Seq.map snd
          
let next a x = (a/x + x)/2.0;;
let rec iter f x = function
                   | 0 -> x
                   | n -> iter f (f x) (n-1);;

let iterate f x = Seq.initInfinite (fun i -> iter f x i);;
// 11.5
let iterate2 f x =
    let rec aux prev = seq {
        let value = f prev 
        yield prev
        yield! aux value 
    }
    aux x 
// We copy enumerator declaration from Chapter 8 
open System.Collections.Generic;;

let enumerator (m: IEnumerable<'c>) =
    let e = ref (m.GetEnumerator())
    let f () =
        match (!e).MoveNext() with
        | false -> None
        | _     -> Some ((!e).Current)
    f;;

let rec inTolerance (eps:float) sq =
    let f = enumerator sq
    let nextVal() = Option.get(f())
    let rec loop a = let b = nextVal()
                     if abs(a-b) > eps then loop b else a
    loop(nextVal());;

let sRoot a = inTolerance 1E-6 (iterate (next a) 1.0);;
// 11.6
let sRootUnfold x = 1.0 |> Seq.unfold(fun a ->
                                        let _a = next x a
                                        let _b = next x _a
                                        if abs(a-_a) > 1E-6 then Some(next x _a,next x _b)
                                        else None
                                        ) |> Seq.last
// 11.7

let s = Seq.init 100 (float) 
let accfl sq =
    let rec aux acc sq = seq {
        yield acc + Seq.head sq
        yield! aux (acc + Seq.head sq) (Seq.tail sq)
    }
    aux 0.0 sq

// 11.11
let fac2 =
    let rec aux i prev = seq {
        yield prev
        yield! aux (i+1) (prev*i)
    }
    (aux 1 1)
    
