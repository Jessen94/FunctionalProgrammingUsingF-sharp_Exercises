module Fsharp_book_exercises.chapterFive
// 5.1
let listFilterWithFoldBack = fun n lst-> List.foldBack(fun e acc -> if e<> n then e::acc else acc) lst []
// 5.2
let revrev = fun lst -> List.fold(fun acc e -> (List.fold (fun acc e -> e::acc) [] e)::acc ) [] lst
// 5.3
let sum a = List.fold(fun acc e -> if fst a e then acc+1 else acc) 0 (snd a)
// 5.4
let downto1 f n e =
    match n <= 0 with
    | true -> e
    | false -> List.foldBack f [1..n] e
let downto1Fac n =  downto1 (*) n 1
let lst  n = downto1 (fun e a -> e::a) n []
// 5.5
