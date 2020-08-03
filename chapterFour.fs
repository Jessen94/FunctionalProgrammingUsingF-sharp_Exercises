module chapterFour
open System
// 4.1
let upto b =
    let rec aux a acc = match a with
                        | 0 -> acc
                        | x when a > 0 -> aux (a-1) (x::acc)
    aux b [];;

// 4.2
let downto1 n =
    let rec aux b a acc = match a with
                            | _ when a > b -> acc
                            | _ when a > 0 -> aux b (a+1) (a::acc)
                            | _ -> acc
                        
    aux n 1 [];;

// 4.3
let evenN n = List.filter(fun e -> (e % 2) = 0 ) ([0..n])

//4.4
let rec altsum = function
                 | [] -> 0
                 | [x] -> x
                 | x0::x1::xs -> x0 - x1 + altsum xs;;
                 
let rec altsum2 = function
                  | [] -> 0
                  | x0::x1::xs -> x0 - x1 + altsum xs;;

// 4.5
let rmodd n = n |> List.mapi (fun index item -> (index,item))
                |> List.filter (fun (index,item) -> index % 2 = 0)
                |> List.map (fun (index,item) -> item);;

// 4.6
let rmeven = List.filter (fun e -> e % 2 = 0)

// 4.7
let multiplicity x xs = xs |> List.filter (fun e -> e = x ) |> List.length

// 4.8
let split n = n |> List.mapi (fun index item -> (index,item))
                |> List.partition (fun (index,item)-> index % 2 = 0)
                


 