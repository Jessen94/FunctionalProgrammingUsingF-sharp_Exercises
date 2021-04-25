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
let split n =
    let (lst1,lst2) =  n |> List.mapi (fun index item -> (index,item))
                         |> List.partition (fun (index,item)-> index % 2 = 0 )
    (lst1 |> List.map snd,lst2 |> List.map snd)            
// 4.9

let zip = List.zip
let zip2 lst1 lst2 =
    if (List.length lst1) <> (List.length lst2) then failwith "Lists not of equal length"
    else
        let rec aux acc a b =
            match (a,b) with
            | ([],[]) -> List.rev acc
            | (c::cc,d::dd) -> aux ((c,d)::acc) cc dd
        aux [] lst1 lst2
// 4.10

let prefix lst1 lst2  =
    let rec aux =
        function
            | ([],[])       -> true
            | (a::aa,b::bb) when a=b -> aux (aa,bb)
            | _ -> false
    aux (lst1,lst2)

// 4.11
let count =
    fun (lst,i) ->
        let rec aux acc =
            function
                | []    -> acc
                | a::ac when a=i -> aux (acc+1) ac
                | a::ac when a>i -> acc
        aux 0 lst
        
let insert (lst,i) =
    let rec aux acc =
        function
            | [] -> List.rev (i::acc)
            | a::ac when i<a -> aux (a::acc) ac
            | a::ac -> (List.rev (a::acc)) @ ac
    aux [] lst

let intersect a = 
    let rec aux acc =
        function
            | ([],_) -> List.rev acc
            | (_,[]) -> List.rev acc
            | (a::aa,b::bb) when a=b -> aux (a::acc) (aa,bb) 
            | (a::aa,b::bb) when a>b -> aux (acc) (a::aa,bb) 
            | (a::aa,b::bb) when a<b -> aux (acc) (aa,b::bb)
    aux [] a

let plus a =
    let rec aux acc =
        function
            | ([],[])       -> List.rev acc
            | (a,[])        -> (List.rev acc) @ a
            | ([],a)        -> (List.rev acc) @ a
            | (a::ac,b::bc) when a>b -> aux (b::acc) (a::ac,bc)
            | (a::ac,b::bc) when a<b -> aux (a::acc) (ac,b::bc)
            | (a::ac,b::bc) -> aux (a::acc) (ac,b::bc)
    aux [] a            

let minus a =
    let rec aux acc =
        function
            | ([],[])       -> List.rev acc
            | (a,[])        -> (List.rev acc) @ a
            | ([],a)        -> (List.rev acc) @ a
            | (a::aa,b::bb) when a=b -> aux (acc) (aa,bb) 
            | (a::aa,b::bb) when a>b -> aux (b::acc) (a::aa,bb) 
            | (a::aa,b::bb) when a<b -> aux (a::acc) (aa,b::bb)
    aux [] a            
// 4.12
let sum a = List.fold(fun acc e -> if fst a e then acc+1 else acc) 0 (snd a)
// 4.13
let min =
    let rec aux acc =
        function
            | [] -> acc
            | a::ac when a<acc -> aux a ac
            | _::ac -> aux acc ac
    aux 0
    
let delete x =
    let rec aux acc =
        function
            | [] -> acc
            | a::ac when fst x = a -> ac@acc
            | a::ac -> aux (a::acc) ac
    aux [] (snd x)
    
let sort =
    let rec aux acc =
        function
            | []   -> acc
            | a    ->
                let minE = min a
                aux (acc@[minE]) (delete (minE,a))
    aux []                 

//4.14
let minOpt =
    let rec aux acc =
        function
            | [] -> Some acc
            | a::ac when a<acc -> aux a ac
            | _::ac -> aux acc ac
    aux 0
// 4.15
let revrev a =
    let rec aux acc =
        function
            | [] -> acc
            | a::ac ->
                    let rec aux2 acc =
                        function
                            | [] -> acc
                            | a::ac -> aux2 (a::acc) ac
                    aux ((aux2 [] a)::acc) ac                    
    aux [] a

// 4.16
