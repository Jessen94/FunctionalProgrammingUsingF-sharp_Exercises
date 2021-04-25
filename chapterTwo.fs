module ChapterTwo
open System

// 2.1
let f n = if n % 2 = 0 || n % 3 = 0 then true
            else false;;

// 2.2
let rec pow (s:string,n) = match n with
                            | 0 -> s
                            | _ when n > 0 -> s + pow(s,n-1);;
// 2.3
let isIthChar(str:string,i,ch) = if ch = str.[i] then true else false
//2.4
let occFromIth =
    function
     | (str, i, _) when i > String.length str -> 0
     | (str, i, ch) -> str.[i..] |> Seq.filter(fun e -> e=ch) |> Seq.length 
    

//2.5
let occInString (str,ch) = String.collect(fun c -> if c = ch then string c else "") str |> String.length

//2.6
let div (d,n) = d%n
let notDivisible (d,n) = match n%d with
                         | 0 -> false
                         | _ -> true;;
//2.7.1
let rec notDivisible2(a,b,c) = match (a,b) with
                                | _ when a < b -> notDivisible(a,c) && notDivisible2(a+1,b,c)
                                | _ -> notDivisible(a,c);;
//2.7.2
let prime n = match n with
                  | 1 -> true
                  | 2 -> true
                  | 3 -> true
                  | _ -> notDivisible2(4,n,n)
//2.7.3
let rec nextPrime n = match prime(n+1) with
                      | false -> nextPrime n+1
                      | _ -> n+1;;

//2.8
let rec bin (n,k) = match (n,k) with
                    | (_,0) -> 1
                    | (row,_) when row = n -> 1
                    | (_,_) -> bin(n-1,k-1) + bin(n-1,k);;
//2.9
    //1: (int*int) -> int
    //2: f(0,_)
    //3: f(2,3) -> f(2-1, 2*3) -> f(1,6) -> f(1-1,6) -> f(0,6) -> 6
    //4:
    
// 2.10
    //1: (bool * int) -> int
    //2: Exception
    //3: 0

//2.11
let VAT n x = x * (float (n/100))

let unVAT n x = (float) n/(VAT n x)

//2.12

//2.13
let curry =
    fun a ->
        fun b ->
            fun c ->
             a(b,c)

let uncurry =
    fun a ->
        fun (b,c) -> a b c
