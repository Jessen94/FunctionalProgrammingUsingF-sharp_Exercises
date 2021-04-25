module ChapterThree
open System

//3.1
type AMorPM =
    | AM 
    | PM 
    
type time = {
    TimeOfDay: AMorPM
    hour: int
    min: int
    
}

let (.>.) time1 time2 = time1 > time2

let (>.) x y =
    match (x,y) with
    | ((_,_,c),(_,_,f)) when c < f -> true 
    | ((_,b,c),(_,e,f)) when c = f && b < e -> true
    | ((a,b,c),(d,e,f)) when c = f && b = e && a < d -> true
    | _ -> false
//3.2

type oldMoney =
    | Pence of int // 12 to a shilling
    | Shilling of int // 20 to a pound 
    | Pound of int
    
let AddPence a b =
    ((a+b)%12, (a+b)/12)
let AddShilling a b =
    ((a+b)%20, (a+b)/20)
let add =
    fun (a,b,c) (d,e,f) ->
        let (pence, carryShilling) = AddPence a d
        let (shilling, carryPound) = AddShilling (b+carryShilling) e
        (pence,shilling,(carryPound+c+f))

let pence a =
    ((a/12)*12, a%12)
let shilling a =
    ((a/20)*20, a%20)

let subtract =
    fun (a,b,c) (d,e,f) ->
    let (pence, carry) =
        match (a-d)>0 with 
         | false -> (a-d + 12, 1)
         | _ -> (a-d, 0)
    let (shilling, carry) =
        match (b-e-carry)>0 with 
         | false -> (b-e-carry + 20, 1)
         | _ -> (a-d, 0)
    let (pound, carry) =
        match (c-f-carry)>0 with 
         | false -> (c-f-carry, 1)
         | _ -> (c-f, 0)
    if (carry > 0) then
        failwith "You're out of money!"
    (pound, shilling, pence)

// 3.2

