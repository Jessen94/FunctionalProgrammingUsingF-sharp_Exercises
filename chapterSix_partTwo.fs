module Fsharp_book_exercises.chapterSix_partTwo

open System

type BinTree<'a when 'a : comparison> =
     | Leaf
     | Node of BinTree<'a> * 'a * BinTree<'a>;;
// 6.6
// I understood the assigment as delete the smallest element at return the rest of the tree
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
    
// 6.7

// 6.8
type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP  | PUSH of float

type Stack =
    | Empty
    | Stack of float * Stack
    
let popFromStack stack = match stack with
                            | Empty -> None
                            | Stack (a,b) -> Some (a,b)
let intpInstr stack instr =
    let (a,newStack)    = match popFromStack stack with
                            | None        -> (0.0,Empty)
                            | Some (x,st) -> (x,st)
    let (b,newNewStack) = match popFromStack newStack with
                            | None        -> (0.0,Empty)
                            | Some (x,st) -> (x,st)
    match instr with
        | ADD    -> Stack(b+a,newNewStack)
        | SUB    -> Stack(b-a,newNewStack)
        | MULT   -> Stack(a*b,newNewStack)
        | DIV    -> Stack(b/a,newNewStack)
        | SIN    -> Stack(System.Math.Sin(a),newStack)
        | COS    -> Stack(System.Math.Cos(a),newStack)
        | LOG    -> Stack(System.Math.Log(a),newStack)
        | EXP    -> Stack(System.Math.Exp(a),newStack)
        | PUSH x -> Stack(x,stack)
let intpProg iList =
    let rec aux stack = 
        function
            | [a]   -> intpInstr stack a 
            | a::ac -> aux (intpInstr stack a) ac
    aux Empty iList

let p1 = [PUSH 10.0; PUSH 4.0; PUSH 4.0; ADD; SUB]

let trans: obj = failwith "Not implemted"

// 6.9

type Department =
    | Empty
    | SubDepartment of string * int * List<Department>
    
let departments =
    let rec aux acc remaningDeps =
        function
            | Empty -> acc
            | SubDepartment (a,b,[]) when remaningDeps <> [] ->
                let remaning = match remaningDeps with
                                | [a]   -> []
                                | _::ac -> ac
                aux ((a,b)::acc) (remaning) (remaningDeps.Head)
            | SubDepartment (a,b,[])-> aux ((a,b)::acc) [] (Empty)
            | SubDepartment (a,b,c::cc) ->
                aux ((a,b)::acc) cc c
            
    aux [] []

let totalIncome deps =
    let rec aux acc =
        function
            | SubDepartment (_,b,[])     -> acc+b
            | SubDepartment (_,b, c::cc) ->
                let subGrossSummed = List.fold(fun ac2 e -> (departments e |> List.fold(fun ac1 (_,gi) -> gi+ac1) 0)+ac2) 0 (c::cc)  
                acc+subGrossSummed+b
    aux 0 deps
    

let allDepWithTotalIncome deps =
    let rec aux acc remaningDeps =
        function
            | Empty -> acc
            | SubDepartment (a,b,[]) when remaningDeps <> [] ->
                let remaning = match remaningDeps with
                                | [a]   -> []
                                | _::ac -> ac
                aux ((a,b)::acc) (remaning) (remaningDeps.Head)
            | SubDepartment (a,b,[]) -> (a,b)::acc
            | SubDepartment(a,b,c::cc) ->
                let subGrossSummed = List.fold(fun ac2 e -> (departments e |> List.fold(fun ac1 (_,gi) -> gi+ac1) 0)+ac2) 0 (c::cc)  
                aux ((a,subGrossSummed+b)::acc) cc c
    aux [] [] deps
let dep1 = SubDepartment("Dir",1000, [SubDepartment("IT",1000,[SubDepartment("support",1000, [])]);SubDepartment("HR",1000, [])])
