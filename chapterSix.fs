module Fsharp_book_exercises.chapterSix

type Fexpr =
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr
    
let rec D =
    function
        | Const _     -> Const 0.0
        | X           -> Const 1.0
        | Add (fe,ge) -> Add(D fe, D ge)
        | Sub (fe,ge) -> Sub(D fe, D ge)
        | Mul (fe,ge) -> Add(Mul(D fe, ge), Mul(fe, D ge))
        | Div (fe,ge) -> Div(Sub(Mul(D fe, ge),Mul(fe, D ge)), Mul(ge,ge))
        | Sin fe      -> Mul(Cos fe, D fe)
        | Cos fe      -> Mul(Const -1.0, Mul(Sin fe, D fe))
        | Log fe      -> Div(D fe, fe)
        | Exp fe      -> Mul(Exp fe, D fe)

// 6.1
let rec red =
    function
        | Add(Const x, Const y) -> Const (x+y)
        | Add(Const 0.0, y)     -> red(y)
        | Add(x,Const 0.0)      -> red(x)
        
        | Sub(Const x, Const y) -> Const (x-y)
        | Sub(Const 0.0, y)     -> red(y)
        | Sub(x,Const 0.0)      -> red(x)
               
        | Mul(Const 1.0, y)     -> red(y)
        | Mul(x,Const 1.0)      -> red(x)
        | Mul(Const x,Const y)  -> Const (x*y)
        | Mul(Const 0.0, y)     -> Const 0.0
        | Mul(x,Const 0.0)      -> Const 0.0
        
// 6.2
let rec postfix =
    function
        | Const x     -> string x
        | X           -> "x"
        | Add (fe,ge) -> postfix fe + " " + postfix ge + " + "
        | Sub (fe,ge) -> postfix fe + " " + postfix ge + " - "
        | Mul (fe,ge) -> postfix fe + " " + postfix ge + " * "
        | Div (fe,ge) -> postfix fe + " " + postfix ge + " / "
        | Sin fe      -> postfix fe+ " sin "
        | Cos fe      -> postfix fe+ " Cos "
        | Log fe      -> postfix fe+ " log "
        | Exp fe      -> postfix fe+ " exp "

// 6.3
