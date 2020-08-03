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

let (>.) (a,b,c) (d,e,f) =
    if c < f then true
    elif c = f && b < e then true
    elif c = f && b = e && a < d then true
    else false
    


//3.2


