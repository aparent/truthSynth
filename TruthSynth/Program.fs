// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
 
type Gate =   Not of int 
            | CNot of int*int
            | Toffoli of int*int*int 

let applyNot (entry : bool array) targ = 
    entry.[targ] <- not entry.[targ]
     
let applyCNot (entry : bool array) ctrl targ = 
    if entry.[ctrl] 
    then entry.[targ] <- not entry.[targ]

let applyToff (entry : bool array) ctrl1 ctrl2 targ =
    if entry.[ctrl1] && entry.[ctrl2]
    then entry.[targ] <- not entry.[targ]

let applyGateEntry gate (entry : bool array)  = 
    match gate with 
    | Not targ -> applyNot entry targ
    | CNot (ctrl,targ) -> applyCNot entry ctrl targ 
    | Toffoli (ctrl1,ctrl2,targ) -> applyToff entry ctrl1 ctrl2 targ

let applyGate  (gate: Gate) (tt : (bool array) array) = 
    Array.iter (applyGateEntry gate) tt

let hDistance (a : bool array) (b : bool array) = 
   let mutable count = 0
   for i in 0 .. a.Length - 1 do
     if not (a.[i] && b.[i]) 
     then count <- count + 1
   count

//Algorithm L from page 358 of TAOCP vol 4A (1st Edition)
let comb t n = 
    let mutable c = Array.init (t+2) (fun index ->  if index < t then index 
                                                    else if index = t then n
                                                        else 0 
                                     )
    let mutable res = []
    let mutable j = 1
    while t >= j do
        j <- 1
        res <- c.[0..t-1] :: res
        while c.[j-1] + 1 = c.[j] do
            c.[j-1] <- j - 1
            j <- j + 1
        c.[j-1] <- c.[j-1] + 1
    res      


[<EntryPoint>]
let main argv = 
    let mutable test = 
        [| 
            [| false;false;true  |]; 
            [| true;false;false  |]; 
        |]
    applyGate (Not 1) test
    printf "%A\n" (comb 2 4) 
    printf "%A\n" test
    printf "%d\n" (hDistance test.[0] test.[1])
    0 // return an integer exit code
