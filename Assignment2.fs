// Exercise 2.1

let rec downTo n =
    if n = 0 then List.Empty
    else n :: downTo(n-1)

// Exercise 2.1.1

let rec downTo2 n = 
    match n with
    |   0       -> List.Empty
    |   _       -> n::downTo2(n-1)

// Exercise 2.2
let removeOddIdx xs:int list = List.foldBack (fun x (l,r) -> x::r, l) xs ([],[]) |> fst

// Exercise 2.3
let rec combinePair xs = 
    match xs:int list with
    | []                -> []
    | [_]               -> []
    | x1::x2::xs        -> (x1,x2)::combinePair(xs)

// Exercise 2.4 (HR 3.2)
let (.+.) (po, sh, pe) (po1, sh1, pe1) =
    let pence = ((po*240) + (sh*12) + (pe)) + ((po1*240) + (sh1*12) + (pe1))
    let shilling = pence/12
    let pounds = shilling/20
    (pounds, shilling % 20, pence % 12)
let (.-.) (po, sh, pe) (po1, sh1, pe1) =
    let pence = ((po*240) + (sh*12) + (pe))
    let pence1 = ((po1*240) + (sh1*12) + (pe1)) 
    let totalpence = pence - pence1
    let shilling = totalpence/12
    let pounds = shilling/20
    (pounds, shilling % 20, totalpence % 12)

// 12 pence = 1 shilling
// 20 shilling = 1 pound

// Exercise 2.5 (HR 3.3)

let (.+) (a,b) (c,d) = ((+) a c) + ((+) b d)

let (.-) (a,b) (c,d) = ((-) a c) - ((-) b d)

let (.*) (a,b) (c,d) = ( ((-)((*) a c) ((*) b d )) - ((+)((*) b c)) (((*) a d)) )

let (./) (a,b) (c,d) = ((a*c + b*d)/(c*c+d*d)) + ((b*c-a*d)/(c*c+d*d))

// Exercise 2.6 (4.4)

let rec altsum = function
    | []        -> 0
    | x0::xs    -> x0 - altsum xs 

// Exercise 2.7

let explode s = 
    match s with
    | ""        -> []
    | _         -> List.ofArray (s.ToCharArray()) 
 
// Exercise 2.7.1

let rec explode2 s =
    match s with
    | ""            -> []
    | _             -> s.[0]::explode2(s.Remove(0,1))

// Exercise 2.8
let implode (x:char list) = List.foldBack (fun c s -> string c + s) x "" 

// this works too: let implode c = System.String.Concat(Array.ofList(c:char list))

// Exercise 2.8.1

let implode2 (x:char list) = List.fold (fun c s -> string s + c) "" x

// Exercise 2.9

let toUpper s:string = explode(s:string) |> List.map System.Char.ToUpper |> implode 

// Exercise 2.10

let palindrome (s:string) = implode(List.rev(explode(toUpper(s)))) = toUpper s

// Exercise 2.11
let rec ack = function
    | (0,n)                         -> n + 1
    | (m,0) when m > 0              -> ack(m-1,1)
    | (m,n) when m > 0 && n > 0     -> ack(m-1,ack(m,n-1))

// Exercise 2.12
let time f a =
    let start = System.DateTime.Now in
    let res = f a in
    let finish = System.DateTime.Now in
    (res, finish - start);

let timeArg1 f a = time(fun () -> f a)

// Exercise 2.13 (5.4)
let rec downTo1 f n e =
    if n <= 0 then e
    else f n (downTo1 f n e)

// Declare factorial function by use of downTo1



    



    
    




    
    

    