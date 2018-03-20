open System
open System.Drawing

// Exercise 1.1
let sqr x = x * x 

// Exercise 1.2
let pow x n = System.Math.Pow(x,n) 

// Exercise 1.3
let g n = n + 4 

// Exercise 1.4
let h (x,y) = System.Math.Sqrt(x * x + y * y) 

// Exercise 1.5
let rec f = function 
| 0 -> 1
| n -> n + f(n - 1)
// f 4;; val it : int = 11

// Exercise 1.6
let rec fibo = function 
| 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo(n-2)
// fibo 4;; val it : int = 3

// Exercise 1.7
let rec sum = function 
| (m,0) -> m
| (m,n) -> (m+n) + sum(m,n-1)

// Exercise 1.8
(*
    (System.Math.PI, fact-1) : (float, int)
    fact(fact 4) : int
    power(System.Math.PI, fact 2) : float
    (power, fact) : ((float, int) -> float, int -> int)
*)

// Exercise 1.9
(*
           [ a -> 5 ]
    env1 = [ f -> "a variable 'a' + 1" ]
           [ g -> "the function f of b + a" ]

    f 3;;
    val it : int = 4

    g 3;;
    val it : int = 9
*)   

// Exercise 1.10
let dub x:string = x + x

// Exercise 1.11
let rec dubn (s:string) n =
    match n with
    | 0 -> ""
    | _ -> s + dubn s (n-1)

// Exercise 1.12
let timediff (hh1,mm1) (hh2,mm2) = ((hh2)*60 + mm2) - ((hh1)*60 + mm1)

// Exercise 1.13
let minutes = timediff(00,00)

// Exercise 1.14
let rec pows = function
| (s:string,0) -> ""
| (s:string,n) -> s + pows(s,n-1)

// Exercise 1.15

// Exercise 1.16
(*
    1. int * int -> int
    2. x = 0
    3. f(2,3) -> f()
*)