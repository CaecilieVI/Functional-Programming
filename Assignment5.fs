module assignment

// Exercise 4.1 - no code required.
// let xs = [1;2]
// g 2 --> 
// STACK            HEAP
// n = 2    -->     push 2 --> [2;] (n-1)
// reverse  -->     [2;]
// n = 1    -->     push 1 --> [2;1] (n-1) 
// reverse          [1;2]
// n = 0    -->     xs
// n = 2    -->     pop 2 --> push 2 --> [1;2;2] (n-1)
// reverse          [2;2;1]
// n = 1    -->     pop 1 --> push 1 --> [2;2;1;1]
// reverse          [1;1;2;2]
// it       -->     [1;1;2;2]

// Exercise 4.2
let rec sumA = function
    | (m,0,a) -> a
    | (m,n,a) -> sumA(m,n-1,a+m+n)

let sumIt m n = sumA (m,n,m)

// Exercise 4.3
let rec lengthA (l,a) = 
    match l with
    | [] -> a
    | _::xs -> lengthA(xs,a+1)
   
// Exercise 4.4
let rec factC (n, m) c = 
    match (n, m) with
    | (0, m) | (1, m) -> 1
    | (_, m) -> factC (n-1, m) (fun x -> c(n * x))

// Exercise 4.5
let fib n = 
    let l = new ResizeArray<_>()
    let mutable a = 1
    let mutable b = 2
    let mutable ns = n
    while ns > 1 do
        let c = a + b
        l.Add(c)
        a <- b
        b <- c
        ns <- ns - 1
    l |> List.ofSeq 

// Exercise 4.6
let rec fibA n n1 n2 =
    match n with
    | 0 -> n1
    | 1 -> n2
    | _ -> fibA (n-1) n2 (n1+n2) 

let rec fibC n c = 
    match n with
    | 1
    | 2 -> c(1)
    | _ -> 
        let firstParam x =
            let secondParam y =
                c(x+y)
            fibC (n-2) secondParam
        fibC (n-1) firstParam

// Exercise 4.7

type BinTree<'a> = 
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>;;

let rec countA acc t = 
    match t with
    | Leaf -> acc 
    | Node(tl,n,tr) -> (countA acc tl + countA acc tr) + 1

// Exercise 4.8
let rec countAC t a c = 
    match t with
    | Leaf -> c a
    | Node(tl, n, tr) ->
      countAC tl (a+1) (fun x -> countAC tr x c)

let count t = countAC t 0 id

// Exercise 4.9 - no code required
// bigListK will stop due to stackoverflow becuase bigListK applies 1 
// to the continuation function, which ultimately stacks up a lot of 1's.

// Exercise 4.10 HR exercise 9.11
let rec leftTree n t = 
    if n = 0
    then t (Node(Leaf, 0, Leaf))
    else Node(leftTree (n-1) t, n, Leaf) 

let rec rightTree n t = 
    if n = 0
    then t (Node(Leaf, 0, Leaf))
    else Node(Leaf, n , rightTree (n-1) t)
// Exercise 4.11 - OBS: sequence should be infinite or at least larger than 6
let oddNums = Seq.initInfinite(fun n -> (n*2)+1)
let naiveOddNums n = seq[1..2..n]

// Exercise 4.12 - OBS: sequence should be infinite or at least larger than 6
let factNums (n:int) =
    seq { for i in [1..n] -> i}
    |> Seq.reduce (fun acc i -> acc * i)