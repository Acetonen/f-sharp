module TwentyTwo

type 'a cell = Nil | Cons of 'a * Lazy<'a cell>

let rec nat (n:int) : 'a cell = Cons (n, lazy(nat(n+1)))

let n0 = nat 0

let hd (s : 'a cell) : 'a =
    match s with
    Nil -> failwith "hd"
    | Cons (x, _) -> x

let tl (s : 'a cell) : Lazy<'a cell> =
    match s with
    Nil -> failwith "tl"
    | Cons (_, g) -> g


// 51.3
let rec nth (s : 'a cell) (n : int) : 'a =
    match s with
    | s when n = 0 -> hd s
    | s -> let tail = tl s
           nth (tail.Force()) (n - 1)

// EXAMPLE:
// nth n0 30000;;
// val it : int = 30000