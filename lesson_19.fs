module Nineteen

// 48.4.1
let rec fibo1 n n1 n2 = 
    let rec inner = function
        | (current_step, current, _) when current_step = n -> current
        | (current_step, current, previous) -> inner (current_step + 1 , current + previous, current)

    match n with
    | 0 -> n2
    | _ -> inner (1, n1, n2)

// let rec fib1 n n1 n2 = 
//     match n with
//     | 0 -> n2
//     | 1 -> n1
//     | n -> fib1 (n - 1) (n1 + n2) n1
    
// 48.4.2
let rec fibo2 n c =
    let rec inner = function
        | (current_step, current, _) when current_step = n -> c current
        | (current_step, current, previous) -> inner (current_step + 1 , c (current + previous), current)

    match n with
    | 0 -> 0
    | _ -> inner (1, 1, 0)

// let rec fib2 n c = 
//     match n with
//     | 0 -> c 0
//     | 1 -> c 1
//     | n -> fib2 (n - 2) (fun a -> fib2 (n - 1) (fun b -> c (a + b)))

// 48.4.3
let rec bigList n k =
    if n=0 then k []
    else bigList (n-1) (fun res -> k(1::res))