module Nineteen

// 48.4.1
let rec fibo1 n n1 n2 = 
    let rec inner = function
        | (current_step, current, _) when current_step = n -> current
        | (current_step, current, previous) -> inner (current_step + 1 , current + previous, current)

    match n with
    | 0 -> n2
    | _ -> inner (1, n1, n2)

// 48.4.2
let rec fibo2 n c =
    let rec inner = function
        | (current_step, current, _) when current_step = n -> c current
        | (current_step, current, previous) -> inner (current_step + 1 , c (current + previous), current)

    match n with
    | 0 -> 0
    | _ -> inner (1, 1, 0)

// 48.4.3
let rec bigList n k =
    if n=0 then k []
    else bigList (n-1) (fun res -> k(1::res))