module Thirteen

// 39.1
let rec rmodd list = 
    let rec inner = function
        | (_, [], result) -> result
        | (0, _ :: tail, []) -> inner (1, tail, [])
        | (counter, _ :: tail, result) when counter % 2 = 0 -> inner (counter + 1, tail, result)
        | (counter, head :: tail, result) -> inner (counter + 1, tail, head :: result)

    (0, list, []) |> inner |> List.rev

// 39.2
let rec del_even = function
    | [] -> []
    | head :: tail when head % 2 = 0 -> del_even tail
    | head :: tail -> head :: del_even tail

// 39.3
let rec multiplicity x xs = 
    let rec inner = function
        | ([], result) -> result
        | (head :: tail, result) when head = x -> inner (tail, result + 1)
        | (_ :: tail, result) -> inner (tail, result)

    inner (xs, 0)

// 39.4
let rec split list= 
    let rec inner = function
        | (_, [], odd, even) -> (List.rev odd, List.rev even)
        | (counter, head :: tail, odd, even) when counter % 2 = 0 -> inner (counter + 1, tail, odd, head :: even)
        | (counter, head :: tail, odd, even) -> inner (counter + 1, tail, head :: odd, even)

    inner (1, list, [], [])

exception DifferentSizeException
// 39.5
let rec zip (xs1, xs2) =
    match (xs1, xs2) with
    | (h1 :: t1, h2 :: t2) -> (h1, h2) :: zip (t1, t2)
    | ([], []) -> []
    | ([], _) | (_, []) -> raise DifferentSizeException