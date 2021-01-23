module Fourteen

// 40.1
let rec sum (p, xs) = 
    match xs with
        | head :: tail when p head -> head + sum (p, tail)
        | head :: tail -> sum (p, tail)
        | [] -> 0

// 40.2.1
let rec count (xs, n) =
    match xs with
        | head :: tail when head < n -> count (tail, n)
        | head :: (head2 :: tail) when (head = n && head2 = n) -> 1 + count ((head2 :: tail), n)
        | head :: _ when head = n -> 1
        | _ -> 0

// 40.2.2
let rec insert (xs, n) = 
    match xs with
        | head :: tail when head >= n -> n :: head :: tail
        | head :: tail -> head :: insert (tail, n) 
        | [] -> [n]

// 40.3.2
let rec delete (n, xs) = 
    match xs with
        | head :: tail when head = n -> tail
        | head :: tail -> head :: delete (n, tail) 
        | [] -> []

// 40.2.3
let rec intersect (xs1, xs2) =
    match xs1 with
        | head :: tail when List.contains head xs2 -> head :: intersect (tail, delete (head, xs2))
        | _ :: tail -> intersect (tail, xs2)
        | [] -> []

// 40.2.4
let rec plus (xs1, xs2) =
    match (xs1, xs2) with
        | (head :: tail, head2 :: tail2) when head < head2 -> head :: plus (tail, head2 :: tail2)
        | (head :: tail, head2 :: tail2) -> head2 :: plus (head :: tail, tail2)
        | (list, []) -> list
        | ([], list) -> list
        | ([], []) -> []

// 40.2.5
let rec minus (xs1, xs2) =
    match xs2 with
        | head :: tail when List.contains head xs1 -> minus (delete (head, xs1), tail)
        | _ :: tail -> minus (xs1, tail)
        | [] -> xs1

// 40.3.1
let rec smallest list = 
    let rec inner = function
        | (result, head :: tail) when head < result -> inner (head, tail)
        | (result, _ :: tail) -> inner (result, tail)
        | (result, []) -> result

    match list with
        | [] -> 0
        | head :: tail -> inner (head, tail)


// 40.3.3
let rec sort = function
    | [] -> []
    | list -> smallest list :: ((smallest list, list) |> delete |> sort)

// 40.4
let rec revrev = function
    | head :: tail -> revrev tail @ [List.rev head]
    | [] -> []