module Fourteen

// 40.1
let rec sum (p, xs) = 
    match xs with
    | [] -> 0
    | head :: tail when p head -> head + sum (p, tail)
    | _ :: tail -> sum (p, tail)
    

// 40.2.1
let rec count (xs, n: int) =
    match xs with
    | _ -> 0
    | head :: tail when head < n -> count (tail, n)
    | head :: (head2 :: tail) when (head = n && head2 = n) -> 1 + count ((head2 :: tail), n)
    | head :: _ when head = n -> 1
    

// 40.2.2
let rec insert (xs, n: int) = 
    match xs with
    | [] -> [n]
    | head :: _ when head >= n -> n :: xs
    | head :: tail -> head :: insert (tail, n) 
    

// 40.3.2
let rec delete (n: int, xs) = 
    match xs with
    | [] -> []
    | head :: tail when head = n -> tail
    | head :: tail -> head :: delete (n, tail) 
    

// 40.2.3
let rec intersect (xs1, xs2) =
    match xs1 with
    | [] -> []
    | head :: tail when List.contains head xs2 -> head :: intersect (tail, delete (head, xs2))
    | _ :: tail -> intersect (tail, xs2)
    

// without buildins:
// let rec intersect (xs1, xs2) =
//     match (xs1, xs2) with
    // | ([],_) | (_,[]) -> []
    // | (h1::t1, h2::t2) when h1 = h2 -> h1 :: intersect (t1, t2)
    // | (h1::t1, h2::t2) when h1 < h2 -> intersect (t1, xs2)
    // | (h1::t1, h2::t2) -> intersect (xs1, t2)


// 40.2.4
let rec plus (xs1: int list, xs2) =
    match (xs1, xs2) with
    | ([], []) -> []
    | (xs1, []) -> xs1
    | ([], xs2) -> xs2
    | (head :: tail, head2 :: tail2) when head < head2 -> head :: plus (tail, head2 :: tail2)
    | (head :: tail, head2 :: tail2) -> head2 :: plus (head :: tail, tail2)
    
    
// 40.2.5
let rec minus (xs1, xs2) =
    match xs2 with
    | [] -> xs1
    | head :: tail when List.contains head xs1 -> minus (delete (head, xs1), tail)
    | _ :: tail -> minus (xs1, tail)
    

// 40.3.1
let rec smallest list = 
    let rec inner = function
        | (result, []) -> Some result
        | (result, head :: tail) when head < result -> inner (head, tail)
        | (result, _ :: tail) -> inner (result, tail)
        
    match list with
    | [] -> Some 0
    | head :: tail -> inner (head, tail)


// 40.3.3
let rec sort = function
    | [] -> []
    | list -> Option.get (smallest list) :: ((Option.get (smallest list), list) |> delete |> sort)


// with nesting function:
// let rec sort = function
//     | [] -> []
//     | xs -> let s = Option.get (smallest xs)
//             s :: sort (delete (s, xs))


// 40.4
let rec revrev = function
    | [] -> []
    | head :: tail -> revrev tail @ [List.rev head]
   