module Twelve

// 34.1
let rec upto number = 
    let rec inner = function
        | (0, result) -> result
        | (current, result) -> inner (current - 1, current :: result)

    inner (number - 1, [number])

// 34.2
let rec dnto number = 
    let rec inner = function
        | (current, result) when current = number -> current :: result
        | (current, result) -> inner (current + 1, current :: result)

    inner (2, [1])

// 34.3
let rec evenn number = 
    let rec inner = function
        | (0, result) -> 0 :: result
        | (current, result) -> inner (current - 2, current - 2 :: result)

    inner (number * 2, [number * 2])