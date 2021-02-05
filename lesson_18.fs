module Eighteen

// 47.4.1
let f n =
    let x = ref 1
    let result = ref 1
    while ! x < n + 1 do
        result := ! result * ! x
        x := ! x + 1
    ! result

// 47.4.2
let fibo n = 
    let previous = ref 0
    let last = ref 1
    let count = ref 0

    while ! count < n do
        last := ! last + ! previous
        previous := ! last - ! previous
        count := ! count + 1

    if n = 0 then 0 else ! last