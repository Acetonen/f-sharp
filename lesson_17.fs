module Seventeen


// 43.3
let try_find key m = 
    let rec findInTuples = function
        | [] -> None
        | (head, value) :: _ when head = key -> Some(value)
        |  _ :: tail -> findInTuples tail
    
    m |> Map.toList |> findInTuples