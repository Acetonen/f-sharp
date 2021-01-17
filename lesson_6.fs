module Six

// 17.1
let rec pow = function
    | (m,0) -> ""
    | (m,1) -> string m
    | (m,n) -> m + pow (m, n - 1)

// 17.2
let isIthChar (s, n, c) = 
    match n with
    | n when n < 0 || n > String.length s -> false
    | n when s.[n] = c -> true
    | _ -> false

// 17.3
let rec occFromIth (s, n, c) = 
    let rec count = function
        | (sum,index) when index >= String.length s -> sum
        | (sum,index) when s.[index] = c -> count (sum+1, index+1)
        | (sum,index) when s.[index] <> c -> count (sum, index+1)
        | _ -> 0
    
    count (0, n)