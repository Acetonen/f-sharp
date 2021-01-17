module Five

// 16.1
let notDivisible (n, m) = m % n = 0

// 16.2
let prime n =
    let rec find = function
        | (n,1) | (n,0) -> true
        | (n,m) when n % m = 0 -> false
        | (n,m) -> find (n, m - 1)

    find (n, n / 2)