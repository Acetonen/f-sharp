module Seven

// 20.3.1
let vat n x = x * (float(n) / 100.00 + 1.0)

// 20.3.2
let unvat n x = x / (float(n) + 100.0) * 100.0

// 20.3.3
let rec min f = 
    let rec inner = function
        | (f, n) when f n <> 0 -> inner (f, n + 1)
        | (f, n) -> n

    inner (f, 1)