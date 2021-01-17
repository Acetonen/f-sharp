module Nine

let copperInGold = 20 * 12
let copperInSilver = 12

let sortCopper =
    fun total -> 
        (total / copperInGold, 
         total % copperInGold / copperInSilver, 
         total % copperInGold % copperInSilver)

let toCopper (a, b, c) = a * copperInGold + b * copperInSilver + c

// 23.4.1
let (.+.) x y = toCopper x + toCopper y |> sortCopper

let (.-.) x y = toCopper x - toCopper y |> sortCopper

(* 
In functional style:

let rec iter = function
    | (a, b, c) when c > 11 -> iter (a, b + (c / 12), c % 12)
    | (a, b, c) when b > 19 -> iter (a + (b / 20), b % 20, c)
    | (a, b, c) when c < 0 && b > 0 -> iter (a, b - 1, c + 12)
    | (a, b, c) when b < 0 && a > 0 -> iter (a - 1, b + 20, c)
    | (a, b, c) when c < 0 && a > 0 -> iter (a - 1, b + 19, c + 12)
    | (a, b, c) -> (a, b, c)

let (.+.) (a, b, c) (x, y, z) = iter (a + x, b + y, c + z)
let (.-.) (a, b, c) (x, y, z) = iter (a - x, b - y, c - z)
*)

// 23.4.2
let (.+) x y = 
    let a, b = x
    let c, d = y
    (a + c, b + d)

let (.-) x y = 
    let a, b = y
    x .+ (-a, -b)
    
let (.*) x y =
    let a, b = x
    let c, d = y
    (a * c - b * d, b * c + a * d)

let (./) x y =
    let a, b = y
    x .* (a / (a * a + b * b), -b / (a * a + b * b))