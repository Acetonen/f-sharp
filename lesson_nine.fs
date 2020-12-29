let copperInGold = 20 * 12
let copperInSilver = 12

let sortCopper total =
    let gold = total / copperInGold
    let silver = (total - (gold * copperInGold)) / copperInSilver
    let copper = total - (gold * copperInGold) - (silver * copperInSilver)
    (gold, silver, copper)

let toCopper x =
    let (a, b, c) = x
    a * copperInGold + b * copperInSilver + c

// 23.4.1
let (.+.) x y = toCopper x + toCopper y |> sortCopper

let (.-.) x y = toCopper x - toCopper y |> sortCopper

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