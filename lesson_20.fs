module Twenty

// 49.5.1
let even_seq = Seq.initInfinite (fun i -> (i + 1) * 2)

let rec fac = function
    | 1 -> 1
    | n -> n * fac(n-1)

// 49.5.2
let fac_seq = Seq.initInfinite (fun i -> if i = 0 then 1 else fac i)

let seq n = 
    let rec inner = function
        | (current, count) when count = n -> current
        | (current, count) when current < 0 -> inner (current * -1, count + 1)
        | (current, count) -> inner ((current + 1) * -1, count + 1)
    
    inner (0, 0)

// 49.5.3
let seq_seq = Seq.initInfinite (fun i -> seq i)