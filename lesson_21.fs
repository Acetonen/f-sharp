module TwentyOne

// 49.5.2
let fac_seq n = seq {
    let current = ref 1
    yield !current
    for i in 1..n do
        current := !current * i
        yield !current
}

// 49.5.3
let seq_seq n = seq {
    for i in 0..n do
        if i % 2 = 0 
        then yield i / 2 
        else yield (-1) * (i + 1)/2
}