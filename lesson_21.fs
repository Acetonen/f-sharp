module TwentyOne

// 50.2.1
let fac_seq = fun n -> seq {
    let current = ref 1
    yield !current
    for i in 1..n do
        current := !current * i
        yield !current
}

// 50.2.2
let seq_seq = fun n -> seq {
    for i in 0..n do
        if i % 2 = 0 
        then yield i / 2 
        else yield (-1) * (i + 1)/2
}