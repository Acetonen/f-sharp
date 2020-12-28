let curry f = 
    let g x =
        let h y = f (x, y)
        h
    g

    
let uncurry g = 
    let f (x, y) =
        let h = g x
        h y
    f