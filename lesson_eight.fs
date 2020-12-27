let curry f = 
    let g x =
        let h y = f (x, y)
        h
    g

    
    