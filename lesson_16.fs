module Sixteen

// 42.3
let rec allSubsets arrayLength subsetSize =
    let rec subsets s = 
        set [
            if Set.count s = subsetSize 
                then yield s
            for e in s do
                yield! subsets (Set.remove e s) 
        ]
    subsets (set [1 .. arrayLength])
