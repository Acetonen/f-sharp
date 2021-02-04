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


// let allSubsets n k = 
//      let rec subsets xs= 
//          match xs with 
//            | [] -> [[]] 
//            | head::tail -> List.fold (fun head1 tail1 -> (head::tail1)::tail1::head1) [] (subsets tail) 
//      Set.filter (fun xs -> Set.count xs = k) (Set.ofList (List.foldBack (fun head tail -> (Set.ofList head) :: tail) (subsets [1..n]) [] )) 