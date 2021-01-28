module Fifteen

// 41.4.1
let list_filter f xs = List.foldBack (fun element result -> if f element then element :: result else result) xs []


// 41.4.2
let sum (p, xs) = List.foldBack (fun element result -> if p element then element + result else result) xs 0


let rev lst = List.fold (fun head tail -> tail :: head) [] lst

// 41.4.3
let revrev = fun lst -> List.fold (fun head tail -> (rev tail) :: head) [] lst