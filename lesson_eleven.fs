type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) x y = 
    match x, y with
        | x, y when x.f > y.f -> true
        | x, y when x.f < y.f -> false
        | _ -> x > y
