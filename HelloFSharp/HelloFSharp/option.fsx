// it's good to have options

open System

let firstOdd = List.tryPick (fun x -> if x % 2 = 1 then Some x else None)

firstOdd[2;4;6]
firstOdd [2;4;5;6;7]