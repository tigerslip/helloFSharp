// simplest pattern match
let (a,b) = (1,2)

a
b

// simple pattern match
let addPair p = 
    match p with
    | (f, 0) -> f
    | (0, s) -> s
    | (f,s) -> f + s

addPair(0,2)

// fizzbuzzer
let fizzbuzzer i = 
    match i with
    | _ when i % 3 = 0 && i % 5 = 0 -> "fizzbuzz"
    | _ when i % 3 = 0 -> "fizz"
    | _ when i % 5 = 0 -> "buzz"
    | _ -> string i

[1..100] |> List.map fizzbuzzer

// pattern matching with c# try - out function
open System

let add x y = x + y

let testTryParse str = Int32.TryParse(str) |>  function
    | (_, 4) -> printfn "got 4!"
    | (true, value) -> add value 5 |> printfn "%i"
    | (false, _) -> printfn "could not parse"

testTryParse "4" // out got 4.14
testTryParse "3"    // out 3.00000
testTryParse "abc"  // out could not parse