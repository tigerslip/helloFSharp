#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type result = {value:string; index:int}
type otherThing = {results:result list; index:int}

let delims = "|^"

let pipeandcarrotdata = "|A^B^C||B|C"

let zipi f = List.mapi f
let clean f = List.filter f
let zipCarrots = zipi (fun i t -> {value = t; index = i})

let cleanlist predicate = List.filter (fun i -> predicate i)
let cleanCarrots = cleanlist (fun t -> t.value <> "")

let collect items = List.mapi (fun i item -> {value = item; index = i}) items
let cleanEmpties items = List.filter (fun item -> item.value <> "") items

let pCarrots = sepBy (manyChars (noneOf delims)) (pstring "^") |>> (zipCarrots >> cleanCarrots)
let pPipes = 
    let zipPipes i p = {results = p; index = i}
    let isEmpty p = p.results.IsEmpty <> true
    sepBy pCarrots (pstring "|") |>> (List.mapi zipPipes >> List.filter isEmpty)

test pPipes pipeandcarrotdata