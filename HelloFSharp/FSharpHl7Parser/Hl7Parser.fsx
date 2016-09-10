#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type Component = Subcomponents of string list | Value of string
type Field = Repetitions of Component list | Values of string list
type Segment = { childSegments : Segment option; fields : Field list; }
type Hl7Message = { segments : Segment list }

let hl7Seps = "|&~^"
let hl7 = "MSH|^~\&|A|B|C
EVN|P03|1^2^3||
PID|1||d2~e2~f2"

let pval = manyChars (noneOf hl7Seps) |>> (fun res -> res)
let getcomp vals = (fun vals -> match vals.Length with 
                    | 0 -> {Value = ""}
                    | 1 -> {Value = vals.Item[0]}
                    | _ -> {Subcomponents = vals})

let pcomp = sepBy pval (pstring "&") |>> (fun vals -> getcomp vals)

let pfield = sepBy pcomp (pstring "^") |>> (fun r -> {Repitions = r}
let pHeader = anyString 3
let pSegment = sepBy pField (pstring "|")
let pMsg = pipe2 pHeader pSegment (fun name fields -> {name = name; fields = fields})

test pHeader "MSH"
test pComponent "abcd&cdef"
test pField "^a&b^c"
test pMsg "PID|a|b^c&d"