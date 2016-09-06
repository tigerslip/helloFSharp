#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type Subcomponent = { value:string; }
type Component = { subcomponents:Subcomponent list; }
type Field = { components:Component list; }
type Segment = { name:string; fields: Field list; }
type Hl7Message = { segments:Segment list }

let hl7Seps = "|&~^"
let hl7 = "MSH|^~\&|A|B|C
EVN|P03|1^2^3||
PID|1||d2~e2~f2"

let pSubcomponent = manyChars (noneOf "&^|") |>> (fun res -> {value = res})
let pComponent = sepBy pSubcomponent (pstring "&") |>> (fun s -> {subcomponents = s})
let pField = sepBy pComponent (pstring "^") |>> (fun c -> {components = c})
let pHeader = anyString 3
let pSegment = sepBy pField (pstring "|")

let pMsg = pipe2 pHeader pSegment (fun name fields -> {name = name; fields = fields})

test pHeader "MSH"
test pComponent "abcd&cdef"
test pField "^a&b^c"
test pMsg "PID|a|b^c&d"