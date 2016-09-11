#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type Component = {subcomponents: string list}
type SingleField = {components: Component list}
type Field = Repetitions of SingleField list | SingleField of SingleField
type Segment = { name:string; fields:Field list; }
type Hl7Message = { segments:Segment list }

let hl7Seps = "|&~^"
let hl7 = "MSH|^~\&|A|B|C
EVN|P03|1^2^3||
PID|1||d2~e2~f2"

// i think the parser sepBy1 will help here -> it only succeeds if there is at least one one item seperated
let normalChar = satisfy (fun c-> c <> '\\')

let unescape c = match c with
                | 'F' -> '|'
                | 'R' -> '~'
                | 'S' -> '^'
                | 'T' -> '&'
                | 'E' -> '\\'
                | c -> c

let escapedChar = attempt (pchar '\\' >>. anyChar |>> unescape .>> skipChar '\\') <|> pchar '\\'

let pHl7Element = manyCharsTill (normalChar <|> escapedChar) (anyOf hl7Seps)

let pcomp = sepBy pHl7Element (pchar '&') |>> (fun vals -> {subcomponents = vals})
let pfield = sepBy pcomp (pchar '^') |>> (fun comps -> {components = comps})
let pRepsOrField = sepBy pfield (pchar '~') |>> (fun fields -> if fields.Length > 1 then Repetitions fields else SingleField (fields.Item 0))
let pheader = anyString 3 |>> (fun name -> name)
let pSegment = pipe2 pheader (sepBy pRepsOrField (pchar '|')) (fun name repsOrFields -> {name = name; fields = repsOrFields})

test pRepsOrField "ABCD^1234^A&B&C&D"
test pSegment "EVN|P03|1^2^3||"


//let pfield = sepBy pcomp (pstring "^") |>> (fun c -> List.map c (fun vals -> match vals with 
//                                                                                | Subcomponents -> 
//                                                        
//let pHeader = anyString 3
//let pSegment = sepBy pField (pstring "|")
//let pMsg = pipe2 pHeader pSegment (fun name fields -> {name = name; fields = fields})

//test pHeader "MSH"
//test pComponent "abcd&cdef"
//test pField "^a&b^c"
//test pMsg "PID|a|b^c&d"