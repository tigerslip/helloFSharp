#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

// this is a little helper operator to let you see the entering / exiting of diff parsers - writes to console
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

type Subcomponent = {value:string; position:int}
type Component = {subcomponents: Subcomponent list; position:int}
type SingleField = {components: Component list; position:int}
type Field = Repetitions of SingleField list | SingleField of SingleField
type Segment = { name:string; fields:Field list; }
type Hl7Message = { segments:Segment list }

let hl7Seps = "|^~\\&"

let normalChar = noneOf hl7Seps

let unescape c = match c with
                | 'F' -> '|'
                | 'R' -> '~'
                | 'S' -> '^'
                | 'T' -> '&'
                | 'E' -> '\\'
                | c -> c

let escapedChar = attempt (pchar '\\' >>. anyChar |>> unescape .>> skipChar '\\') <|> pchar '\\'

let pHl7Element = manyChars (normalChar <|> escapedChar) <!> "pelement"

let pcomp = sepBy pHl7Element (pchar '&') |>> (fun vals -> List.mapi (fun i s -> {value = s; position = i}) vals) <!> "pcomp"

let pfield = sepBy pcomp (pchar '^') |>> (fun comps -> List.mapi (fun i c -> {subcomponents = c; position = i}) comps) <!> "pfield"

let pRepsOrField = sepBy pfield (pchar '~') 
                    |>> (fun fields -> match fields.Length with
                                        | 0 | 1 -> SingleField {components = fields.Item 0; position = 0}
                                        | _ -> Repetitions (List.mapi (fun i c -> {components = c; position = i}) fields)) <!> "prepsorfields"


let pheader = anyString 3 |>> (fun name -> name) <!> "pheader"

let pSegment = pipe2 pheader (sepBy pRepsOrField (pchar '|')) (fun name repsOrFields -> {name = name; fields = repsOrFields}) <!> "psegment"

test pSegment "EVN|A&1^B^C|123~456~789"

let hl7 = "MSH|^~\\&|A|B|C\nEVN|P03|1^2^3&4&5||\nPID|1||d2~e2~f2"


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