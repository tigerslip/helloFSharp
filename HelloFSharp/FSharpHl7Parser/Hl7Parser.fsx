#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type Subcomponent = {value:string; index:int}
type Component = {subcomponents:Subcomponent list; index:int}
type Field = {components:Component list}
type Repetitions = {repetitions:Component list list}
type FieldOrRepetitions = 
    | Field of Field * index:int 
    | Repetitions of Repetitions * index:int
type Segment = {name:string; fields:FieldOrRepetitions list; children:Segment list}
type Hl7Message = {segments: Segment list}

let hl7Seps = "|^~\\&\r\n"

let normalChar = noneOf hl7Seps
let unescape c = match c with
                    | 'F' -> '|'
                    | 'R' -> '~'
                    | 'S' -> '^'
                    | 'T' -> '&'
                    | 'E' -> '\\'
                    | c -> c

let escapedChar = attempt (pchar '\\' >>. anyChar |>> unescape .>> skipChar '\\') <|> pchar '\\'
let pHl7Element = manyChars (normalChar <|> escapedChar)

let zipSubs = List.mapi (fun i s -> {value = s; index = i})
let zipComps = List.mapi (fun i s -> {subcomponents = s; index = i})
let zipRepsOrFields = List.mapi (fun i fields -> match fields with
                                                    | [f] -> Field({components = fields.Item 0}, i)
                                                    | _ -> Repetitions({repetitions = fields}, i))

let cleanSubs = List.filter (fun t -> t.value <> "")
let cleanlist selector = List.filter (fun l -> (List.isEmpty (selector l)) <> true)
let cleanComps = cleanlist (fun c -> c.subcomponents)
let cleanFields = cleanlist (fun f -> f.components)

let pHl7Part s p zipclean = sepBy p (pstring s) |>> zipclean
let psubs = pHl7Part "&" pHl7Element (zipSubs >> cleanSubs)
let pcomps = pHl7Part "^" psubs (zipComps >> cleanComps)
let pReps = sepBy pcomps (pstring "~")
let pRepsOrFields = sepBy pReps (pstring "|") |>> zipRepsOrFields

let pname = anyString 3 |>> id
let pseg = pipe2 pname pRepsOrFields (fun name fields -> {name = name; fields = fields; children = List.empty})
let pmsg = sepBy pseg newline |>> (fun segments -> {segments = segments})

test pmsg "EVN|A&1^B^C|123~456~789\r\nPID|A|B|C"