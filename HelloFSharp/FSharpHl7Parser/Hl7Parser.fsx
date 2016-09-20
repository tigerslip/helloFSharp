#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type Subcomponent = {value:string; index:int}
type Component = {subcomponents:Subcomponent list; index:int}
type Field = {components:Component list; index:int}
type Repetitions = {fields:Field list; index:int}
type FieldOrRepetitions = Field of Field | Repetitions of Repetitions
type Segment = {name:string; fields:FieldOrRepetitions list; children:Segment list; index:int}

let delims = "|^"

let pipeandcarrotdata = "|A^B^C||B|C"

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
let pHl7Element = manyChars (normalChar <|> escapedChar)

let zipi res = List.mapi (fun i item -> res i item)
let zipSubs = zipi (fun i s -> {value = s; index = i})
let zipComps = zipi (fun i s -> {subcomponents = s; index = i})
let zipFields = zipi (fun i c -> {components = c; index = i})
let zipReps = zipi (fun i f -> {fields=f; index=i})
let zipSegs = zipi (fun i f n c -> {name = n; fields = f; index = i; children = c}) 

let clean predicate = List.filter (fun i -> predicate i)
let cleanSubs = clean (fun t -> t.value <> "")
let cleanlist selector = clean (fun l -> (List.isEmpty (selector l)) <> true)
let cleanComps = cleanlist (fun c -> c.subcomponents)
let cleanFields = cleanlist (fun f -> f.components)
let cleanReps = cleanlist (fun r -> r.fields)

let pHl7Part s p zip clean = sepBy p (pstring s) |>> (zip >> clean)
let psubs = pHl7Part "&" pHl7Element zipSubs cleanSubs
let pcomps = pHl7Part "^" psubs zipComps cleanComps
let preps = pHl7Part "~" pcomps 
let pfields = pHl7Part "|" pcomps zipFields cleanFields


//test pSegment "EVN|A&1^B^C|123~456~789"