#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type Subcomponent = {value:string; position:int}
type Component = {subcomponents: Subcomponent list; position:int}
type SimpleField = {components: Component list; position:int}
type RepetitionField = {fields: SimpleField list; position:int} 
type Field = Repetitions of RepetitionField | SimpleField of SimpleField
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

let pHl7Element = manyChars (normalChar <|> escapedChar)

test pHl7Element "1234"

let pSegName = anyString 3

let pComps = sepBy (pstring "^") pHl7Element |>> (fun c -> (c))

let pFields = sepBy (pstring "|") pComps |>> (fun fs -> (fs))

test pFields "|A&1^B^C|123~456~789"

test pSegName ""