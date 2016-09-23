module Hl7Parser

open FParsec

type Subcomponent = {value:string; index:int}
type Component = {subcomponents:Subcomponent list; index:int}
type Field = {components:Component list}
type Repetitions = {repetitions:Component list list}
type FieldOrRepetitions = Field of Field * index:int | Repetitions of Repetitions * index:int
type Segment = {name:string; fields:FieldOrRepetitions list; children:Segment list}
type Hl7Message = {segments: Segment list}

type Hl7Parser(hl7Seps:string) =
    
    do (if(hl7Seps.Length <> 5) then raise (System.Exception("Hl7 seperators must be exactly 5 characters.")))

    let fieldSep = hl7Seps.[0]
    let repSep = hl7Seps.[1]
    let compSep = hl7Seps.[2]
    let subSep = hl7Seps.[3]
    let escChar = hl7Seps.[4]

    let normalChar = noneOf (hl7Seps + "\r\n")
    let unescape c = match c with
                        | 'F' -> fieldSep
                        | 'R' -> repSep
                        | 'S' -> compSep
                        | 'T' -> subSep
                        | 'E' -> escChar
                        | c -> c

    let escapedChar = attempt (pchar escChar >>. anyChar |>> unescape .>> skipChar escChar) <|> pchar escChar
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

    let pHl7Part s p zipclean = sepBy p (pchar s) |>> zipclean
    let psubs = pHl7Part subSep pHl7Element (zipSubs >> cleanSubs)
    let pcomps = pHl7Part compSep psubs (zipComps >> cleanComps)
    let pReps = sepBy pcomps (pchar repSep)
    let pRepsOrFields = sepBy pReps (pchar fieldSep) |>> zipRepsOrFields

    let pname = anyString 3 |>> id
    let pseg = pipe2 pname pRepsOrFields (fun name fields -> {name = name; fields = fields; children = List.empty})
    let pmsg = sepBy pseg newline |>> (fun segments -> {segments = segments})

    new() = Hl7Parser("|^~\\&")

    member this.Parse hl7 = match run pmsg hl7 with
                            | Success(result, _, _) -> result
                            | Failure(errorMsg, _, _) -> raise(System.Exception(errorMsg))