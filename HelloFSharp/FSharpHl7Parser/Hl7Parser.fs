module Hl7Parser

open FParsec

let hl7tostring f c = f |> Seq.map (sprintf "%O") |> String.concat (c.ToString())

type Subcomponent = {value:string; index:int}
    with override this.ToString() = this.value

type Component = {subcomponents:Subcomponent list; index:int; seperator:char}
    with override this.ToString() = hl7tostring this.subcomponents this.seperator

type Field = {components:Component list; index:int; seperator:char}
    with override this.ToString() = hl7tostring this.components this.seperator

type Repetitions = {repetitions:Component list list; index:int; seperator:char}
    with override this.ToString() = hl7tostring this.repetitions this.seperator

type FieldOrRepetitions = Field of Field | Repetitions of Repetitions
    with override this.ToString() = match this with
                                        | Field(this) -> this.ToString()
                                        | Repetitions(this) -> this.ToString() 

type Segment = {name:string; fields:FieldOrRepetitions list; children:Segment list; seperator:char}
    with override this.ToString() = this.name + (hl7tostring this.fields this.seperator)

type Hl7Message = {segments: Segment list}

let private pmsg(hl7Seps:string) = 
    
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
    let zipComps = List.mapi (fun i s -> {subcomponents = s; index = i; seperator = subSep})
    let zipRepsOrFields = List.mapi (fun i fields -> match fields with
                                                        | [f] -> Field({components = fields.Item 0; index = i; seperator = compSep})
                                                        | _ -> Repetitions({repetitions = fields; index = i; seperator = repSep}))

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
    let pseg = pipe2 pname pRepsOrFields (fun name fields -> {name = name; fields = fields; children = List.empty; seperator = fieldSep})
    sepBy pseg newline |>> (fun segments -> {segments = segments})

let private parserInit hl7 = 
    let pmsgheader = (pstring "MSH") <|> (pstring "FHS")
    let pseps = (anyString 5)
    lookAhead (pmsgheader >>. pseps) >>= pmsg

let Parse hl7 = match run (parserInit hl7) hl7 with
                    | Success(result, _, _) -> result
                    | Failure(errorMsg, _, _) -> raise(System.Exception(errorMsg))