namespace Hl7

open Hl7.Data
open FParsec

module Parser = 
    let private pmsg(hl7Seps:string) = 
    
        let fieldSep = hl7Seps.[0]
        let repSep = hl7Seps.[1]
        let compSep = hl7Seps.[2]
        let subSep = hl7Seps.[3]
        let escChar = hl7Seps.[4]

        let pHl7Element = 
            let normalChar = noneOf (sprintf "%s\r\n" hl7Seps)
            let unescape c = match c with
                                | 'F' -> fieldSep
                                | 'R' -> repSep
                                | 'S' -> compSep
                                | 'T' -> subSep
                                | 'E' -> escChar
                                | c -> c

            let escapedChar = attempt (pchar escChar >>. anyChar |>> unescape .>> skipChar escChar) <|> pchar escChar
            manyChars (normalChar <|> escapedChar)

        let pHl7Part c p = sepBy p (pchar c)
        let cleanlist selector = List.filter (fun l -> (List.isEmpty (selector l)) <> true)
    
        let psubs = 
            let zipSubs = List.mapi (fun i s -> {value = s; index = i})
            let cleanSubs = List.filter (fun t -> t.value.Length > 0)
            pHl7Part subSep pHl7Element |>> (zipSubs >> cleanSubs)

        let pcomps = 
            let zipComps = List.mapi (fun i s -> {subcomponents = s; index = i; seperator = subSep})
            let cleanComps = cleanlist (fun c -> c.subcomponents)
            pHl7Part compSep psubs |>> (zipComps >> cleanComps)

        let pReps = pHl7Part repSep pcomps

        let pRepsOrFields = 
            let zipRepsOrFields = 
                List.mapi (fun i fields -> 
                    match fields with
                    | [f] -> Field({components = fields.Item 0; index = i; seperator = compSep})
                    | _ -> Repetitions({repetitions = fields; index = i; seperator = repSep}))
            pHl7Part fieldSep pReps |>> zipRepsOrFields
        
        let pname = anyString 3 |>> id
        let pseg = pipe2 pname ((pchar fieldSep) >>. pRepsOrFields) (fun name fields -> {name = name; fields = fields; children = List.empty; seperator = fieldSep})
        sepBy pseg newline |>> (fun segments -> {segments = segments})

    let private parserInit hl7 = 
        let pmsgheader = (pstring "MSH") <|> (pstring "FHS")
        let pseps = (anyString 5)
        lookAhead (pmsgheader >>. pseps) >>= pmsg

    let Parse hl7 = match run (parserInit hl7) hl7 with
                        | Success(result, _, _) -> result
                        | Failure(errorMsg, _, _) -> raise(System.Exception(errorMsg))