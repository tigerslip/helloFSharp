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

        let psubs = 
            let newSub i s = {value = s; index = i}
            let notEmpty s = s.value.Length > 0
            sepBy pHl7Element (pchar subSep)  |>> (List.mapi newSub >> List.filter notEmpty)

        let pcomps = 
            let newComp i s = {subcomponents = s; index = i; seperator = subSep}
            let emptyComp c =  List.isEmpty c.subcomponents <> true
            sepBy psubs (pchar compSep) |>> (List.mapi newComp >> List.filter emptyComp)

        let pReps = sepBy pcomps (pchar repSep)

        let pRepsOrFields = 
            let zipRepsOrFields i f = match f with
                                        | [c] -> Field({components = c; index = i; seperator = compSep})
                                        | _ -> Repetitions({repetitions = f; index = i; seperator = repSep})
            sepBy pReps (pchar fieldSep)  |>> List.mapi zipRepsOrFields
        
        let pseg = 
            let pname = anyString 3 |>> id
            let zipSeg n f = {name = n; fields = f; children = List.empty; seperator = fieldSep}
            pipe2 pname ((pchar fieldSep) >>. pRepsOrFields) zipSeg

        sepBy pseg newline |>> (fun segments -> {segments = segments})

    let private parserInit hl7 = 
        let pmsgheader = (pstring "MSH") <|> (pstring "FHS")
        let pseps = (anyString 5)
        lookAhead (pmsgheader >>. pseps) >>= pmsg

    let Parse hl7 = match run (parserInit hl7) hl7 with
                        | Success(result, _, _) -> result
                        | Failure(errorMsg, _, _) -> raise(System.Exception(errorMsg))