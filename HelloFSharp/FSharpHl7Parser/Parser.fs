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

        let zipSub i s = {value = s; index = i}

        let psubs = 
            let notEmpty s = s.value.Length > 0
            sepBy pHl7Element (pchar subSep)  |>> (List.mapi zipSub >> List.filter notEmpty)

        let zipComp i s = {subcomponents = s; index = i; seperator = subSep}

        let pcomps = 
            let emptyComp c =  List.isEmpty c.subcomponents <> true
            sepBy psubs (pchar compSep) |>> (List.mapi zipComp >> List.filter emptyComp)

        let pReps = sepBy pcomps (pchar repSep)

        let zipRepsOrFields i f = match f with
                                    | [c] -> Field({components = c; index = i; seperator = compSep})
                                    | _ -> Repetitions({repetitions = f; index = i; seperator = repSep})

        let pRepsOrFields = 
            sepBy pReps (pchar fieldSep)  |>> List.mapi zipRepsOrFields

        let pname = anyString 3 |>> id
        let zipSeg n f = {name = n; fields = f; children = List.empty; seperator = fieldSep}

        let pseg = 
            pipe2 pname ((pchar fieldSep) >>. pRepsOrFields) zipSeg

        let pheader = 
            let packField s i = Field({components = [zipComp 0 [zipSub 0 s]]; index = i; seperator = compSep})
            let packHeaderFields = [packField "|" 0; packField hl7Seps.[1..4] 1]
            let ziphead name fields = zipSeg name (List.append packHeaderFields fields)
            let phead = pname .>> (pstring (sprintf "%s|" hl7Seps))
            pipe2 phead pRepsOrFields ziphead

        let pSegs = sepBy pseg newline
        let packSegs h s = List.append [h] s
        let zipSegs segs = {segments = segs}
        pipe2 (pheader .>> newline) pSegs (fun a b -> zipSegs (packSegs a b))

    let private pSeps =
        let pmsgheader = (pstring "MSH") <|> (pstring "FHS")
        let pseps = (anyString 5)
        lookAhead  (pmsgheader >>. pseps) |>> id

    let private parserInit = pSeps >>= pmsg

    let private runParser p hl7 = match run p hl7 with
                                    | Success(result, _, _) -> result
                                    | Failure(errorMsg, _, _) -> raise(System.Exception(errorMsg))

    let GetSeperators hl7 = runParser pSeps hl7

    let Parse hl7 = runParser parserInit hl7