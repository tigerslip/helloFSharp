namespace Hl7

open Hl7.Data
open FParsec

module Parser = 
    let private pHl7Element f r c s e = 
        let normalChar = noneOf (sprintf "%c%c%c%c%c\r\n" f r c s e)
        let unescape c = match c with
                            | 'F' -> f
                            | 'R' -> r
                            | 'S' -> c
                            | 'T' -> s
                            | 'E' -> e
                            | c -> c

        let escapedChar = attempt (pchar e >>. anyChar |>> unescape .>> skipChar e) <|> pchar e
        manyChars (normalChar <|> escapedChar)
    
    let private pmsg(hl7Seps:string) = 
    
        let fsep = hl7Seps.[0]
        let rsep = hl7Seps.[1]
        let csep = hl7Seps.[2]
        let ssep = hl7Seps.[3]
        let esc = hl7Seps.[4]

        let zipSub i s = {value = s; index = i}

        let psubs = 
            let notEmpty s = s.value.Length > 0
            sepBy (pHl7Element fsep rsep csep ssep esc) (pchar ssep)  |>> (List.mapi zipSub >> List.filter notEmpty)

        let zipComp i s = {subcomponents = s; index = i; seperator = ssep}

        let pcomps = 
            let emptyComp c =  List.isEmpty c.subcomponents <> true
            sepBy psubs (pchar csep) |>> (List.mapi zipComp >> List.filter emptyComp)

        let pReps = sepBy pcomps (pchar rsep)

        let zipRepsOrFields i f = match f with
                                    | [c] -> Field({components = c; index = i; seperator = csep})
                                    | _ -> Repetitions({repetitions = f; index = i; seperator = rsep})

        let pRepsOrFields = 
            sepBy pReps (pchar fsep)  |>> List.mapi zipRepsOrFields

        let pname = anyString 3 |>> id
        let zipSeg n f = {name = n; fields = f; children = List.empty; seperator = fsep}

        let pseg = 
            pipe2 pname ((pchar fsep) >>. pRepsOrFields) zipSeg

        let pheader = 
            let packField s i = Field({components = [zipComp 0 [zipSub 0 s]]; index = i; seperator = csep})
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

    let ParseSeperators hl7 = runParser pSeps hl7

    let ParseElementWith hl7 f c s r e = runParser (pHl7Element f c s r e) hl7

    let ParseElement hl7 = ParseElementWith hl7 '|' '^' '&' '~' '\\'

    let Parse hl7 = runParser parserInit hl7