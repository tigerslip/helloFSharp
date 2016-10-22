namespace Hl7

open Hl7.Data
open Hl7.Subcomponents.Parser
open FParsec
open RunParsers

module Parser = 

    let private pmsg(hl7Seps:string) = 
        let fsep,csep,ssep,rsep,esc = (seps hl7Seps)

        let zipComp i s = {subcomponents = s; index = i; seperator = ssep}

        let pcomps = 
            let emptyComp c =  List.isEmpty c.subcomponents <> true
            sepBy (psubs (seps hl7Seps)) (pchar csep) |>> (List.mapi zipComp >> List.filter emptyComp)

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

    let ParseSeperators hl7 = runParser pSeps hl7

    let Parse hl7 = runParser parserInit hl7