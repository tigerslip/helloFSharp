namespace Hl7.Subcomponents

open FParsec
open Hl7.Data
open RunParsers

module Parser =
    let standardSeps = "|^&~\\"
    
    let seps(sepStr:string) = 
        (sepStr.[0], sepStr.[1], sepStr.[2], sepStr.[3], sepStr.[4])
    
    let internal pHl7Element seps = 
            let fieldSep,compSep,subComp,repSep,esc = seps
            let normalChar = noneOf (sprintf "%c%c%c%c%c\r\n" fieldSep compSep subComp repSep esc)
            let unescape c = match c with
                                | 'F' -> fieldSep
                                | 'R' -> repSep
                                | 'S' -> compSep
                                | 'T' -> subComp
                                | 'E' -> esc
                                | c -> c

            let escapedChar = attempt (pchar esc >>. anyChar |>> unescape .>> skipChar esc) <|> pchar esc
            manyChars (normalChar <|> escapedChar)

    let internal zipSub i s = {value = s; index = i}

    let internal psubs seps = 
        let _,_,ssep,_,_ = seps
        let notEmpty s = s.value.Length > 0
        sepBy (pHl7Element seps) (pchar ssep)
            |>> (List.mapi zipSub >> List.filter notEmpty)

    let ParseElementWithSeperators hl7 sepsStr = runParser (pHl7Element (seps sepsStr)) hl7

    let ParseElement hl7 = ParseElementWithSeperators hl7 standardSeps

    let Parse hl7 = runParser (psubs (seps standardSeps)) hl7