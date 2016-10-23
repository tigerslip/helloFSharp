namespace Hl7.Subcomponents

open FParsec
open Hl7.Data
open RunParsers

module Parser =
    
    let internal pHl7Element seps = 
        let fieldSep,compSep,subComp,repSep,esc = seps
        let normalChar = 
            noneOf (sprintf "%c%c%c%c%c\r\n" fieldSep compSep subComp repSep esc)
        let unescape c = match c with
                            | 'F' -> fieldSep
                            | 'R' -> repSep
                            | 'S' -> compSep
                            | 'T' -> subComp
                            | 'E' -> esc
                            | c -> c
        let escapedChar = 
            attempt (pchar esc >>. anyChar |>> unescape .>> skipChar esc)
                <|> pchar esc
        manyChars (normalChar <|> escapedChar)

    let zipSub i s = {value = s; index = i}

    let internal psubs hl7Seps = 
        let hl7ElementParser = pHl7Element hl7Seps
        let _,_,ssep,_,_ = hl7Seps
        let notEmpty s = s.value.Length > 0
        sepBy hl7ElementParser (pchar ssep)
            |>> (List.mapi zipSub >> List.filter notEmpty)

    let ParseElementWithSeperators hl7 sepsStr = 
        runParser (pHl7Element (seps sepsStr)) hl7

    let ParseElement hl7 = 
        ParseElementWithSeperators hl7 standardHl7Seps

    let Parse hl7 = 
        runParser (psubs standardSeps) hl7