namespace Hl7.Components

open FParsec
open Hl7.Data
open Hl7.Subcomponents.Parser
open RunParsers

module Parser = 
    let zipComp i s = {subcomponents = s; index = i}

    let pcomps hl7Seps = 
        let _,csep,_,_,_ = hl7Seps
        let emptyComp c =  List.isEmpty c.subcomponents <> true
        sepBy (psubs hl7Seps) (pchar csep) 
            |>> (List.mapi zipComp >> List.filter emptyComp)

    //let Parse = runParser (pcomps standardSeps 