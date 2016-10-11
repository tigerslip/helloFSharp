module ParsingHl7BasicElement

open NUnit.Framework
open Hl7
open Hl7.Data

let hl7 = "\\F\\\\R\\\\S\\\\T\\\\E\\"

[<Test>]
let ``standard seperators are escaped`` ()= 
    Assert.AreEqual("|^&~\\", Parser.ParseElement hl7)