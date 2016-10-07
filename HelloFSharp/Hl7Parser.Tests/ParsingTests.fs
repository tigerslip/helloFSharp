module ParsingHeaderSegment

open NUnit.Framework
open Hl7

let sampleMsh = "MSH|^~\\&|MM^ModernizingMedicine\r\nPID|A|B|C"

let msg = Parser.Parse sampleMsh
let msh = msg.segments.[0]
let pid = msg.segments.[1]

[<Test>]
let ``first delimiter is in field 0`` ()=
    Assert.AreEqual("|", msh.fields.[0].ToString())

[<Test>]
let ``next 4 delimiters are in field 1`` ()=
    Assert.AreEqual("^~\\&", msh.fields.[1].ToString())

[<Test>]
let ``data starts in field 2`` ()=
    Assert.AreEqual("MM^ModernizingMedicine", msh.fields.[2].ToString())

[<Test>]
let ``pid date is correct`` ()=
    Assert.AreEqual("A", pid.fields.[0].ToString())
    Assert.AreEqual("B", pid.fields.[1].ToString())
    Assert.AreEqual("C", pid.fields.[2].ToString())

[<Test>]
[<TestCase("MSH|~^&\\", '|', '~', '^', '&', '\\')>]
[<TestCase("MSH[-'!#", '[', '-', ''', '!', '#')>]
let ``parse message seperators`` hl7 field rep comp subcomp esc =
    let seps = Parser.GetSeperators hl7
    Assert.AreEqual(field, seps.[0])
    Assert.AreEqual(rep, seps.[1])
    Assert.AreEqual(comp, seps.[2])
    Assert.AreEqual(subcomp, seps.[3])
    Assert.AreEqual(esc, seps.[4])