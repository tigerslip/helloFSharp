module ParsingHeaderSegment

open NUnit.Framework
open Hl7
open Hl7.Data

let sampleMsh = "MSH|^~\\&|MM^ModernizingMedicine|B\r\nPID|A|B|C"

let msg = Parser.Parse sampleMsh
let msh = msg.segments.[0]
let pid = msg.segments.[1]

let assertField field =
    match field with
     | Field{components = c; index = i; seperator = s} -> c
     | _ -> raise (System.Exception("expected a field"))

[<Test>]
let ``first delimiter is in field 0`` ()=
    Assert.AreEqual("|", msh.fields.[0].ToString())

[<Test>]
let ``next 4 delimiters are in field 1`` ()=
    Assert.AreEqual("^~\\&", msh.fields.[1].ToString())

[<Test>]
let ``is pid 1 a field?`` ()=
    let f = assertField pid.fields.[0]
    Assert.AreEqual("A", (f.Item 0).ToString())

[<Test>]
let ``data starts in field 2`` ()=
    let b = assertField msh.fields.[3]
    Assert.AreEqual("B", (b.Item 0).ToString())
    let m = assertField msh.fields.[2]
    Assert.AreEqual("MM", m.Item 0)
    Assert.AreEqual("ModernizingMedicine", m.Item 1)
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
    let seps = Parser.ParseSeperators hl7
    Assert.AreEqual(field, seps.[0])
    Assert.AreEqual(rep, seps.[1])
    Assert.AreEqual(comp, seps.[2])
    Assert.AreEqual(subcomp, seps.[3])
    Assert.AreEqual(esc, seps.[4])