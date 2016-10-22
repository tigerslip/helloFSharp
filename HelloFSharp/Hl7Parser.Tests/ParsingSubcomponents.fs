module ParsingSubcomponents

open NUnit.Framework
open Hl7
open Hl7.Data

[<Test>]
let ``parse simple string`` ()=
    let sub = Hl7.Subcomponents.Parser.ParseElement "thing"
    Assert.AreEqual("thing", sub)

[<Test>]
let ``parse string with escape chars`` ()=
    let sub = Hl7.Subcomponents.Parser.ParseElement @"\F\ \R\ \S\ \T\ \E\"
    Assert.AreEqual(@"| ~ ^ & \", sub)

[<Test>]
let ``parse subcomponents`` () =
    let expected = [{value="A"; index=0}; {value="B"; index=1}]
    let result = Hl7.Subcomponents.Parser.Parse "A&B"
    CollectionAssert.AreEqual(expected, result)

[<Test>]
let ``parse subcomponents starting with empty element`` () = 
    let expected = [{value="A"; index=1}; {value="B"; index=2}]
    let result = Hl7.Subcomponents.Parser.Parse "&A&B"
    CollectionAssert.AreEqual(expected, result)

[<Test>]
let ``parse subcomponents with seperators`` ()= 
    let huh = "A \\ ^ &"
    let result = Hl7.Subcomponents.Parser.ParseElement @"A \E\ \S\ \T\"
    Assert.AreEqual(huh, result)
    
    let expected = [{value="A | ~ B"; index = 2}; {value="A \\ ^ &"; index = 4}]
    let result = Hl7.Subcomponents.Parser.Parse @"&&A \F\ \S\ B&&A \E\ \S\ \T\"
    Assert.AreEqual(expected.Head, result.Head)
    Assert.AreEqual(expected.Item 1, result.Item 1)
    CollectionAssert.AreEqual(expected, result)