module firstlib.tests.FirstTests

open NUnit.Framework
open Something

type MyFirstTests ()=

  [<Test>]
  member this.IsNotBroken ()=
    Assert.IsTrue true

  [<Test>]
  member this.IsBroken () =
    Assert.IsFalse true