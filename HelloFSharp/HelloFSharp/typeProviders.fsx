#r @"..\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
open FSharp.Data
open System

let dudes = CsvProvider<"test.csv", Schema = "Name,Age,Weight">.GetSample()

let dudesOverthirty = dudes.Rows |> Seq.filter (fun r -> r.Age > 30)
dudesOverthirty