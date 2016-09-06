#r @"..\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

open FSharp.Data

type WorldBank = WorldBankDataProvider<"World Development Indicators">
let wb = WorldBank.GetDataContext()
wb.Countries.``Korea, Rep.``.CapitalCity