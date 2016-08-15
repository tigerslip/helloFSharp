#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open FParsec

let test parser str =
    match run parser str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type Subcomponent = { value:string; }
type Component = { subcomponents:Subcomponent[]; }
type Field = { components:Component[]; }
type Segment = { name:string; fields: Field[]; segments: Segment[] }
type Hl7Message = { segments:Segment[] }

let hl7 = "MSH|^~\&|A|B|C
EVN|P03|1^2^3||
PID|1||d2~e2~f2"