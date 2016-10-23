namespace Hl7

module Printing = 
    let hl7tostring f c = f |> Seq.map (sprintf "%O") |> String.concat (c.ToString())

module Data = 
    [<Literal>]
    let standardHl7Seps = "|^&~\\"
    [<Literal>]
    let standardSubSep = '&'

    let internal seps(sepStr:string) = 
        (sepStr.[0], sepStr.[1], sepStr.[2], sepStr.[3], sepStr.[4])

    let standardSeps = seps standardHl7Seps

    type Subcomponent = {value:string; index:int}
    with override this.ToString() = this.value

    type Component = {subcomponents:Subcomponent list; index:int;}
    //with override this.ToString() = Printing.hl7tostring this.subcomponents

    type Field = {components:Component list; index:int; seperator:char}
    //with override this.ToString() = Printing.hl7tostring this.components this.seperator

    type Repetitions = {repetitions:Component list list; index:int; seperator:char}
    //with override this.ToString() = Printing.hl7tostring this.repetitions this.seperator

    type FieldOrRepetitions = Field of Field | Repetitions of Repetitions
    //with override this.ToString() = match this with
                                        //| Field(this) -> this.ToString()
                                        //| Repetitions(this) -> this.ToString()

    type Segment = {name:string; fields:FieldOrRepetitions list; children:Segment list; seperator:char}
    //with override this.ToString() = sprintf "%s|%s" this.name (Printing.hl7tostring this.fields this.seperator)

    type Hl7Message = {segments: Segment list}