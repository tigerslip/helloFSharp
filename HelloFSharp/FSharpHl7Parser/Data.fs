namespace Hl7.Data

module private Printing = 
    let hl7tostring f c = f |> Seq.map (sprintf "%O") |> String.concat (c.ToString())

type Subcomponent = {value:string; index:int}
    with override this.ToString() = this.value

type Component = {subcomponents:Subcomponent list; index:int; seperator:char}
    with override this.ToString() = Printing.hl7tostring this.subcomponents this.seperator

type Field = {components:Component list; index:int; seperator:char}
    with override this.ToString() = Printing.hl7tostring this.components this.seperator

type Repetitions = {repetitions:Component list list; index:int; seperator:char}
    with override this.ToString() = Printing.hl7tostring this.repetitions this.seperator

type FieldOrRepetitions = Field of Field | Repetitions of Repetitions
    with override this.ToString() = match this with
                                        | Field(this) -> this.ToString()
                                        | Repetitions(this) -> this.ToString()

type Segment = {name:string; fields:FieldOrRepetitions list; children:Segment list; seperator:char}
    with override this.ToString() = sprintf "%s|%s" this.name (Printing.hl7tostring this.fields this.seperator)

type Hl7Message = {segments: Segment list}