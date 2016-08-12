// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
#load "..\..\..\Packages\FSharp.Charting.0.90.14\FSharp.Charting.fsx"
open FSharp.Charting

open NokiaComposer

let generateSamples milliseconds frequency = 
    let sampleRate = 44100.
    let sixteenBitSampleLimit = 32767.
    let volume = 0.8

    let toAmplitude x = 
        x 
        |> (*) (2. * System.Math.PI * frequency / sampleRate)
        |> sin
        |> (*) sixteenBitSampleLimit
        |> (*) volume
        |> int16

    let numOfSamples = milliseconds / 1000. * sampleRate
    let requiredSamples = seq { 1.0..numOfSamples}
    Seq.map toAmplitude requiredSamples

let points = generateSamples 150. 440.
points |> Chart.Line