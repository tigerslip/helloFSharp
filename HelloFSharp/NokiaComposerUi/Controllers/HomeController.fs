namespace NokiaComposerUi.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax
open System.Net.Mime
open Assembler

type HomeController() =
    inherit Controller()
    member this.Index () = this.View()

    [<HttpPost>]
    member this.Produce(score:string) =
        match Assembler.assembleToPackedStream score with
            | Choice2Of2 ms -> 
                this.Response.AppendHeader("content-disposition", @"attachment;filename=""ringring.wav""")
                ms.Position <- 0L
                this.File(ms, "audi/x-wav")
            | Choice1Of2 err -> failwith err