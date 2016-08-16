#r @"..\packages\Suave.0.33.0\lib\net40\Suave.dll"

open Suave
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Web

let app = 
    choose
        [ GET >>= choose
            [ path "/hello" >>= OK "Hello GET"
              path "/goodbye" >>= OK "Good bye GET" ]
        ]

startWebServer defaultConfig app