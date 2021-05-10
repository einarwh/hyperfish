module Routing

    open Microsoft.AspNetCore.Http
    open Giraffe
    open System.Text.RegularExpressions
    
    let routexp (path : string) (routeHandler : seq<string> -> HttpHandler): HttpHandler =
        let pattern = sprintf "^%s$" path
        let regex   = Regex(pattern, RegexOptions.Compiled)

        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            let result  = regex.Match (SubRouting.getNextPartOfPath ctx)
            match result.Success with
            | true  ->
                let args = result.Groups |> Seq.map (fun x -> x.Value)
                routeHandler args next ctx
            | false -> skipPipeline
