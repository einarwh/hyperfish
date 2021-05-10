module Handler

open Box
open Lens
open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.GiraffeViewEngine
open Reform
open Styling
open Fishier

let render (s : string) : HttpHandler = 
    //(bounds, background, shapes) 
    let bounds = (600, 600)
    let background = White
    let picture = createLensPicture fishShapes
    let box = { a = { x = 100.; y = 100. }
                b = { x = 400.; y = 0. }
                c = { x = 0.; y = 400. } }
    let lens = (box, Blackish)
    let shapes = picture lens
    let v = view (bounds, background, shapes)
    let result = renderHtmlDocument v
    htmlString result

let escherHandler (matches : string seq) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        match matches |> Seq.tail |> Seq.tryExactlyOne with 
        | Some thing -> (render thing) next ctx
        | None -> (text "nothing" next ctx)
