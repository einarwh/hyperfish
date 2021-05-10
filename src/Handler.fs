module Handler

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.GiraffeViewEngine
open Box
open Lens
open Reform
open Shade
open Styling
open Fishier
open Letters
open Figures

let choosePicture (name : string) : Picture = 
    let shapes = 
        match name with 
        | "/fish" -> fishShapes 
        | "/george" -> georgeShapes
        | "/f-letter" -> fLetter
        | "/h-letter" -> hLetter
        | "/e-letter" -> eLetter
        | "/n-letter" -> nLetter
        | "/d-letter" -> dLetter
        | "/r-letter" -> rLetter
        | "/s-letter" -> sLetter
        | "/o-letter" -> oLetter
        | _ -> fishShapes
    createLensPicture shapes

let render (s : string) : HttpHandler = 
    //(bounds, background, shapes) 
    let bounds = (600, 600)
    let background = Grey
    let picture = choosePicture s
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
