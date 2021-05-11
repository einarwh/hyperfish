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
open Stack

exception TypeException of string

let tryLookupPicture (name : string) : Picture option = 
    let maybeShapes = 
        match name with 
        | "blank" -> Some []
        | "fish" -> Some fishShapes 
        | "george" -> Some georgeShapes
        | "f-letter" -> Some fLetter
        | "h-letter" -> Some hLetter
        | "e-letter" -> Some eLetter
        | "n-letter" -> Some nLetter
        | "d-letter" -> Some dLetter
        | "r-letter" -> Some rLetter
        | "s-letter" -> Some sLetter
        | "o-letter" -> Some oLetter
        | _ -> None
    maybeShapes |> Option.map (fun shapes -> createLensPicture shapes) 

let tryParsePictureValue (s : string) : StackValue option =
    s |> tryLookupPicture |> Option.map PictureValue 

let tryParseNumber (str : string) : int option =
    match System.Int32.TryParse str with
    | (true, n) -> Some n
    | _ -> None

let tryParseNumberValue (s : string) : StackValue option = 
    s |> tryParseNumber |> Option.map NumberValue

let transformCode (transform : Picture -> Picture) : Stack -> Stack = 
    fun stack ->    
        match stack with 
        | [] -> raise (StackUnderflowException)
        | PictureValue p :: restStack -> PictureValue (transform p) :: restStack
        | _ -> raise (TypeException "Expected a picture on the stack")

let tryLookupFunction (name : string) : Function option =
    match name with 
    | "turn" -> 
        Some { Name = "turn"; Code = transformCode turn } 
    | "flip" -> 
        Some { Name = "flip"; Code = transformCode flip } 
    | "toss" -> 
        Some { Name = "toss"; Code = transformCode toss } 
    | _ -> None

let tryParseFunctionValue (s : string) : StackValue option =
    s |> tryLookupFunction |> Option.map FunctionValue

let parseStackValue (s : string) : StackValue =
    let parseResult = 
        tryParseNumberValue s
          |> Option.orElse (tryParsePictureValue s)
          |> Option.orElse (tryParseFunctionValue s)
    match parseResult with 
    | Some v -> v 
    | None -> failwith <| sprintf "Invalid value %s" s  

let rec tryFindFirstPicture (stack : Stack) : Picture option = 
    match stack with 
    | [] -> None 
    | PictureValue p :: t -> Some p 
    | _ :: t -> tryFindFirstPicture t

let fooStuff values = 
    let stk = runProgram [] values
    let maybePicture = tryFindFirstPicture stk 
    let bounds = (600, 600)
    let background = Grey
    match maybePicture with 
    | Some picture -> 
        let box = { a = { x = 100.; y = 100. }
                    b = { x = 400.; y = 0. }
                    c = { x = 0.; y = 400. } }
        let lens = (box, Blackish)
        let shapes = picture lens
        let v = view (bounds, background, shapes)
        let result = renderHtmlDocument v
        htmlString result
    | None -> htmlString "Nothing here."


let render (s : string) : HttpHandler = 
    let strs = s.Split("/") |> List.ofArray |> List.tail
    try 
        let values = strs |> List.map parseStackValue
        fooStuff values
    with 
    | _ -> htmlString "bad stuff"

let escherHandler (matches : string seq) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        match matches |> Seq.tail |> Seq.tryExactlyOne with 
        | Some thing -> (render thing) next ctx
        | None -> (text "nothing" next ctx)
