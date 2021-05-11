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

let pairCombinatorCode (combinator : Picture -> Picture -> Picture) : Stack -> Stack = 
    fun stack -> 
        match stack with 
        | [] -> raise (StackUnderflowException)
        | PictureValue p1 :: stack1 -> 
            match stack1 with
            | [] -> raise (StackUnderflowException)
            | PictureValue p2 :: stack2 -> 
                PictureValue (combinator p1 p2) :: stack2
            | _ -> raise (TypeException "Expected a picture as the second argument on the stack")
        | _ -> raise (TypeException "Expected a picture as the first argument on the stack")

let weightedPairCombinatorCode (combinator : int -> int -> Picture -> Picture -> Picture) : Stack -> Stack = 
    fun stack -> 
        match stack with 
        | [] -> raise (StackUnderflowException)
        | NumberValue n1 :: stack1 -> 
            match stack1 with 
            | [] -> raise (StackUnderflowException)
            | NumberValue n2 :: stack2 -> 
                match stack2 with 
                | [] -> raise (StackUnderflowException)
                | PictureValue p1 :: stack3 -> 
                    match stack3 with 
                    | [] -> raise (StackUnderflowException)
                    | PictureValue p2 :: stack4 -> 
                        PictureValue (combinator n1 n2 p1 p2) :: stack4
                    | _ -> raise (TypeException "Expected a picture as the fourth argument on the stack")
                | _ -> raise (TypeException "Expected a picture as the third argument on the stack")
            | _ -> raise (TypeException "Expected a number as the second argument on the stack")
        | _ -> raise (TypeException "Expected a number as the first argument on the stack")

let quartetCombinatorCode (combinator : Picture -> Picture -> Picture -> Picture -> Picture) : Stack -> Stack = 
    fun stack -> 
        match stack with 
        | [] -> raise (StackUnderflowException)
        | PictureValue p1 :: stack1 -> 
            match stack1 with 
            | [] -> raise (StackUnderflowException)
            | PictureValue p2 :: stack2 -> 
                match stack2 with 
                | [] -> raise (StackUnderflowException)
                | PictureValue p3 :: stack3 -> 
                    match stack3 with 
                    | [] -> raise (StackUnderflowException)
                    | PictureValue p4 :: stack4 -> 
                        PictureValue (combinator p1 p2 p3 p4) :: stack4
                    | _ -> raise (TypeException "Expected a picture as the fourth argument on the stack")
                | _ -> raise (TypeException "Expected a picture as the third argument on the stack")
            | _ -> raise (TypeException "Expected a picture as the second argument on the stack")
        | _ -> raise (TypeException "Expected a picture as the first argument on the stack")

let nonetCombinatorCode (combinator : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture) : Stack -> Stack = 
    fun stack -> 
        match stack with 
        | [] -> raise (StackUnderflowException)
        | PictureValue p1 :: stack1 -> 
            match stack1 with 
            | [] -> raise (StackUnderflowException)
            | PictureValue p2 :: stack2 -> 
                match stack2 with 
                | [] -> raise (StackUnderflowException)
                | PictureValue p3 :: stack3 -> 
                    match stack3 with 
                    | [] -> raise (StackUnderflowException)
                    | PictureValue p4 :: stack4 -> 
                        match stack4 with 
                        | [] -> raise (StackUnderflowException)
                        | PictureValue p5 :: stack5 -> 
                            match stack5 with 
                            | [] -> raise (StackUnderflowException)
                            | PictureValue p6 :: stack6 -> 
                                match stack6 with 
                                | [] -> raise (StackUnderflowException)
                                | PictureValue p7 :: stack7 -> 
                                    match stack7 with 
                                    | [] -> raise (StackUnderflowException)
                                    | PictureValue p8 :: stack8 -> 
                                        match stack8 with 
                                        | [] -> raise (StackUnderflowException)
                                        | PictureValue p9 :: stack9 -> 
                                            PictureValue (combinator p1 p2 p3 p4 p5 p6 p7 p8 p9) :: stack9
                                        | _ -> raise (TypeException "Expected a picture as the ninth argument on the stack")
                                    | _ -> raise (TypeException "Expected a picture as the eight argument on the stack")
                                | _ -> raise (TypeException "Expected a picture as the seventh argument on the stack")
                            | _ -> raise (TypeException "Expected a picture as the sixth argument on the stack")
                        | _ -> raise (TypeException "Expected a picture as the fifth argument on the stack")
                    | _ -> raise (TypeException "Expected a picture as the fourth argument on the stack")
                | _ -> raise (TypeException "Expected a picture as the third argument on the stack")
            | _ -> raise (TypeException "Expected a picture as the second argument on the stack")
        | _ -> raise (TypeException "Expected a picture as the first argument on the stack")


let numberCombinatorCode (combinator : int -> Picture -> Picture) : Stack -> Stack = 
    fun stack -> 
        match stack with 
        | [] -> raise (StackUnderflowException)
        | NumberValue n1 :: stack1 -> 
            match stack1 with
            | [] -> raise (StackUnderflowException)
            | PictureValue p2 :: stack2 -> 
                PictureValue (combinator n1 p2) :: stack2
            | _ -> raise (TypeException "Expected a picture as the second argument on the stack")
        | _ -> raise (TypeException "Expected a number as the first argument on the stack")

let tryLookupFunction (name : string) : Function option =
    match name with 
    | "turn" -> 
        Some { Name = "turn"; Code = transformCode turn } 
    | "flip" -> 
        Some { Name = "flip"; Code = transformCode flip } 
    | "toss" -> 
        Some { Name = "toss"; Code = transformCode toss } 
    | "hue" -> 
        Some { Name = "hue"; Code = transformCode rehue } 
    | "above" -> 
        Some { Name = "above"; Code = pairCombinatorCode above } 
    | "beside" -> 
        Some { Name = "beside"; Code = pairCombinatorCode beside } 
    | "over" -> 
        Some { Name = "over"; Code = pairCombinatorCode over } 
    | "above-ratio" -> 
        Some { Name = "above-ratio"; Code = weightedPairCombinatorCode aboveRatio } 
    | "beside-ratio" -> 
        Some { Name = "beside-ratio"; Code = weightedPairCombinatorCode besideRatio } 
    | "quartet" -> 
        Some { Name = "quartet"; Code = quartetCombinatorCode quartet } 
    | "nonet" -> 
        Some { Name = "nonet"; Code = nonetCombinatorCode nonet } 
    | "t-tile-1" -> 
        Some { Name = "t-tile-1"; Code = transformCode ttile1 } 
    | "t-tile-2" -> 
        Some { Name = "t-tile-2"; Code = transformCode ttile2 } 
    | "u-tile-1" -> 
        Some { Name = "u-tile-1"; Code = transformCode utile1 } 
    | "u-tile-2" -> 
        Some { Name = "u-tile-2"; Code = transformCode utile2 } 
    | "u-tile-3" -> 
        Some { Name = "u-tile-3"; Code = transformCode utile3 } 
    | "side-1" -> 
        Some { Name = "side-1"; Code = numberCombinatorCode sideNS } 
    | "side-2" -> 
        Some { Name = "side-2"; Code = numberCombinatorCode sideEW } 
    | "corner-1" -> 
        Some { Name = "corner-1"; Code = numberCombinatorCode cornerNWSE } 
    | "corner-2" -> 
        Some { Name = "corner-2"; Code = numberCombinatorCode cornerNESW } 
    | "square-limit" -> 
        Some { Name = "square-limit"; Code = numberCombinatorCode squareLimit } 
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
    let bounds = (500, 500)
    let background = Grey
    match maybePicture with 
    | Some picture -> 
        let box = { a = { x = 50.; y = 50. }
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
    | StackUnderflowException -> htmlString "Stack underflow exception!"
    | TypeException msg -> htmlString msg
    | _ -> htmlString "bad stuff"

let escherHandler (matches : string seq) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        match matches |> Seq.tail |> Seq.tryExactlyOne with 
        | Some thing -> (render thing) next ctx
        | None -> (text "nothing" next ctx)
