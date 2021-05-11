module Handler

open FSharp.Control.Tasks
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

let concatStrings (s : string) (strings : string seq) = 
    System.String.Join(s, strings)

let pictureShapesDictionary = dict [
    "blank", []
    "fish", fishShapes 
    "george", georgeShapes
    "f-letter", fLetter
    "h-letter", hLetter
    "e-letter", eLetter
    "n-letter", nLetter
    "d-letter", dLetter
    "r-letter", rLetter
    "s-letter", sLetter
    "o-letter", oLetter
]

let tailless list = 
    match List.rev list with 
    | [] -> []
    | h :: t -> List.rev t

let toStackString (strs : string seq) : string = 
    concatStrings "/" strs

let tryLookupPicture (name : string) : Picture option = 
    let maybeShapes = 
        match pictureShapesDictionary.TryGetValue(name) with 
        | (true, shapes) -> Some shapes 
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

let popCode : Stack -> Stack = 
    fun stack -> 
        match stack with 
        | [] -> raise (StackUnderflowException)
        | _ :: restStack -> restStack

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

let dupCode : Stack -> Stack = 
    fun stack -> 
        match stack with 
        | [] -> raise (StackUnderflowException)
        | PictureValue p :: _ -> 
            PictureValue p :: stack
        | _ -> raise (TypeException "Expected a picture as the first argument on the stack")

let functionDictionary = dict [
    ("dup", dupCode)
    ("turn", transformCode turn)
    ("flip", transformCode flip) 
    ("toss", transformCode toss)
    ("hue", transformCode rehue) 
    ("above", pairCombinatorCode above) 
    ("beside", pairCombinatorCode beside) 
    ("over", pairCombinatorCode over)
    ("above-ratio", weightedPairCombinatorCode aboveRatio) 
    ("beside-ratio", weightedPairCombinatorCode besideRatio) 
    ("quartet", quartetCombinatorCode quartet)
    ("nonet", nonetCombinatorCode nonet)
    ("t-tile-1", transformCode ttile1)
    ("t-tile-2", transformCode ttile2) 
    ("u-tile-1", transformCode utile1) 
    ("u-tile-2", transformCode utile2) 
    ("u-tile-3", transformCode utile3) 
    ("side-1", numberCombinatorCode sideNS) 
    ("side-2", numberCombinatorCode sideEW) 
    ("corner-1", numberCombinatorCode cornerNWSE) 
    ("corner-2", numberCombinatorCode cornerNESW) 
    ("square-limit", numberCombinatorCode squareLimit) 
]

let tryLookupFunction (name : string) : Function option =
    match functionDictionary.TryGetValue(name) with 
    | (true, code) -> Some { Name = name; Code = code }
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

let toSvg (picture : Picture) : XmlNode =
    let box = { a = { x = 100.; y = 100. }
                b = { x = 200.; y = 0. }
                c = { x = 0.; y = 200. } }
    let lens = (box, Blackish)
    view ((400, 400), Grey, picture lens)

let handleRequest stackStrings = 
    let values = stackStrings |> List.map parseStackValue
    let stackString = toStackString stackStrings
    let popStackString = stackStrings |> tailless |> toStackString
    let stk = runProgram [] values
    let maybePicture = tryFindFirstPicture stk 
    let pictureNode = 
        maybePicture
        |> Option.map toSvg 
        |> Option.defaultValue (div [] []) 

    let createLink name = 
        if stackString = "" then 
            sprintf "/escher/%s" name
        else 
            sprintf "/escher/%s/%s" stackString name

    let pictureNames = pictureShapesDictionary |> Seq.map (fun kvp -> kvp.Key) |> Seq.toList
    let pictureLinks = pictureNames |> List.map (fun n -> a [ attr "href" (createLink n) ] [ str n ] )
    let formActionTarget = [ "escher"; stackString ] |> concatStrings "/" |> sprintf "/%s"
    let picturesDiv = div [] [ 
        h3 [] [ str "Pictures" ] 
        div [] [
            ul [] (List.map (fun link -> li [] [link]) pictureLinks)
        ] 
        hr []
        h3 [] [ str "Input" ]

        form [ attr "action" formActionTarget; attr "method" "POST" ] [
            label [ attr "for" "number" ] [ str "Number" ]
            input [ attr "type" "text"; attr "id" "number"; attr "name" "number" ]
            input [ attr "type" "submit"; attr "value" "push" ]
        ]
    ] 
 
    let withPopLink (links : XmlNode list) : XmlNode list =
        match stackString with 
        | "" -> links
        | _ -> 
            let popHref = 
                if popStackString = "" then "/escher"
                else sprintf "/escher/%s" popStackString
            let popLink = a [ attr "href" popHref ] [ str "pop" ] 
            popLink :: links

    let withClearLink (links : XmlNode list) : XmlNode list =
        let clearLink = a [ attr "href" "/escher" ] [ str "clear" ] 
        clearLink :: links

    let operationNames = functionDictionary |> Seq.map (fun kvp -> kvp.Key) |> Seq.toList
    let operationLinks = operationNames |> List.map (fun n -> a [ attr "href" (createLink n) ] [ str n ] )
    let allOperationLinks = 
        operationLinks |> withPopLink |> withClearLink
    let operationsDiv = div [] [ 
        h3 [] [ str "Operations" ] 
        div [] [
            ul [] (List.map (fun link -> li [] [link]) allOperationLinks)
        ] 
    ] 
    
    let doc = 
        html [] [
            head [] [
                title [] [ str "Hyperfish: Hypermedia-driven functional geometry"]
            ]
            body [] [
                table [ attr "valign" "top" ] [
                    tr [ attr "valign" "top" ] [
                        td [ attr "width" "400" ] [
                            pictureNode
                        ]
                        td [ attr "width" "150" ] [
                            picturesDiv
                        ]
                        td [ attr "width" "150" ] [
                            operationsDiv
                        ]
                    ]
                ]
            ]
        ]
    let result = renderHtmlDocument doc
    htmlString result

let render (s : string) : HttpHandler = 
    let strs = s.Split("/") |> List.ofArray |> List.tail
    try 
        handleRequest strs
    with 
    | StackUnderflowException -> htmlString "Stack underflow exception!"
    | TypeException msg -> htmlString msg
    | _ -> htmlString "bad stuff"

let escherHandler (matches : string seq) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        match matches |> Seq.tail |> Seq.tryExactlyOne with 
        | Some thing -> (render thing) next ctx
        | None -> (text "nothing" next ctx)

let escherPostHandler (matches : string seq) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        match ctx.Request.HasFormContentType with
        | false -> text "Bad request - where is the form?" next ctx
        | true ->
            printfn "FORM!!! %A" ctx.Request.Form
            match ctx.Request.Form.TryGetValue("number") with 
            | (true, formStringValues) ->
                let numberStr = formStringValues.[0]
                let location = sprintf "%s/%s" (matches |> Seq.head) numberStr
                (redirectTo false location) next ctx
            | _ ->
                text "Bad request - where is the number?" next ctx
