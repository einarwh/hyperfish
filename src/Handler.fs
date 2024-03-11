module Handler

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.ViewEngine
open Box
open Lens
open Transformation
open Picture
open Styling
open Fish
open Lizard
open Letters
open Figures
open Stack

exception TypeException of string

let concatStrings (s : string) (strings : string seq) = 
    System.String.Join(s, strings)

let pictureShapesDictionary = dict [
    "fish", fishShapes 
    "george", georgeShapes
    "byteman", byteman
    "f-letter", fLetter
    "h-letter", hLetter
    "e-letter", eLetter
    "n-letter", nLetter
    "d-letter", dLetter
    "r-letter", rLetter
    "s-letter", sLetter
    "o-letter", oLetter
    "lizard", lizard
    "blank", []
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

let zoomCombinatorCode (combinator : int -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture) : Stack -> Stack = 
    fun stack -> 
        match stack with 
        | [] -> raise (StackUnderflowException)
        | NumberValue n :: restStack ->
            nonetCombinatorCode (combinator n) restStack
        | _ -> raise (TypeException "Expected a number on the stack.")

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

let transformFunction (name : string) (transform : Picture -> Picture) : Function = 
    { Name = name; Parameters = [PictureType]; Code = transformCode transform }

let pairCombinatorFunction (name : string) (combinator : Picture -> Picture -> Picture) : Function = 
    { Name = name; Parameters = [PictureType; PictureType]; Code = pairCombinatorCode combinator }

let weightedPairCombinatorFunction (name : string) (combinator : int -> int -> Picture -> Picture -> Picture) : Function = 
    { Name = name; Parameters = [NumberType; NumberType; PictureType; PictureType]; Code = weightedPairCombinatorCode combinator }

let numberCombinatorFunction (name : string) (combinator : int -> Picture -> Picture) : Function = 
    { Name = name; Parameters = [NumberType; PictureType]; Code = numberCombinatorCode combinator }

let quartetCombinatorFunction (name : string) (combinator : Picture -> Picture -> Picture -> Picture -> Picture) : Function = 
    { Name = name; Parameters = [PictureType; PictureType; PictureType; PictureType]; Code = quartetCombinatorCode combinator }

let nonetCombinatorFunction (name : string) (combinator : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture) : Function = 
    { Name = name; Parameters = [PictureType; PictureType; PictureType; PictureType; PictureType; PictureType; PictureType; PictureType; PictureType]; Code = nonetCombinatorCode combinator }

let zoomCombinatorFunction (name : string) (combinator : int -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture) : Function = 
    { Name = name; Parameters = [NumberType; PictureType; PictureType; PictureType; PictureType; PictureType; PictureType; PictureType; PictureType; PictureType]; Code = zoomCombinatorCode combinator }

let functionDictionary : System.Collections.Generic.IDictionary<string, Function> = dict [
    ("dup", { Name = "dup"; Parameters = [PictureType]; Code = dupCode })
    ("turn", transformFunction "turn" turn)
    ("flip", transformFunction "flip" flip) 
    ("toss", transformFunction "toss" toss)
    ("hue", transformFunction "hue" rehue) 
    ("above", pairCombinatorFunction "above" above) 
    ("beside", pairCombinatorFunction "beside" beside) 
    ("over", pairCombinatorFunction "over" over)
    ("above-ratio", weightedPairCombinatorFunction "above-ratio" aboveRatio) 
    ("beside-ratio", weightedPairCombinatorFunction "beside-ratio" besideRatio) 
    ("quartet", quartetCombinatorFunction "quartet" quartet)
    ("self-quartet", transformFunction "self-quartet" selfQuartet)
    ("nonet", nonetCombinatorFunction "nonet" nonet)
    ("self-nonet", transformFunction "self-nonet" selfNonet)
    ("zoom", zoomCombinatorFunction "zoom" zoom)
    ("t-tile-1", transformFunction "ttile1" ttile1)
    ("t-tile-2", transformFunction "ttile2" ttile2) 
    ("u-tile-1", transformFunction "utile1" utile1) 
    ("u-tile-2", transformFunction "utile2" utile2) 
    ("u-tile-3", transformFunction "utile3" utile3) 
    ("side-1", numberCombinatorFunction "side-1" sideNS) 
    ("side-2", numberCombinatorFunction "side-2" sideEW) 
    ("corner-1", numberCombinatorFunction "corner-1" cornerNWSE) 
    ("corner-2", numberCombinatorFunction "corner-2" cornerNESW) 
    ("square-limit", numberCombinatorFunction "square-limit" squareLimit) 
    ("tunnel-corner", numberCombinatorFunction "tunnel-corner" tunnelCorner) 
    ("tunnel", numberCombinatorFunction "tunnel" tunnel) 
]

let tryLookupFunction (name : string) : Function option =
    match functionDictionary.TryGetValue(name) with 
    | (true, f) -> Some f
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
                b = { x = 400.; y = 0. }
                c = { x = 0.; y = 400. } }
    let lens = (box, Blackish)
    view ((600, 600), Grey, picture lens)

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
            sprintf "/%s" name
        else 
            sprintf "/%s/%s" stackString name

    let pictureNames = pictureShapesDictionary |> Seq.map (fun kvp -> kvp.Key) |> Seq.toList
    let pictureLinks = pictureNames |> List.map (fun n -> a [ attr "style" "color:darkred"; attr "href" (createLink n) ] [ str n ] )
    let formActionTarget = [ stackString ] |> concatStrings "/" |> sprintf "/%s"
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
                if popStackString = "" then "/"
                else sprintf "/%s" popStackString
            let popLink = a [ attr "style" "color:darkred"; attr "href" popHref ] [ str "pop" ] 
            popLink :: links

    let withClearLink (links : XmlNode list) : XmlNode list =
        let clearLink = a [ attr "style" "color:darkred"; attr "href" "/" ] [ str "clear" ] 
        clearLink :: links

    let rec legalOperation (parameters : ParamType list) (stack : Stack): bool = 
        match (parameters, stack) with 
        | [], _ -> true
        | (PictureType :: restParams, PictureValue _ :: restStack) -> legalOperation restParams restStack
        | (NumberType :: restParams, NumberValue _ :: restStack) -> legalOperation restParams restStack
        | _ -> false

    let operationNames = 
        functionDictionary 
        |> Seq.filter (fun kvp -> legalOperation kvp.Value.Parameters stk) 
        |> Seq.map (fun kvp -> kvp.Key) 
        |> Seq.toList

    let operationLinks = operationNames |> List.map (fun n -> a [ attr "style" "color:darkred"; attr "href" (createLink n) ] [ str n ] )
    let allOperationLinks = 
        operationLinks 
        |> withPopLink 
        |> withClearLink
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
                link [ attr "rel" "stylesheet"; attr "type" "text/css"; attr "href" "//fonts.googleapis.com/css?family=Open+Sans" ]
            ]
            body [ attr "style" "font-family:Open Sans" ] [
                table [ attr "valign" "top" ] [
                    tr [ attr "valign" "top" ] [
                        td [ attr "width" "600" ] [
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
    let result = RenderView.AsString.htmlDocument doc
    htmlString result

let getHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let pathString = ctx.Request.Path.Value.Substring(1) 
        let pathElements = pathString.Split("/") |> Array.toList |> List.filter (fun s -> s.Length > 0)
        try 
            (handleRequest pathElements) next ctx
        with 
        | StackUnderflowException -> 
            ctx.SetStatusCode 400
            (htmlString "Stack underflow exception!") next ctx
        | TypeException msg -> 
            ctx.SetStatusCode 400
            (htmlString msg) next ctx 
        | ex ->
            ctx.SetStatusCode 500 
            (htmlString ex.Message) next ctx

let postHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let pathString = ctx.Request.Path.Value 
        match ctx.Request.HasFormContentType with
        | false -> text "Bad request - where is the form?" next ctx
        | true ->
            match ctx.Request.Form.TryGetValue("number") with 
            | (true, formStringValues) ->
                let numberStr = formStringValues.[0]
                let pathString = ctx.Request.Path.Value 
                let location = sprintf "%s/%s" pathString numberStr
                (redirectTo false location) next ctx
            | _ ->
                text "Bad request - where is the number?" next ctx
