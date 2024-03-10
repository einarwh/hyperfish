module Transformation

open Vector
open Shape
open Styling
open Box
open Lens
open Picture
open Giraffe.ViewEngine

type Bounds = (int * int)

type Model = (Bounds * StyleColor * (Shape * Style) list)

let mapper ({ a = a; b = b; c = c } : Box)
           { x = x; y = y } =
   a + b * x + c * y

let size { x = x; y = y } = 
  sqrt(x * x + y * y)

let getStrokeWidth { a = _; b = b; c = c } =
  let s = min (size b) (size c)
  s / 80.

let getRadiusScale { a = _; b = b; c = c } =
  let s = min (size b) (size c)
  s / 20.

let mapBezierSegment m = function 
| { controlPoint1 = cp1
    controlPoint2 = cp2
    endPoint = ep } ->
  { controlPoint1 = m cp1
    controlPoint2 = m cp2
    endPoint = m ep }

let mapLineSegment m = function 
| { targetPoint = tp } ->
  { targetPoint = m tp }

let mapPathSegment m = function 
| BezierSegment bs -> BezierSegment (mapBezierSegment m bs) 
| LineSegment ls -> LineSegment (mapLineSegment m ls)

let isInnerEye name = 
  name = "eye-inner" || name = "egg-eye-inner"

let isOuterEye name = 
  name = "eye-outer" || name = "egg-eye-outer"

let getColor name = function 
  | Blackish -> 
    if name = "primary" then StyleColor.Black  
    else if isOuterEye name then StyleColor.White 
    else if isInnerEye name then StyleColor.Black 
    else StyleColor.White 
  | Greyish -> 
    if name = "primary" then StyleColor.Grey 
    else if isOuterEye name then StyleColor.White 
    else if isInnerEye name then StyleColor.Grey 
    else StyleColor.White 
  | Whiteish -> 
    if name = "primary" then StyleColor.White  
    else if isOuterEye name then StyleColor.White  
    else if isInnerEye name then StyleColor.Black 
    else StyleColor.Black

let getEyeLiner sw hue =  
  { strokeColor = getColor "secondary" hue 
    strokeWidth = sw }
    
let getClosedPathStyle name sw hue = 
   let stroke = 
    if isOuterEye name then Some <| getEyeLiner sw hue 
    else 
      match hue with 
      | Whiteish -> Some { strokeColor = StyleColor.Grey; strokeWidth = 1 }
      | _ -> None 
   let fill = Some { fillColor = getColor name hue }
   { stroke = stroke; fill = fill }

let getOpenPathStyle name sw hue = 
  let stroke = 
    { strokeColor = getColor name hue 
      strokeWidth = sw }
  { stroke = Some stroke; fill = None }    

let getPathStyle close name sw hue = 
  if close then 
    getClosedPathStyle name sw hue 
  else 
    getOpenPathStyle name sw hue 

let getDefaultColor name hue = 
  if name = "secondary" then 
    match hue with 
    | Blackish -> StyleColor.White
    | Greyish -> StyleColor.White
    | Whiteish -> StyleColor.Black
  else
    match hue with 
    | Blackish -> StyleColor.Black
    | Greyish -> StyleColor.Grey
    | Whiteish -> StyleColor.White

let getDefaultFill name hue = 
  if name = "filled" then 
    match hue with 
    | Blackish -> Some { fillColor = StyleColor.Black }
    | Greyish ->  Some { fillColor = StyleColor.Grey  }
    | Whiteish -> Some { fillColor = StyleColor.White } 
  else
    None

let getDefaultStyle name hue sw = 
  let stroke = 
    { strokeWidth = sw 
      strokeColor = getDefaultColor name hue }
  { stroke = Some stroke; fill = getDefaultFill name hue }

let mapNamedShape (box : Box, hue : Hue) (name, shape) : (Shape * Style) = 
  let m = mapper box
  let sw = getStrokeWidth box
  let rs = getRadiusScale box 
  match shape with
  | Polygon { points = pts } ->
    Polygon { points = pts |> List.map m }, getDefaultStyle name hue sw
  | Curve { point1 = v1
            point2 = v2 
            point3 = v3 
            point4 = v4 } ->
    Curve { point1 = m v1
            point2 = m v2 
            point3 = m v3 
            point4 = m v4 }, getDefaultStyle name hue sw
  | Path { start = start; close = close; segments = beziers } ->
    let style = getPathStyle close name sw hue
    Path { start = m start; close = close; segments = beziers |> List.map (mapPathSegment m) }, style
  | Line { lineStart = v1
           lineEnd = v2 } ->
    Line { lineStart = m v1 
           lineEnd = m v2 }, getDefaultStyle name hue sw
  | Circle { center = c; radius = r } ->
    Circle { center = m c; radius = rs * r } , getDefaultStyle name hue sw
  | Polyline { pts = pts } ->
    Polyline { pts = pts |> List.map m }, getDefaultStyle name hue sw

let createLensPicture (shapes : (string * Shape) list) : Picture = 
   fun lens ->
     shapes |> List.map (mapNamedShape lens)

let mirrorVector height { x = x; y = y } =
  { x = x; y = height - y }
  
let mirrorShape mirror = function 
    | Line { lineStart = lineStart 
             lineEnd = lineEnd } ->
      Line { lineStart = mirror lineStart 
             lineEnd = mirror lineEnd }
    | Circle { center = c; radius = r } ->
      Circle { center = mirror c; radius = r }
    | Polyline { pts = pts } ->
      Polyline { pts = pts |> List.map mirror }
    | Polygon { points = pts } ->
      Polygon { points = pts |> List.map mirror }
    | Curve { point1 = v1
              point2 = v2 
              point3 = v3 
              point4 = v4 } ->
      Curve { point1 = mirror v1
              point2 = mirror v2 
              point3 = mirror v3 
              point4 = mirror v4 }
    | Path { start = start; close = close; segments = pathSegments } ->        
      Path { start = mirror start; close = close; segments = pathSegments |> List.map (mapPathSegment mirror) }

let getStrokeWidthFromStyle = function 
  | Some strokeStyle ->
    strokeStyle.strokeWidth
  | None -> 1.

let getStrokeColorFromStyle = function 
  | Some strokeStyle ->
    strokeStyle.strokeColor
  | None ->
    StyleColor.Black

let getStrokeColorName = function 
  | StyleColor.Black -> "black"
  | StyleColor.White -> "white"
  | StyleColor.Grey -> "grey"

let getStrokePen { strokeWidth = sw; strokeColor = sc } = 
  let color = 
    match sc with 
    | Black -> "black" 
    | Grey -> "grey"
    | White -> "white"
  (color, sw)

let getFillBrush { fillColor = fc } = 
  match fc with 
  | Black -> "black" 
  | Grey -> "grey"
  | White -> "white"

let svg = tag "svg"

let _stroke = attr "stroke" 
let _blackstroke = _stroke "black"
let _fill = attr "fill"
let _nonefill = _fill "none" 
let _strokeWidth = attr "stroke-width" 
let _strokeLineCap = attr "stroke-linecap"
let _d = attr "d"

let line (lineShape : LineShape) : XmlNode = 
  match lineShape with 
  | { lineStart = { x = x1; y = y1 } 
      lineEnd = { x = x2; y = y2 } } -> 
  let pt n v = v |> sprintf "%f" |> attr n
  let attrs = 
    [ pt "x1" x1 
      pt "y1" y1
      pt "x2" x2 
      pt "y2" y2 
      _stroke "green" ]
  tag "line" attrs []

let circle (style : Style) (circleShape : CircleShape) : XmlNode = 
  match circleShape with 
  | { center = { x = x; y = y } 
      radius = r } -> 
  let pt n v = v |> sprintf "%f" |> attr n
  let strokeWidth = getStrokeWidthFromStyle style.stroke
  let (strokeColor, sw) = 
    match style.stroke with 
    | Some stroke -> getStrokePen stroke
    | None -> ("none", strokeWidth)
  let fillColor = 
    match style.fill with 
    | Some fill -> getFillBrush fill
    | None -> "none"
  let attrs = 
    [ pt "cx" x 
      pt "cy" y
      pt "r" r 
      _strokeWidth (sprintf "%f" strokeWidth)
      _stroke strokeColor
      _fill fillColor ]
  tag "circle" attrs []

let polygon (style : Style) (polygonShape : PolygonShape) : XmlNode = 
  match polygonShape with 
  | { points = pts } -> 
    let strokeWidth = getStrokeWidthFromStyle style.stroke
    let (strokeColor, sw) = 
      match style.stroke with 
      | Some stroke -> getStrokePen stroke
      | None -> ("none", strokeWidth)
    let fillColor = 
      match style.fill with 
      | Some fill -> getFillBrush fill
      | None -> "none"
    let pt { x = x; y = y } = sprintf "%f,%f" x y
    let s = pts |> List.map pt |> List.fold (fun acc it -> if acc = "" then it else acc + " " + it) ""
    let attrs = 
      [ _stroke strokeColor
        _fill fillColor
        attr "points" s ]
    tag "polygon" attrs [] 

let polyline (polylineShape : PolylineShape) : XmlNode = 
  match polylineShape with 
  | { pts = pts } -> 
    let pt { x = x; y = y } = sprintf "%f,%f" x y
    let s = pts |> List.map pt |> List.fold (fun acc it -> if acc = "" then it else acc + " " + it) ""
    let attrs = 
      [ _blackstroke
        _nonefill
        attr "points" s ]
    tag "polyline" attrs [] 

let curve (style : Style) (curveShape : CurveShape) : XmlNode = 
  match curveShape with 
  | { point1 = { x = x1; y = y1 }
      point2 = { x = x2; y = y2 } 
      point3 = { x = x3; y = y3 } 
      point4 = { x = x4; y = y4 } } ->
    let d = sprintf "M%f %f C %f %f, %f %f, %f %f" x1 y1 x2 y2 x3 y3 x4 y4
    let strokeWidth = getStrokeWidthFromStyle style.stroke
    let (strokeColor, sw) = 
      match style.stroke with 
      | Some stroke -> getStrokePen stroke
      | None -> ("none", strokeWidth)
    let fillColor = 
      match style.fill with 
      | Some fill -> getFillBrush fill
      | None -> "none"
    let attrs  =
        [ _stroke strokeColor
          _fill fillColor
          _strokeWidth (sprintf "%f" sw)
          _strokeLineCap "butt"
          _d d ]
    tag "path" attrs []

let path (style : Style) (pathShape : PathShape) : XmlNode = 
  match pathShape with 
  | { start = { x = x; y = y }; close = close; segments = pathSegments } -> 
    let nextBezierSegment 
      { controlPoint1 = { x = x1; y = y1 }
        controlPoint2 = { x = x2; y = y2 }
        endPoint      = { x = x3; y = y3 } } =
      sprintf "C %f %f, %f %f, %f %f" x1 y1 x2 y2 x3 y3 
    let nextLineSegment 
      { targetPoint = {x = x; y = y } } =
      sprintf "L %f %f" x y
    let nextPathSegment segment = 
      match segment with 
      | BezierSegment bs -> nextBezierSegment bs
      | LineSegment ls -> nextLineSegment ls
    let nextShape { controlPoint1 = { x = x1; y = y1 }
                    controlPoint2 = { x = x2; y = y2 }
                    endPoint      = { x = x3; y = y3 } } =
      sprintf "C %f %f, %f %f, %f %f" x1 y1 x2 y2 x3 y3 
    let startStr = sprintf "M%f %f" x y
    let nextStrs = pathSegments |> List.map nextPathSegment 
    let strs = if close then nextStrs @ [ "Z" ] else nextStrs
    let d = startStr :: strs |> String.concat " "
    let strokeWidth = getStrokeWidthFromStyle style.stroke
    let (strokeColor, sw) = 
      match style.stroke with 
      | Some stroke -> getStrokePen stroke
      | None -> ("none", strokeWidth)
    let fillColor = 
      match style.fill with 
      | Some fill -> getFillBrush fill
      | None -> "none"
    let attrs =
      [ _stroke strokeColor
        _fill fillColor
        _strokeWidth (sprintf "%f" sw)
        _strokeLineCap "butt"
        _d d ]
    tag "path" attrs []

let toSvgElement (style : Style) = function 
    | Line lineShape ->
      line lineShape
    | Circle circleShape ->
      circle style circleShape
    | Polyline polylineShape ->
      polyline polylineShape
    | Polygon polygonShape ->
      polygon style polygonShape
    | Curve curveShape ->
      curve style curveShape
    | Path pathShape ->
      path style pathShape 

let getBackgroundColor = function 
  | Grey -> "#CCCCCC"
  | Black -> "black"
  | White -> "white"       

let view ((bounds, background, shapes) : Model) : XmlNode = 
    let (svgWidth, svgHeight) = bounds
    let mv = (mirrorVector <| float svgHeight)
    let fn (shape, style) = 
      (mirrorShape mv >> toSvgElement style) shape      
    let svgElements = shapes |> List.map fn
    svg [ //Style [ BackgroundColor (getBackgroundColor background) ]
          attr "width" (sprintf "%d" svgWidth) 
          attr "height" (sprintf "%d" svgHeight) 
        ]     
        svgElements
