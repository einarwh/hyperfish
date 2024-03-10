module Figures

open Vector
open Shape

let george = 
  let pts1 = [ 
    { x = 0.00; y = 0.55 } 
    { x = 0.15; y = 0.45 }
    { x = 0.30; y = 0.55 }
    { x = 0.40; y = 0.50 }
    { x = 0.20; y = 0.00 } ]
  let pts2 = [
    { x = 0.00; y = 0.80 }
    { x = 0.15; y = 0.60 }
    { x = 0.30; y = 0.65 }
    { x = 0.40; y = 0.65 }
    { x = 0.35; y = 0.80 }
    { x = 0.40; y = 1.00 } ]
  let pts3 = [
    { x = 0.60; y = 1.00 }
    { x = 0.65; y = 0.80 }
    { x = 0.60; y = 0.65 }
    { x = 0.80; y = 0.65 }
    { x = 1.00; y = 0.45 } ]
  let pts4 = [
    { x = 1.00; y = 0.20 }
    { x = 0.60; y = 0.50 }
    { x = 0.80; y = 0.00 } ]
  let pts5 = [
    { x = 0.40; y = 0.00 }
    { x = 0.50; y = 0.30 }
    { x = 0.60; y = 0.00 } ]
  [ Polyline { pts = pts1 }
    Polyline { pts = pts2 }
    Polyline { pts = pts3 }
    Polyline { pts = pts4 }
    Polyline { pts = pts5 } ]

let georgeShapes = george |> List.map (fun shape -> ("primary", shape))

let bytemanPolygon = 
  let pt01 =  { x = 0.250; y = 0.000 } 
  let pt02 =  { x = 0.375; y = 0.000 } 
  let pt03 =  { x = 0.375; y = 0.250 } 
  let pt04 =  { x = 0.625; y = 0.250 } 
  let pt05 =  { x = 0.625; y = 0.000 } 
  let pt06 =  { x = 0.750; y = 0.000 } 
  let pt07 =  { x = 0.750; y = 0.625 } 
  let pt08 =  { x = 1.000; y = 0.625 } 
  let pt09 =  { x = 1.000; y = 0.750 } 
  let pt10 =  { x = 0.625; y = 0.750 } 
  let pt11 =  { x = 0.625; y = 1.000 } 
  let pt12 =  { x = 0.375; y = 1.000 } 
  let pt13 =  { x = 0.375; y = 0.750 } 
  let pt14 =  { x = 0.000; y = 0.750 } 
  let pt15 =  { x = 0.000; y = 0.625 } 
  let pt16 =  { x = 0.250; y = 0.625 } 
  let pts = [ pt01; pt02; pt03; pt04; pt05; pt06; pt07; pt08; pt09; pt10; pt11; pt12; pt13; pt14; pt15; pt16 ]
  Polygon { points = pts }

let filled (shapes : Shape list) : (string * Shape) list = 
  shapes |> List.map (fun s -> ("filled", s))

let byteman = filled [ bytemanPolygon ]
