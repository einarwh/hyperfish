module Shape

open Vector

type PolygonShape = 
  { points : Vector list }

type PolylineShape = 
  { pts : Vector list }

type CurveShape = 
  { point1 : Vector
    point2 : Vector
    point3 : Vector
    point4 : Vector }

type CircleShape = 
  { center : Vector 
    radius : float }

type BezierSegment = 
  { controlPoint1 : Vector
    controlPoint2 : Vector
    endPoint : Vector }

type LineSegment = 
  { targetPoint : Vector}

type PathSegment = 
  | LineSegment of LineSegment
  | BezierSegment of BezierSegment

type LineShape = 
  { lineStart : Vector 
    lineEnd : Vector}

type PathShape = 
  { start : Vector 
    close : bool 
    segments : PathSegment list }

type Shape =
  | Line of LineShape
  | Circle of CircleShape
  | Polygon of PolygonShape
  | Polyline of PolylineShape
  | Curve of CurveShape
  | Path of PathShape
