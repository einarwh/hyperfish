module Herring

open Vector
open Shape

let pt x y = { x = x; y = y }

let curveto cp1 cp2 ep : PathSegment = 
  BezierSegment { controlPoint1 = cp1; controlPoint2 = cp2; endPoint = ep }

let lineto p : PathSegment = 
  LineSegment { targetPoint = p }

let lizardBodySegments : PathSegment list = 
  [ 
    curveto (pt 0.020 0.050) (pt 0.030 0.120) (pt 0.025 0.185)
    curveto (pt 0.080 0.120) (pt 0.200 0.105) (pt 0.310 0.090)
    lineto  (pt 0.310 -0.313)
    curveto (pt 0.450 -0.170) (pt 0.500 -0.100) (pt 0.630 0.065)
    curveto (pt 0.700 0.040) (pt 0.780 0.010) (pt 0.850 0.000)
    curveto (pt 0.700 -0.070) (pt 0.563 -0.180) (pt 0.563 -0.313)
    curveto (pt 0.680 -0.310) (pt 0.780 -0.410) (pt 0.813 -0.375)
    curveto (pt 0.830 -0.350) (pt 0.780 -0.300) (pt 0.750 -0.250)
    lineto  (pt 1.000 0.000)
    lineto  (pt 0.750 0.250)
    lineto  (pt 0.870 0.500)
    curveto (pt 0.950 0.675) (pt 1.060 0.795) (pt 1.270 0.850)
    curveto (pt 1.200 0.940) (pt 1.100 0.980) (pt 1.000 1.000)
    curveto (pt 0.980 0.900) (pt 0.940 0.800) (pt 0.850 0.730)
    curveto (pt 0.795 0.940) (pt 0.675 1.050) (pt 0.500 1.130)
    lineto  (pt 0.250 1.250) 
    lineto  (pt 0.000 1.000)
    lineto  (pt 0.250 0.750)
    curveto (pt 0.300 0.780) (pt 0.350 0.830) (pt 0.375 0.813)
    curveto (pt 0.410 0.780) (pt 0.310 0.680) (pt 0.313 0.563)
    curveto (pt 0.180 0.563) (pt 0.070 0.700) (pt 0.000 0.850)
    curveto (pt -0.010 0.780) (pt -0.040 0.700) (pt -0.065 0.630)
    curveto (pt 0.100 0.500) (pt 0.170 0.450) (pt 0.313 0.310)
    lineto  (pt -0.090 0.310) 
    curveto (pt -0.105 0.200) (pt -0.120 0.080) (pt -0.185 0.025)
    curveto (pt -0.120 0.030) (pt -0.050 0.020) (pt 0.000 0.000) 
  ]

let lizardBody = 
  let pathShape = 
    { start = pt 0. 0.
      close = true 
      segments = lizardBodySegments }
  Path pathShape

let lizardSpine = 
  let segments = 
    [ lineto  (pt 0.270 1.000)
      curveto (pt 0.740 1.000) (pt 0.645 0.480) (pt 0.350 -0.210) ]
  let pathShape = 
    { start = pt 0.090 1.000
      close = false 
      segments = segments }
  Path pathShape

let lowerEye = 
  Circle { center = (pt 0.270 0.880); radius = 1. }

let upperEye = 
  Circle { center = (pt 0.270 1.120); radius = 1. }

let herringPath = 
  let segments = 
    [ lineto (pt 0.750 0.000)
      lineto (pt 1.000 0.250) 
      lineto (pt 0.750 0.250)
      curveto (pt 0.350 0.250) (pt 0.100 0.500) (pt 0.075 0.925)
      curveto (pt 0.500 0.900) (pt 0.750 0.650) (pt 0.750 0.250) ]
  let pathShape = 
    { start = pt 0.750 0.250 
      close = true 
      segments = segments }
  Path pathShape



let fishEye = 
  Circle { center = (pt 0.225 0.775); radius = 0.6 }

let leftCurve = 
  let curveShape = 
    { point1 = pt 0.000 0.000
      point2 = pt 0.100 0.125
      point3 = pt 0.100 0.375 
      point4 = pt 0.000 0.500 }
  Curve curveShape

let rightCurve = 
  let curveShape = 
    { point1 = pt 1.000 0.500
      point2 = pt 0.900 0.625
      point3 = pt 0.900 0.875 
      point4 = pt 1.000 1.000 }
  Curve curveShape

let gillCurve = 
  let curveShape = 
    { point1 = pt 0.162 0.580
      point2 = pt 0.300 0.600
      point3 = pt 0.400 0.700 
      point4 = pt 0.420 0.838 }
  Curve curveShape

let herringShapes = 
  [ 
    "herring", herringPath
    "herring", fishEye
    "herring", leftCurve
    "herring", rightCurve
    "herring", gillCurve
  ]

let herring = 
  herringShapes 
