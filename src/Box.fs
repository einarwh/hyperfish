module Box

open Vector

type Box = { a : Vector; b : Vector; c : Vector}

let turnBox { a = a; b = b; c = c } = 
  { a = a + b; b = c; c = -b }

