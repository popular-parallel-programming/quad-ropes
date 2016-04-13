#load "Utils.fs"
#load "ViewArray2D.fs"
#load "QuadRope.fs"
#load "QuadRope.Parallel.fs"

#nowarn "62"

open RadTrees
open RadTrees.QuadRope
open RadTrees.QuadRope.Parallel

let mutable calls = 3
let cond () =
    if calls <= 0 then
        true
    else
        calls <- (calls - 1)
        false

let unpack = function
    | Done rope -> rope, Empty
    | More (ps, us) -> ps, us
