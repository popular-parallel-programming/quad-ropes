#load "Utils.fs"
#load "ViewArray2D.fs"
#load "QuadRope.fs"
#load "QuadRope.Parallel.fs"

#nowarn "62"

open RadTrees

let rope = QuadRope.init 10 10 (*)
let a = QuadRope.init 4 4 (*);
let b = QuadRope.hcat a a;;
let c = QuadRope.vcat a a;;

let rec dontimes n x f =
    if n = 0 then
        x
    else
        dontimes (n - 1) (f x) f

let hcatn n =
    let q = QuadRope.initZeros 1 1
    let mutable p = q
    for i in 1 .. n do
        p <- QuadRope.hcat p q
    p
