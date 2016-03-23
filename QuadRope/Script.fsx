#load "Utils.fs"
#load "QuadRope.fs"
#load "QuadRope.Parallel.fs"

#nowarn "62"

open RadTrees
open RadTrees.QuadRope
open RadTrees.QuadRope.Path

let next (a, b) =
    match QuadRope.Parallel.next a b with
        | QuadRope.Parallel.Done rp -> rp, Top
        | QuadRope.Parallel.More (rp, path) -> rp, path

let l0 = hcat (init 1 3 (*)) (init 1 5 (*))
let l1 = hcat (init 1 4 (*)) (init 1 3 (*))
let l2 = hcat l1 (hcat l1 l0)
let l = hcat (init 1 3 (*)) l2

balanceH l
