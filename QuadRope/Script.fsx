#load "Types.fs"
#load "Utils.fs"
#load "Array2D.fs"
#load "Array2D.Parallel.fs"
#load "ArraySlice.fs"
#load "Target.fs"
#load "QuadRope.fs"
#load "QuadRope.Parallel.fs"
#load "Examples.fs"

#nowarn "62"

open RadTrees
open RadTrees.Types

let slice = QuadRope.slice
let create = QuadRope.create
let mmult = Examples.QuadRope.mmult
let pointwise = QuadRope.SparseDouble.pointwise
let materialize = QuadRope.materialize
let zip = QuadRope.zip


let n = 10

let q = QuadRope.hcat (create n (n - 1) 1.0) (create n 1 0.0)
let s = QuadRope.SparseDouble.upperDiagonal n 1.0

let sq = zip (*) s q
let qs = zip (*) q s

let equals a b = QuadRope.toArray2D a = QuadRope.toArray2D b

equals sq qs
