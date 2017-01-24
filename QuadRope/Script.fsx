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

let left = function
    | HCat (_, _, _, _, qr, _)
    | VCat (_, _, _, _, qr, _)
    | qr -> qr

let right = function
    | HCat (_, _, _, _, _, qr)
    | VCat (_, _, _, _, _, qr)
    | qr -> qr

let rec print = function
    | HCat (_, _, _, _, a, b) -> sprintf "(hcat %s %s)" (print a) (print b)
    | VCat (_, _, _, _, a, b) -> sprintf "(vcat %s %s)" (print a) (print b)
    | Slice (_, _, _, _, qr) -> sprintf "/%s/" (print qr)
    | Sparse _ -> "."
    | Leaf _ -> "[]"
    | Empty -> "e"
