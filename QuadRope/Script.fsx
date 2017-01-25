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

let print qr =
    let rec print n qr =
        let tabs = String.replicate n " "
        match qr with
            | HCat (_, _, _, _, a, b) ->
                sprintf "\n%s(hcat %s %s)" tabs (print (n + 1) a) (print (n + 1) b)
            | VCat (_, _, _, _, a, b) ->
                sprintf "\n%s(vcat %s %s)" tabs (print (n + 1) a) (print (n + 1) b)
            | Slice (_, _, _, _, qr) ->
                sprintf "/%s/" (print (n + 1) qr)
            | Sparse _ -> "."
            | Leaf _ -> "[]"
            | Empty -> "e"
    print 0 qr
