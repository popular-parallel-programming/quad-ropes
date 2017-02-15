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

let rec dontimes n f s =
    match n with
        | 0 -> s
        | n -> f n (dontimes (n - 1) f s)

let adversarial =
    let rec hcat qr n =
        let qr' = QuadRope.create (QuadRope.rows qr) 1 0
        let qr'' = QuadRope.hcat qr qr'
        if n <= 0 then qr'' else vcat qr'' (n - 1)
    and     vcat qr n =
        let qr' = QuadRope.create 1 (QuadRope.cols qr) 0
        let qr'' = QuadRope.vcat qr qr'
        if n <= 0 then qr'' else hcat qr'' (n - 1)
    hcat

let q = adversarial (QuadRope.init 1 1 (+)) 10


// Amino acid sequence alignment on quad ropes.
let strlen = String.length

let rec findMaxIdx c i j scores =
    let c' = max c (QuadRope.get scores i j)
    if j < QuadRope.cols scores - 1 then
        findMaxIdx c' i (j + 1) scores
    else if i < QuadRope.rows scores - 1 then
        findMaxIdx c' (i + 1) 0 scores
    else
        i, j

let findNextIdx i j scores =
    let a = QuadRope.get scores (i - 1) j
    let b = QuadRope.get scores i (j - 1)
    let c = QuadRope.get scores (i - 1) (j - 1)
    if a > b && a > c then
        i - 1, j
    else if b > a && b > c then
        i, j - 1
    else
        i - 1, j - 1

let rec backtrace (i, j) scores trace =
    let score = QuadRope.get scores i j
    let trace' = QuadRope.hcat (QuadRope.singleton score) trace
    if score = 0 then
        trace'
    else
        backtrace (findNextIdx i j scores) scores trace'

let align a b =
    let f row diag col score =
        max (max (diag + score) (max (row - 1) (col - 1))) 0
    let scores =
        QuadRope.init (strlen a) (strlen b) (fun i j -> if a.[i] = b.[j] then 1 else - 1)
        |> QuadRope.scan f 0
    backtrace (findMaxIdx 0 0 0 scores) scores Empty
