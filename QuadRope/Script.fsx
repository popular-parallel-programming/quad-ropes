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

/// Compute the max of a and b by means of f.
let maxBy f a b = if f a > f b then a else b

/// Find the maximum value in scores and return its index.
let rec findMax scores =
    QuadRope.mapi (fun i j s -> (i, j), s) scores
    |> QuadRope.reduce (maxBy snd) ((0, 0), 0)
    |> fst

/// Find the next highest index during backtracking.
let findNextHeigest i j scores =
    let scores' = QuadRope.set (QuadRope.slice (i - 1) (j - 1) 2 2 scores) 1 1 -1
    let i', j' = findMax scores'
    i - 1 + i', j - 1 + j'

/// Backtrack through a score matrix from some starting index pair i
/// and j.
let rec backtrace (i, j) scores trace =
    let score = QuadRope.get scores i j
    let trace' = trace + score
    if score = 0 then
        trace'
    else
        backtrace (findNextHighest i j scores) scores trace'

/// Compute the score after Smith-Waterman.
let swscore row diag col s =
    max (max (diag + s) (max (row - 1) (col - 1))) 0

/// Compute the alignment cost of two sequences a and b.
let align a b =
    let scores =
        QuadRope.init (strlen a) (strlen b) (fun i j -> if a.[i] = b.[j] then 1 else - 1)
        |> QuadRope.scan swscore 0
    backtrace (findMax scores) scores 0
