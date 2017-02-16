module RadTrees.Examples.SmithWaterman

open RadTrees

let private strlen = String.length

/// Compute the max of a and b by means of f.
let private maxBy f a b =
    if f a > f b then a else b


/// Compute the score after Smith-Waterman.
let private swscore row diag col s =
    max (max (diag + s) (max (row - 1) (col - 1))) 0


module QuadRope =

    /// Find the maximum value in scores and return its index.
    let rec private findMax scores =
        QuadRope.mapi (fun i j s -> (i, j), s) scores
        |> QuadRope.reduce (maxBy snd) ((0, 0), 0)
        |> fst


    /// Find the next highest index during backtracking.
    let private findNextHighest i j scores =
        let scores' = QuadRope.set (QuadRope.slice (i - 1) (j - 1) 2 2 scores) 1 1 -1
        let i', j' = findMax scores'
        i - 1 + i', j - 1 + j'


    /// Backtrack through a score matrix from some starting index pair i
    /// and j.
    let rec private backtrace (i, j) scores trace =
        let score = QuadRope.get scores i j
        let trace' = trace + score
        if score = 0 then
            trace'
        else
            backtrace (findNextHighest i j scores) scores trace'


    /// Compute the alignment cost of two sequences a and b.
    let align a b =
        let scores =
            QuadRope.init (strlen a) (strlen b) (fun i j -> if a.[i] = b.[j] then 1 else - 1)
            |> QuadRope.scan swscore 0
        backtrace (findMax scores) scores 0



module Array2D =

    /// Find the maximum value in scores and return its index.
    let rec private findMax scores =
        Array2D.mapi (fun i j s -> (i, j), s) scores
        |> Array2D.reduce (maxBy snd)
        |> fst


    /// Find the next highest index during backtracking.
    let private findNextHighest i j scores =
        let scores' = Array2D.set (Array2D.slice (i - 1) (j - 1) 2 2 scores) 1 1 -1
        let i', j' = findMax scores'
        i - 1 + i', j - 1 + j'


    /// Backtrack through a score matrix from some starting index pair i
    /// and j.
    let rec private backtrace (i, j) scores trace =
        let score = Array2D.get scores i j
        let trace' = trace + score
        if score = 0 then
            trace'
        else
            backtrace (findNextHighest i j scores) scores trace'


    /// Compute the alignment cost of two sequences a and b.
    let align a b =
        let scores =
            Array2D.init (strlen a) (strlen b) (fun i j -> if a.[i] = b.[j] then 1 else - 1)
            |> Array2D.scan swscore 0
        backtrace (findMax scores) scores 0
