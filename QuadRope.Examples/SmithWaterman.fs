// Copyright (c) 2017 Florian Biermann, fbie@itu.dk

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:

// * The above copyright notice and this permission notice shall be
//   included in all copies or substantial portions of the Software.

// * The software is provided "as is", without warranty of any kind,
//   express or implied, including but not limited to the warranties of
//   merchantability, fitness for a particular purpose and
//   noninfringement. In no event shall the authors or copyright holders be
//   liable for any claim, damages or other liability, whether in an action
//   of contract, tort or otherwise, arising from, out of or in connection
//   with the software or the use or other dealings in the software.

module QuadRopes.Examples.SmithWaterman

open QuadRopes

let private strlen = String.length

/// Compute the max of a and b by means of f.
let private orderBy comp f a b =
    if comp (f a) (f b) then a, b else b, a


let maxScore a b =
    orderBy (>) (fst >> snd) a b
    ||> orderBy (>) (fst >> fst)
    ||> orderBy (>) snd |> fst


/// Return a function that computes the element-wise distance between
/// two strings.
let dist (a : string) (b : string) =
    fun i j -> if a.[i] = b.[j] then 1 else - 1


/// Compute the score after Smith-Waterman.
let private swscore row diag col s =
    max (max (diag + s) (max (row - 1) (col - 1))) 0


/// A kernel for backtracking through a score matrix.
let private btKernel row diag col score =
    max (max row diag) col + score


module QuadRope =

    let alignmentBuilder mapi reduce scan init =

        /// Find the maximum value in scores and return its index.
        let findMax =
            mapi (fun i j s -> (i, j), s)
            >> reduce maxScore ((0, 0), 0)
            >> fst


        /// Backtrack through a score matrix from some starting index pair i
        /// and j.
        let backtrack (i, j) scores =
            let scores' = QuadRope.slice 0 0 (i + 1) (j + 1) scores // Start from i, j
                          |> scan btKernel 0   // Take value at i, j as start.
            QuadRope.get scores' i j


        /// Compute the alignment cost of two sequences a and b.
        let align a b =
            let scores = init (strlen a) (strlen b) (dist a b) |> scan swscore 0
            backtrack (findMax scores) scores

        align


    let align =
        alignmentBuilder QuadRope.mapi
                         QuadRope.reduce
                         QuadRope.scan
                         QuadRope.init


    let alignPar =
        alignmentBuilder Parallel.QuadRope.mapi
                         Parallel.QuadRope.reduce
                         Parallel.QuadRope.scan
                         Parallel.QuadRope.init



module Array2D =

    /// Find the maximum value in scores and return its index.
    let private findMax =
        Array2D.mapi (fun i j s -> (i, j), s)
        >> Array2DExt.reduce maxScore
        >> fst


    /// Backtrack through a score matrix from some starting index pair i
    /// and j.
    let private backtrack (i, j) scores =
        let scores' = Array2DExt.slice 0 0 (i + 1) (j + 1) scores // Start from i, j
                      |> Array2DExt.scan btKernel 0   // Take value at i, j as start.
        Array2D.get scores' i j


    /// Compute the alignment cost of two sequences a and b.
    let align a b =
        let scores = Array2D.init (strlen a) (strlen b) (dist a b) |> Array2DExt.scan swscore 0
        backtrack (findMax scores) scores
