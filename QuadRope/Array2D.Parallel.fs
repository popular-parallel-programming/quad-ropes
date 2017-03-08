// Copyright (c) 2016 Florian Biermann, fbie@itu.dk

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

module RadTrees.Parallel.Array2DExt

open RadTrees.Utils
open RadTrees.Utils.Tasks

/// Initialize a 2D array in parallel.
let init h w f =
    let f' = Functions.adapt2 f
    let arr = Array2D.zeroCreate h w
    if h < w then
        parfor 0 w (fun j -> for i = 0 to h - 1 do arr.[i, j] <- Functions.invoke2 f' i j)
    else
        parfor 0 h (fun i -> for j = 0 to w - 1 do arr.[i, j] <- Functions.invoke2 f' i j)
    arr


/// Concatenate two arrays in first dimension.
let cat1 left right =
    if Array2D.length2 left <> Array2D.length2 right then invalidArg "right" "length2 must be equal."
    let l1 = Array2D.length1 left + Array2D.length1 right
    let l2 = Array2D.length2 left
    let l1l = Array2D.length1 left
    init l1 l2 (fun i j -> if i < l1l then left.[i, j] else right.[i - l1l, j])


/// Concatenate two arrays in second dimension.
let cat2 left right =
    if Array2D.length1 left <> Array2D.length1 right then invalidArg "right" "length1 must be equal."
    let l1 = Array2D.length1 left
    let l2 = Array2D.length2 left + Array2D.length2 right
    let l2l = Array2D.length2 left
    init l1 l2 (fun i j -> if j < l2l then left.[i, j] else right.[i, j - l2l])


/// Revert an array in first dimension.
let rev1 (arr : _ [,]) =
    let h = Array2D.length1 arr
    init h (Array2D.length2 arr) (fun i j -> arr.[h - i, j])


/// Revert an array in second dimension.
let rev2 (arr : _ [,]) =
    let w = Array2D.length2 arr
    init (Array2D.length1 arr) w (fun i j -> arr.[i, w - j])


/// Fold each column of a 2D array, calling state with each column to get the state.
let fold1 f state (arr : _ [,]) =
    let res = init 1 (Array2D.length2 arr) (fun _ j -> state j)
    parfor 0 (Array2D.length2 res) (fun j ->
                                    for i in 0 .. Array2D.length1 arr - 1 do
                                        res.[0, j] <- f res.[0, j] arr.[i, j])
    res


/// Fold each row of a 2D array, calling state with each row to get the state.
let fold2 f state (arr : _ [,]) =
    let res = init (Array2D.length1 arr) 1 (fun i _ -> state i)
    parfor 0 (Array2D.length1 arr) (fun i ->
                                    for j in 0 .. Array2D.length2 arr - 1 do
                                        res.[i, 0] <- f res.[i, 0] arr.[i, j])
    res


/// Apply f to all elements of arr in parallel.
let map f (arr : _ [,]) =
    init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> f arr.[i, j])


/// Apply f to all elements of arr0 and arr1 in parallel.
let map2 f (arr0 : _ [,]) (arr1 : _ [,]) =
    if Array2D.length1 arr0 <> Array2D.length1 arr1 || Array2D.length2 arr0 <> Array2D.length2 arr1 then
        invalidArg "arr1" "Both arrays must be of same shape."
    let f' = Functions.adapt2 f
    init (Array2D.length1 arr0)
         (Array2D.length2 arr0)
         (fun i j -> Functions.invoke2 f' arr0.[i, j] arr1.[i, j])


/// Reduce each column of a 2D array.
let mapReduce1 f g (arr : _ [,]) =
    init 1 (Array2D.length2 arr)
         (fun _ j ->
          let mutable acc = f arr.[0, j]
          for i in 1 .. Array2D.length1 arr - 1 do
              acc <- g acc (f arr.[i, j])
          acc)


/// Reduce each row of a 2D array.
let mapReduce2 f g (arr : _ [,]) =
    init (Array2D.length1 arr) 1
         (fun i _ ->
          let mutable acc = f arr.[i, 0]
          for j in 1 .. Array2D.length2 arr - 1 do
              acc <- g acc (f arr.[i, j])
          acc)


let reduce1 f arr = mapReduce1 id f arr
let reduce2 f arr = mapReduce2 id f arr


/// Map a function f to all values in the array and combine the
/// results using g.
let mapReduce f g (arr : _ [,]) =
    let g' = Functions.adapt2 g
    let mapReduceView i0 j0 h w =
        let h = min (i0 + h) (Array2D.length1 arr)
        let w = min (j0 + w) (Array2D.length2 arr)
        let mutable acc = f arr.[i0, j0]
        for j in j0 .. w - 1 do
            acc <- g acc (f arr.[i0, j])
        for i in i0 + 1 .. h - 1 do
            for j in j0 .. w - 1 do
                acc <- Functions.invoke2 g' acc (f arr.[i, j])
        acc
    let h = Array2D.length1 arr / numthreads()
    let mapReduceStep i =
        mapReduceView (i * h) 0 h (Array2D.length2 arr)
    Array.reduce g (Array.Parallel.init (numthreads()) mapReduceStep)


let reduce f arr = mapReduce id f arr


let transpose arr =
    init (Array2D.length2 arr) (Array2D.length1 arr) (fun i j -> arr.[j, i])
