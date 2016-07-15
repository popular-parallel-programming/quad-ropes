module RadTrees.Parallel.Array2D

open RadTrees.Utils.Tasks

/// Initialize a 2D array in parallel.
let init h w f =
    array2D (Array.Parallel.init h (fun i -> Array.init w (fun j -> f i j)))

/// Concatenate two arrays in first dimension.
let cat1 left right =
    if Array2D.length2 left <> Array2D.length2 right then failwith "length2 must be equal!"
    let l1 = Array2D.length1 left + Array2D.length1 right
    let l2 = Array2D.length2 left
    let l1l = Array2D.length1 left
    init l1 l2 (fun i j -> if i < l1l then left.[i, j] else right.[i - l1l, j])

/// Concatenate two arrays in second dimension.
let cat2 left right =
    if Array2D.length1 left <> Array2D.length1 right then failwith "length1 must be equal!"
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
    let res = Array2D.init 1 (Array2D.length2 arr) (fun _ j -> state j)
    parfor 0 (Array2D.length2 res) (fun j ->
                                    for i in 0 .. Array2D.length1 arr - 1 do
                                        res.[0, j] <- f res.[0, j] arr.[i, j])
    res

/// Fold each row of a 2D array, calling state with each row to get the state.
let fold2 f state (arr : _ [,]) =
    let res = Array2D.init (Array2D.length1 arr) 1 (fun i _ -> state i)
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
        failwith "arrays must be of same shape."
    init (Array2D.length1 arr0) (Array2D.length2 arr0) (fun i j -> f arr0.[i, j] arr1.[i, j])

/// Reduce each column of a 2D array.
let mapReduce1 f g (arr : _ [,]) =
    let mapreduce j =
        let mutable acc = f arr.[0, j]
        for i in 1 .. Array2D.length1 arr - 1 do
            acc <- g acc (f arr.[i, j])
        acc
    let arr0 = Array.Parallel.init (Array2D.length2 arr) mapreduce
    init 1 (Array2D.length2 arr) (fun _ j -> arr0.[j])

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
    let mapReduceView i0 j0 h w =
        let h = min (i0 + h) (Array2D.length1 arr)
        let w = min (j0 + w) (Array2D.length2 arr)
        let mutable acc = f arr.[i0, j0]
        for j in j0 .. w - 1 do
            acc <- g acc (f arr.[i0, j])
        for i in i0 + 1 .. h - 1 do
            for j in j0 .. w - 1 do
                acc <- g acc (f arr.[i, j])
        acc
    let h = Array2D.length1 arr / numthreads()
    let mapReduceStep i =
        mapReduceView (i * h) 0 h (Array2D.length2 arr)
    Array.reduce g (Array.Parallel.init (numthreads()) mapReduceStep)

let reduce f arr = mapReduce id f arr

let transpose arr =
    init (Array2D.length2 arr) (Array2D.length1 arr) (fun i j -> arr.[j, i])
