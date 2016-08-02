module RadTrees.ArraySlice

open RadTrees
open Types

let inline make arr =
    ArraySlice (0, 0, Array2D.length1 arr, Array2D.length2 arr, arr)

let inline makeSlice i j h w arr =
    ArraySlice (max 0 i, max 0 j, min h (Array2D.length1 arr), min w (Array2D.length2 arr), arr)

let inline private sliceArray (ArraySlice (i, j, h, w, arr)) =
    Array2D.slice arr i j h w

let inline private apply f slice =
    make (f (sliceArray slice))

let inline private apply2 f slice0 slice1 =
    make (f (sliceArray slice0) (sliceArray slice1))

let inline length1 (ArraySlice (_, _, h0, _, _)) = h0

let inline length2 (ArraySlice (_, _, _, w0, _)) = w0

let get (ArraySlice (i0, j0, _, _, arr)) i j =
    arr.[i0 + i, j0 + j]

let set (ArraySlice (i0, j0, h0, w0, arr)) i j v =
    let arr0 = arr.[i0 .. i0 + h0, j0 .. j0 + w0]
    arr0.[i, j] <- v
    ArraySlice (0, 0, h0, w0, arr0)

let slice (ArraySlice (i0, j0, h0, w0, arr) as slice) i j h w =
    if i <= 0 && j <= 0 && h0 <= h && w0 <= w then
        slice
    else
        let i1 = max 0 i
        let j1 = max 0 j
        let h0 = min (h0 - i1) h
        let w0 = min (w0 - j1) w
        ArraySlice (i0 + i1, j0 + j1, h0, w0, arr)

let singleton v =
    make (Array2D.singleton v)

/// True if array contains only a single element.
let isSingleton (ArraySlice (_, _, h0, w0, _)) =
    h0 = 1 && w0 = 1

/// True if array contains no elements.
let isEmpty (ArraySlice (_, _, h0, w0, _)) =
    h0 = 0 || w0 = 0

/// Concatenate two arrays in first dimension.
let cat1 left right =
    apply2 Array2D.cat1 left right

/// Concatenate two arrays in second dimension.
let cat2 left right =
    apply2 Array2D.cat2 left right

/// Revert an array in first dimension.
let rev1 slice =
    apply Array2D.rev1 slice

/// Revert an array in second dimension.
let rev2 slice =
    apply Array2D.rev2 slice

/// Fold each column of a 2D array, calling state with each column to get the state.
let fold1 f state slice =
    apply (Array2D.fold1 f state) slice

/// Fold each row of a 2D array, calling state with each row to get the state.
let fold2 f state slice =
    apply (Array2D.fold2 f state) slice

/// Compute the column-wise prefix sum for f.
let scan1 f state slice =
    apply (Array2D.scan1 f state) slice

/// Compute the row-wise prefix sum for f.
let scan2 f state slice =
    apply (Array2D.scan2 f state) slice

let map f slice =
    apply (Array2D.map f) slice

let mapi f slice =
    apply (Array2D.mapi f) slice

let map2 f left right =
    apply2 (Array2D.map2 f) left right

/// Reduce each column of a 2D array.
let mapReduce1 f g slice =
    apply (Array2D.mapReduce1 f g) slice

/// Reduce each row of a 2D array.
let mapReduce2 f g slice =
    apply (Array2D.mapReduce2 f g) slice

let reduce1 f arr = mapReduce1 id f arr
let reduce2 f arr = mapReduce2 id f arr

/// Map a function f to all values in the array and combine the
/// results using g.
let mapReduce f g slice =
    Array2D.mapReduce f g (sliceArray slice)

let reduce f arr = mapReduce id f arr

let sort1 p slice =
    apply (Array2D.sort1 p) slice

let sort2 p slice =
    apply (Array2D.sort2 p) slice

/// Initialize a 2D array with all zeros.
let initZeros h w =
    make (Array2D.initZeros h w)

let transpose slice =
    apply Array2D.transpose slice

let filter1 p slice =
    apply (Array2D.filter1 p) slice

let filter2 p slice =
    apply (Array2D.filter2 p) slice

let iter f (ArraySlice (i, j, h, w, arr)) =
    for x in i .. i + h - 1 do
        for y in j .. j + w - 1 do
            f arr.[x, y]

let iteri f (ArraySlice (i, j, h, w, arr)) =
    for x in 0 .. h - 1 do
        for y in 0 .. w - 1 do
            f x y arr.[x + i, y + j]
