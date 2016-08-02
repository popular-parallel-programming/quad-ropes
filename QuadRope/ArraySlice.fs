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
let cat1 (ArraySlice (i0, j0, h0, w0, arr0)) (ArraySlice (i1, j1, h1, w1, arr1)) =
    if w0 <> w1 then invalidArg "right" "length2 must be equal."
    let h = h0 + h1
    make (Array2D.init h w0 (fun i j -> if i < h0 then arr0.[i0 + i, j0 + j] else arr1.[i1 + i - h0, j1 + j]))

/// Concatenate two arrays in second dimension.
let cat2 (ArraySlice (i0, j0, h0, w0, arr0)) (ArraySlice (i1, j1, h1, w1, arr1)) =
    if h0 <> h1 then invalidArg "right" "length1 must be equal."
    let w = w0 + w1
    make (Array2D.init h0 w (fun i j -> if j < w0 then arr0.[i0 + i, j0 + j] else arr1.[i1 + i, j1 + j - w0]))

/// Revert an array in first dimension.
let rev1 slice =
    apply Array2D.rev1 slice

/// Revert an array in second dimension.
let rev2 slice =
    apply Array2D.rev2 slice

/// Fold each column of a 2D array, calling state with each column to get the state.
let fold1 f state (ArraySlice (i, j, h, w, arr)) =
    make (Array2D.init 1 w
                   (fun _ y ->
                    let mutable acc = state y
                    for x in i .. i + h - 1 do
                        acc <- f acc arr.[x, j + y]
                    acc))

/// Fold each row of a 2D array, calling state with each row to get the state.
let fold2 f state (ArraySlice (i, j, h, w, arr)) =
    make (Array2D.init h 1
                       (fun x _ ->
                        let mutable acc = state x
                        for y in j .. j + w - 1 do
                            acc <- f acc arr.[i + x, y]
                        acc))

/// Compute the column-wise prefix sum for f.
let scan1 f state slice =
    apply (Array2D.scan1 f state) slice

/// Compute the row-wise prefix sum for f.
let scan2 f state slice =
    apply (Array2D.scan2 f state) slice

let map f (ArraySlice (i, j, h, w, arr)) =
    make (Array2D.init h w (fun x y -> f arr.[i + x, j + y]))

let mapi f (ArraySlice (i, j, h, w, arr)) =
    make (Array2D.init h w (fun x y -> f x y arr.[i + x, j + y]))

let map2 f (ArraySlice (i0, j0, h0, w0, arr0)) (ArraySlice (i1, j1, _, _, arr1)) =
    make (Array2D.init h0 w0 (fun x y -> f arr0.[x + i0, y + j0] arr1.[x + i1, y + j1]))

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
let mapReduce f g (ArraySlice (i, j, h, w, arr)) =
    let mutable acc = f arr.[i, j]
    for y in j + 1 .. j + w - 1 do
        acc <- g acc (f arr.[i, y])
    for x in i + 1 .. i + h - 1 do
        for y in j .. j + w - 1 do
            acc <- g acc (f arr.[x, y])
    acc

let reduce f arr = mapReduce id f arr

let sort1 p slice =
    apply (Array2D.sort1 p) slice

let sort2 p slice =
    apply (Array2D.sort2 p) slice

/// Initialize a 2D array with all zeros.
let initZeros h w =
    make (Array2D.initZeros h w)

let transpose (ArraySlice (i, j, h, w, arr)) =
    make (Array2D.init w h (fun x y -> arr.[y + i, x + j]))

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
