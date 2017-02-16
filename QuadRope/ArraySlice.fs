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

module internal RadTrees.ArraySlice

open RadTrees
open Types


/// This is the empty slice. It does not refer to any array and you
/// cannot retrieve any values from it.
let emptySlice = { r = 0; c = 0; h = 0; w = 0; vals = null }


/// Instantiate a new array slice that allows accessing the entire
/// array.
let make arr =
    if Array2D.isEmpty arr then
        emptySlice
    else
        { r = 0; c = 0; h = Array2D.length1 arr; w = Array2D.length2 arr; vals = arr }


/// Instantiate a new array slice that allows accessing the array only
/// in the specified area.
let makeSlice i j h w arr =
    let i' = max 0 i
    let j' = max 0 j
    if (Array2D.isEmpty arr
        || h <= 0 || w <= 0
        || Array2D.length1 arr <= i' || Array2D.length2 arr <= j')
    then
        emptySlice
    else
        { r = max 0 i
          c = max 0 j
          h = max 0 (min h (Array2D.length1 arr))
          w = max 0 (min w (Array2D.length2 arr))
          vals = arr }


/// Compute a new array from a slice.
let inline private sliceArray slc =
    Array2D.slice slc.r slc.c slc.h slc.w slc.vals


/// Compute a new array from a slice, apply function f to it and make
/// a new array slice from the resulting array.
let inline private apply f slc = make (f (sliceArray slc))


/// The height of an array slice.
let inline length1 slc = slc.h
let inline rows slc = slc.h


/// The width of an array slice.
let inline length2 slc = slc.w
let inline cols slc = slc.w


// Handy for iterating.
let inline minr slc = slc.r
let inline maxr slc = slc.r + slc.h - 1
let inline minc slc = slc.c
let inline maxc slc = slc.c + slc.w - 1


let inline fastGet slc i j = slc.vals.[slc.r + i, slc.c + j]


/// Return the value are i, j.
let get slc i j =
    if (rows slc) <= i then
        invalidArg "i" "May not access array slice outside of specified bounds."
    if (cols slc) <= j then
        invalidArg "j" "May not access array slice outside of specified bounds."
    fastGet slc i j


/// Copy the underlying array, set the value at i, j to v and return a
/// new array slice from the copy.
let set slc i j v =
    let vals = slc.vals.[minr slc .. maxr slc, minc slc .. maxc slc]
    vals.[i, j] <- v
    make vals


/// Slice up an array slice. This is a constant time operation and no
/// arrays are re-allocated.
let slice i j h w slc =
    if i <= 0 && j <= 0 && rows slc <= h && cols slc <= w then
        slc
    else if slc.h < i || slc.w < j || h < 0 || w < 0 then
        emptySlice
    else
        let i = max 0 i
        let j = max 0 j
        let h = max 0 (min (rows slc - i) h)
        let w = max 0 (min (cols slc - j) w)
        { slc with r = minr slc + i; c = minc slc + j; h = h; w = w }


/// Split into four slices, each differing in size by at most one row
/// or column. Returned order is NE, NW, SW and SE.
let split4 slc =
    let h = rows slc >>> 1
    let w = cols slc >>> 1
    slice 0 w  h      (w + 1) slc,
    slice 0 0  h       w      slc,
    slice h 0 (h + 1)  w      slc,
    slice h w (h + 1) (w + 1) slc


/// Split into two slices, each differing in size by at most one
/// column. Returned order is W, E.
let hsplit2 slc =
    let h = rows slc
    let w = cols slc >>> 1
    slice 0 0 h w slc, slice 0 w h (w + 1) slc


/// Split into two slices, each differing in size by at most one
/// row. Returned order is N, S.
let vsplit2 slc =
    let h = rows slc >>> 1
    let w = cols slc
    slice 0 0 h w slc, slice h 0 (h + 1) w slc


// Convenience function that allow for splitting slices into equal
// sized parts in parallel.

let inline leftHalf slc =
    slice 0 0 (rows slc) (cols slc >>> 1) slc


let inline rightHalf slc =
    slice 0 (cols slc >>> 1) (rows slc) (cols slc) slc


let inline upperHalf slc =
    slice 0 0 (rows slc >>> 1) (cols slc) slc


let inline lowerHalf slc =
    slice (rows slc >>> 1) 0 (rows slc) (cols slc) slc


let inline nw slc =
    slice 0 0 (rows slc >>> 1) (cols slc >>> 1) slc


let inline ne slc =
    slice 0 (cols slc >>> 1) (rows slc >>> 1) (cols slc) slc


let inline sw slc =
    slice (rows slc >>> 1) 0 (rows slc) (cols slc >>> 1) slc


let inline se slc =
    slice (rows slc >>> 1) (cols slc >>> 1) (rows slc) (cols slc) slc


/// Produce a singleton array slice.
let singleton v =
    make (Array2D.singleton v)


/// True if array slice contains only a single element.
let isSingleton slc =
    rows slc = 1 && cols slc = 1


/// True if array slice contains no elements.
let isEmpty slc =
    rows slc <= 0 || cols slc <= 0


/// Concatenate two array slices in first dimension.
let cat1 left right =
    if cols left <> cols right then
        invalidArg "right" "length2 must be equal."
    let vals = Array2D.zeroCreate (rows left + rows right) (cols left)
    Array2D.blit left.vals  0 0 vals  0          0 (rows left)  (cols left)
    Array2D.blit right.vals 0 0 vals (rows left) 0 (rows right) (cols right)
    make vals


/// Concatenate two arrays in second dimension.
let cat2 left right =
    if rows left <> rows right then
        invalidArg "right" "length1 must be equal."
    let vals = Array2D.zeroCreate (rows left) (cols left + cols right)
    Array2D.blit left.vals  0 0 vals 0  0          (rows left)  (cols left)
    Array2D.blit right.vals 0 0 vals 0 (cols left) (rows right) (cols right)
    make vals


/// Revert an array slice in first dimension.
let rev1 slice =
    apply Array2D.rev1 slice


/// Revert an array slice in second dimension.
let rev2 slice =
    apply Array2D.rev2 slice


/// Fold each column of an array slice, calling state with each column
/// to get the corresponding state.
let fold1 f state slc =
    make (Array2D.init 1 slc.w
                       (fun _ y ->
                        let mutable acc = state y
                        for x in slc.r .. slc.r + slc.h - 1 do
                            acc <- f acc slc.vals.[x, slc.c + y]
                        acc))


/// Fold each row of an array slice, calling state with each row to
/// get the corresponding state.
let fold2 f state slc =
    make (Array2D.init slc.h 1
                       (fun x _ ->
                        let mutable acc = state x
                        for y in slc.c .. slc.c + slc.w - 1 do
                            acc <- f acc slc.vals.[slc.r + x, y]
                        acc))


/// Compute the column-wise prefix sum for f.
let scan1 f state slice =
    apply (Array2D.scan1 f state) slice


/// Compute the row-wise prefix sum for f.
let scan2 f state slice =
    apply (Array2D.scan2 f state) slice


/// Map a function f to all values in the array and combine the
/// results using g.
let mapreduce f g slc =
    // Pick a starting element.
    let mutable acc = f (fastGet slc 0 0)
    // Accumulate the first row, skip element at 0,0.
    for j in 1 .. cols slc - 1 do
        acc <- g acc (f (fastGet slc 0 j))
    // Now accumulate the remaining rows.
    for i in 1 .. rows slc - 1 do
        for j in 0 .. cols slc - 1 do
            acc <- g acc (f (fastGet slc i j))
    acc


let reduce f arr = mapreduce id f arr


let sort1 p slice =
    apply (Array2D.sort1 p) slice


let sort2 p slice =
    apply (Array2D.sort2 p) slice


/// Initialize a 2D array with all zeros.
let initZeros h w =
    make (Array2D.initZeros h w)


let transpose slc =
    make (Array2D.init slc.w slc.h (fun i j -> fastGet slc j i))


let filter1 p slice =
    apply (Array2D.filter1 p) slice


let filter2 p slice =
    apply (Array2D.filter2 p) slice


/// Iterate over all elements in row-first order.
let iter f slc =
    for i in 0 .. rows slc - 1 do
        for j in 0 .. cols slc - 1 do
            f (fastGet slc i j)


/// Iterate over all elements in row-first order and pass indices to
/// iteration function.
let iteri f slc =
    for i in 0 .. rows slc - 1 do
        for j in 0 .. cols slc - 1 do
            f i j (fastGet slc i j)


let iteri2 f left right =
    if rows left <> rows right || cols left <> cols right then
        invalidArg "right" "length1 and length2 must be equal."
    for i in 0 .. rows left - 1 do
        for j in 0 .. cols left - 1 do
            f i j (fastGet left i j) (fastGet right i j)


/// Initialize a hopefully empty ArraySlice.
let internal init slc f =
    for i in minr slc .. maxr slc do
        for j in minc slc .. maxc slc do
            slc.vals.[i, j] <- f i j


/// Convenience function to create a new empty ArraySlice that can be
/// initialized using init.
let internal zeroCreate h w =
    make (Array2D.zeroCreate h w)
