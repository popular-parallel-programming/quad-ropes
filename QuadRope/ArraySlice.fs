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

module internal QuadRope.ArraySlice

open QuadRope
open QuadRope.Types
open QuadRope.Utils

/// This is the empty slice. It does not refer to any array and you
/// cannot retrieve any values from it.
let emptySlice =
    { rowOff = 0;
      colOff = 0;
      rowStride = 1;
      colStride = 1;
      rows = 0;
      cols = 0;
      data = null }


/// Instantiate a new array slice that allows accessing the entire
/// array.
let make arr =
    if Array2DExt.isEmpty arr then
        emptySlice
    else
        { rowOff = 0;
          colOff = 0;
          rowStride = 1;
          colStride = 1;
          rows = Array2D.length1 arr;
          cols = Array2D.length2 arr;
          data = arr }


/// Instantiate a new array slice that allows accessing the array only
/// in the specified area.
let makeSlice i j h w arr =
    let i' = max 0 i
    let j' = max 0 j
    if (Array2DExt.isEmpty arr
        || h <= 0 || w <= 0
        || Array2D.length1 arr <= i' || Array2D.length2 arr <= j')
    then
        emptySlice
    else
        { rowOff = max 0 i;
          colOff = max 0 j;
          rowStride = 1;
          colStride = 1;
          rows = max 0 (min h (Array2D.length1 arr));
          cols = max 0 (min w (Array2D.length2 arr));
          data = arr }


/// The height of an array slice.
let inline rows slc = slc.rows


/// The width of an array slice.
let inline cols slc = slc.cols


let inline fastGet slc i j =
    slc.data.[slc.rowOff + i * slc.rowStride,
              slc.colOff + j * slc.colStride]


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
    let data = Array2D.init slc.rows slc.cols (fastGet slc)
    data.[i, j] <- v
    make data


/// Slice up an array slice. This is a constant time operation and no
/// arrays are re-allocated.
let slice i j h w slc =
    if i <= 0 && j <= 0 && rows slc <= h && cols slc <= w then
        slc
    else if slc.rowOff < i || slc.colOff < j || h < 0 || w < 0 then
        emptySlice
    else
        let i = max 0 i
        let j = max 0 j
        let h = max 0 (min (rows slc - i) h)
        let w = max 0 (min (cols slc - j) w)

        // Keep data array and stride, adjust offsets for stride to
        // move offset in correct direction.
        { slc with rowOff = slc.rowOff + (i * slc.rowStride);
                   colOff = slc.colOff + (j * slc.colStride);
                   rows = h;
                   cols = w }


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
    make (Array2DExt.singleton v)


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
    let data = Array2D.zeroCreate (rows left + rows right) (cols left)
    // Two for loops are faster than lambda with conditional.
    for j in 0 .. Array2D.length2 data - 1 do
        for i in 0 .. left.rows - 1 do
            data.[i, j] <- fastGet left i j
        for i in 0 .. right.rows - 1 do
            data.[left.rows + i, j] <- fastGet right i j
    make data


/// Concatenate two arrays in second dimension.
let cat2 left right =
    if rows left <> rows right then
        invalidArg "right" "length1 must be equal."
    let data = Array2D.zeroCreate (rows left) (cols left + cols right)
    for i in 0 .. Array2D.length1 data - 1 do
        for j in 0 .. left.cols - 1 do
            data.[i, j] <- fastGet left i j
        for j in 0 .. right.cols - 1 do
            data.[i, left.cols + j] <- fastGet right i j
    make data


/// Revert an array slice in first dimension.
let hrev slc =
    { slc with rowOff = slc.rowOff + slc.rowStride * (slc.rows - 1);
               rowStride = -slc.rowStride }



/// Revert an array slice in second dimension.
let vrev slc =
    { slc with colOff = slc.colOff + slc.colStride * (slc.cols - 1);
               colStride = -slc.colStride }


/// Map a function f to all values in the array and combine the
/// results using g.
let mapreduce f g epsilon slc =
    let g' = Functions.adapt2 g
    let mutable acc = epsilon
    for i in 0 .. rows slc - 1 do
        for j in 0 .. cols slc - 1 do
            acc <- Functions.invoke2 g' acc (f (fastGet slc i j))
    acc


let reduce f arr = mapreduce id f arr


/// Initialize a 2D array with all zeros.
let initZeros h w =
    make (Array2DExt.initZeros h w)


let transpose slc =
    make (Array2D.init slc.cols slc.rows (fun i j -> fastGet slc j i))


/// Iterate over all elements in row-first order.
let iter f slc =
    for i in 0 .. rows slc - 1 do
        for j in 0 .. cols slc - 1 do
            f (fastGet slc i j)


/// Same as iteri but for optimized closures.
let internal iteriOpt f slc =
    for i in 0 .. rows slc - 1 do
        for j in 0 .. cols slc - 1 do
            Functions.invoke3 f i j (fastGet slc i j)


/// Iterate over all elements in row-first order and pass indices to
/// iteration function.
let iteri f slc =
    iteriOpt (Functions.adapt3 f) slc


/// Same as iteri2 but for optimized closures.
let internal iteriOpt2 f left right =
    if rows left <> rows right || cols left <> cols right then
        invalidArg "right" "length1 and length2 must be equal."
    for i in 0 .. rows left - 1 do
        for j in 0 .. cols left - 1 do
            Functions.invoke4 f i j (fastGet left i j) (fastGet right i j)


/// Iterate over all elements of two slices in row-first order and
/// pass indices to the iteration function.
let iteri2 f left right =
    iteriOpt2 (Functions.adapt4 f) left right


/// Initialize a hopefully empty ArraySlice.
let internal init slc f =
    for i in slc.rowOff .. slc.rowStride .. slc.rowOff + slc.rows - 1 do
        for j in slc.colOff .. slc.colStride .. slc.colOff + slc.cols - 1 do
            slc.data.[i, j] <- Functions.invoke2 f i j


/// Convenience function to create a new empty ArraySlice that can be
/// initialized using init.
let internal zeroCreate h w =
    make (Array2D.zeroCreate h w)
