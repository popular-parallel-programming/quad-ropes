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
    { offset = 0;
      rowStride = 0;
      colStride = 0;
      rows = 0;
      cols = 0;
      data = null }


/// Instantiate a new array slice that allows accessing the entire
/// array.
let make rows cols arr =
    if Array.isEmpty arr then
        emptySlice
    else
        { offset = 0;
          rowStride = cols;
          colStride = 1;
          rows = rows
          cols = cols
          data = arr }


/// The height of an array slice.
let inline rows slc = slc.rows


/// The width of an array slice.
let inline cols slc = slc.cols


let inline idx slc i j =
    slc.offset + i * slc.rowStride + j * slc.colStride


let inline fastGet slc i j =
    slc.data.[idx slc i j]


/// Return the value are i, j.
let get slc i j =
    if (rows slc) <= i then
        invalidArg "i" "May not access array slice outside of specified bounds."
    if (cols slc) <= j then
        invalidArg "j" "May not access array slice outside of specified bounds."
    fastGet slc i j


/// Slice up an array slice. This is a constant time operation and no
/// arrays are re-allocated.
let slice i j h w slc =
    let rowOff = max 0 i
    let colOff = max 0 j
    let rows = max 0 (min (rows slc - rowOff) h)
    let cols = max 0 (min (cols slc - colOff) w)

    // Keep data array and stride, adjust offsets for stride to
    // move offset in correct direction.
    { slc with offset = slc.offset + (rowOff * slc.rowStride) + (colOff * slc.colStride);
               rows = rows;
               cols = cols }


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
    make 1 1 [| v |]


/// True if array slice contains only a single element.
let isSingleton slc =
    rows slc = 1 && cols slc = 1


/// True if array slice contains no elements.
let isEmpty slc =
    rows slc <= 0 || cols slc <= 0


/// Revert an array slice in first dimension.
let vrev slc =
    { slc with offset = slc.offset + slc.rowStride * (slc.rows - 1);
               rowStride = -slc.rowStride; }


/// Revert an array slice in second dimension.
let hrev slc =
    { slc with offset = slc.offset + slc.colStride * (slc.cols - 1);
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


let transpose slc =
    { slc with rowStride = slc.rowStride;
               colStride = slc.colStride;
               rows = slc.cols;
               cols = slc.rows; }


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


/// Just write imperatively.
let write slc i j v =
    slc.data.[idx slc i j] <- v


/// Same as fill but for optimized closures.
let internal fillOpt slc f =
    for i in 0 .. slc.rows - 1 do
        for j in 0 .. slc.cols - 1 do
            write slc i j (Functions.invoke2 f i j)


/// Initialize a hopefully empty ArraySlice.
let internal fill slc f =
    fillOpt slc (Functions.adapt2 f)


let internal fillOffset slc i j f =
    fill slc (fun x y -> f (i + x) (j + y))

/// Convenience function to create a new empty ArraySlice that can be
/// initialized using init.
let internal zeroCreate h w =
    make h w (Array.zeroCreate (h * w))


let internal init h w f =
    let slc = zeroCreate h w
    fill slc f;
    slc


/// Copy the underlying array, set the value at i, j to v and return a
/// new array slice from the copy.
let set slc i j v =
    let slc' = zeroCreate slc.rows slc.cols
    slc'.data.[idx slc' i j] <- v;
    slc'


let fromArray2D arr =
    let slc = zeroCreate (Array2D.length1 arr) (Array2D.length2 arr)
    Array2D.iteri (write slc) arr;
    slc


/// Concatenate two array slices in first dimension.
let vcat left right =
    if cols left <> cols right then
        invalidArg "right" "length2 must be equal."
    let slc = zeroCreate (rows left + rows right) (cols left)
    iteri (write slc) left;
    iteri (fun i j v -> write slc (i + rows left) j v) right;
    slc


/// Concatenate two arrays in second dimension.
let hcat left right =
    if rows left <> rows right then
        invalidArg "right" "length1 must be equal."
    let slc = zeroCreate (rows left) (cols left + cols right)
    iteri (write slc) left;
    iteri (fun i j v -> write slc i (j + cols left) v) right;
    slc
