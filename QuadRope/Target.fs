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

[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module internal QuadRope.Target

open QuadRope.Types
open QuadRope.Utils

/// A convenience wrapper for writing into a target array with
/// some offset.
type 'a Target = { i : int; j : int; vals : 'a [,] }


let inline rows tgt = Array2D.length1 tgt.vals - tgt.i
let inline cols tgt = Array2D.length2 tgt.vals - tgt.j


/// Create a new target descriptor of size h * w.
let inline make h w = { i = 0; j = 0; vals = Array2D.zeroCreate h w }
let inline makeWith h w v = { i = 0; j = 0; vals = Array2D.create h w v }


/// The "empty target", a target that is not initialized.
let empty = { i = 0; j = 0; vals = null }


/// True if the target is the empty target.
let inline isEmpty tgt =
    isNull tgt.vals


/// Advance the target by i and j in both dimensions.
let inline increment (tgt : _ Target) i j =
    { tgt with i = tgt.i + i; j = tgt.j + j }


/// To avoid having to think about fringes by using conditionals,
/// we simply extend the target array by one row and one column,
/// into which we load the initial value.
let inline makeWithFringe h w value =
    let tgt = make (h + 1) (w + 1)
    for i in 0 .. h do
        tgt.vals.[i, 0] <- value
    for j in 0 .. w do
        tgt.vals.[0, j] <- value
    increment tgt 1 1


let inline incrementRow tgt i = increment tgt i 0
let inline incrementCol tgt j = increment tgt 0 j


/// Generalized write to target.
let inline writemap (tgt : _ Target) f r c v =
    tgt.vals.[tgt.i + r, tgt.j + c] <- f v


let inline writemap2 (tgt : _ Target) f r c v1 v2 =
    tgt.vals.[tgt.i + r, tgt.j + c] <- Functions.invoke2 f v1 v2


/// Generalized write to target with index pairs.
let inline writemapi (tgt : _ Target) f r c v =
    tgt.vals.[tgt.i + r, tgt.j + c] <- Functions.invoke3 f r c v


let inline writemapi2 (tgt : _ Target) f r c v1 v2 =
    tgt.vals.[tgt.i + r, tgt.j + c] <- Functions.invoke4 f r c v1 v2


/// Simplified write to target.
let inline write (tgt : _ Target) r c v =
    writemap tgt id r c v


/// Fill the target descriptor with values.
let inline fill (tgt : _ Target) h w v =
    for r in tgt.i .. tgt.i + h - 1 do
        for c in tgt.j .. tgt.j + w - 1 do
            tgt.vals.[r, c] <- v


/// Build a leaf node from a target for a given height and width.
let inline toSlice (tgt : _ Target) h w =
    ArraySlice.makeSlice tgt.i tgt.j h w tgt.vals


/// Map a function to the values of a leaf and return a new leaf
/// instance. This writes to the underlying target array.
let inline map f slc (tgt : _ Target) =
    ArraySlice.iteri (writemap tgt f) slc
    toSlice tgt (ArraySlice.rows slc) (ArraySlice.cols slc)


/// Map a function to the values of two leafs and return a new leaf
/// instance. This writes to the underlying target array.
let inline map2 f slc1 slc2 (tgt : _ Target) =
    ArraySlice.iteri2 (writemap2 tgt (Functions.adapt2 f)) slc1 slc2
    toSlice tgt (ArraySlice.rows slc1) (ArraySlice.cols slc1)


/// Map a function to the values and index pairs of a leaf and
/// return a new leaf instance. This writes to the underlying
/// target array.
let inline mapi f slc (tgt : _ Target) =
    ArraySlice.iteri (writemapi tgt (Functions.adapt3 f)) slc
    toSlice tgt (ArraySlice.rows slc) (ArraySlice.cols slc)


/// Use the offset stored in tgt to iterate over some array slice.
let inline iteri f slc (tgt : _ Target) =
    ArraySlice.iteri (fun i j v -> f (tgt.i + i) (tgt.j + j) v) slc


/// Write the elements of slc in reverse horizontal order into
/// target.
let inline hrev slc (tgt : _ Target) =
    ArraySlice.iteri (fun i j v -> write tgt i (ArraySlice.cols slc - 1 - j) v) slc
    toSlice tgt (ArraySlice.rows slc) (ArraySlice.cols slc)


/// Write the elements of slc in reverse vertical order into
/// target.
let inline vrev slc (tgt : _ Target) =
    ArraySlice.iteri (fun i j v -> write tgt (ArraySlice.rows slc - 1 - i) j v) slc
    toSlice tgt (ArraySlice.rows slc) (ArraySlice.cols slc)


/// Write the elements of slc in transposed order into target.
let inline transpose slc (tgt : _ Target) =
    let tgt' = { tgt with i = tgt.j; j = tgt.i } // Target must be transposed, too.
    ArraySlice.iteri (fun i j v -> write tgt' j i v) slc
    toSlice tgt' (ArraySlice.cols slc) (ArraySlice.rows slc)


/// Read from target with its offset.
let inline get (tgt : _ Target) i j =
    tgt.vals.[tgt.i + i, tgt.j + j]


/// Generalized two-dimensional scan using a function f and a function
/// slc to access "state" based on (i, j) position. Function f is
/// called like this:
/// f(I(i, j - 1), I(i - 1, j - 1), I(i - 1, j), I(i,j))
let scan f slc tgt =
    // Compute prefix for remaining elements, taking prefix sums from
    // tgt itself.
    let f' = Functions.adapt4 f
    for i in 0 .. ArraySlice.rows slc - 1 do
        for j in 0 .. ArraySlice.cols slc - 1 do
            write tgt i j (Functions.invoke4 f'
                                             (get tgt i (j - 1))       // Prefix from same row.
                                             (get tgt (i - 1) (j - 1)) // Prefix from diagonal.
                                             (get tgt (i - 1) j)       // Prefix from same column.
                                             (ArraySlice.get slc i j))
