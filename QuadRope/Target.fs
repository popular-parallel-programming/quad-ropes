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

[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module internal RadTrees.Target

open Types

/// A convenience wrapper for writing into a target array with
/// some offset.
type 'a Target = { i : int; j : int; vals : 'a [,] }

/// Create a new target descriptor of size h * w.
let inline make h w = { i = 0; j = 0; vals = Array2D.zeroCreate h w }

let inline increment (tgt : _ Target) i j =
    { tgt with i = tgt.i + i; j = tgt.j + j }

let inline incrementRow tgt i = increment tgt i 0
let inline incrementCol tgt j = increment tgt 0 j

/// Adjust target descriptor to match north-eastern child.
let ne tgt = function
    | Node (_, _, _, _, nw, _, _) ->
        incrementCol tgt (cols nw)
    | _ -> tgt

/// Adjust target descriptor to match south-western child.
let sw tgt = function
    | Node (_, _, _, _, nw, _, _) ->
        incrementRow tgt (rows nw)
    | _ -> tgt

/// Adjust target descriptor to match south-eastern child.
let se tgt = function
    | Node (_, _, _, ne, _, sw, _) ->
        increment tgt (rows ne) (cols sw)
    | _ -> tgt

/// Generalized write to target.
let inline writemap (tgt : _ Target) f r c v =
    tgt.vals.[tgt.i + r, tgt.j + c] <- f v

let inline writemap2 (tgt : _ Target) f r c v1 v2 =
    tgt.vals.[tgt.i + r, tgt.j + c] <- f v1 v2

/// Generalized write to target with index pairs.
let inline writemapi (tgt : _ Target) f r c v =
    tgt.vals.[tgt.i + r, tgt.j + c] <- f r c v

let inline writemapi2 (tgt : _ Target) f r c v1 v2 =
    tgt.vals.[tgt.i + r, tgt.j + c] <- f r c v1 v2

/// Simplified write to target.
let inline write (tgt : _ Target) r c v =
    writemap tgt id r c v

/// Build a leaf node from a target for a given height and width.
let inline toSlice (tgt : _ Target) h w =
    ArraySlice.makeSlice tgt.i tgt.j h w tgt.vals

/// Map a function to the values of a leaf and return a new leaf
/// instance. This writes to the underlying target array.
let inline map f slc (tgt : _ Target) =
    ArraySlice.iteri (writemap tgt f) slc
    toSlice tgt (ArraySlice.length1 slc) (ArraySlice.length2 slc)

/// Map a function to the values of two leafs and return a new leaf
/// instance. This writes to the underlying target array.
let inline map2 f slc1 slc2 (tgt : _ Target) =
    ArraySlice.iteri2 (writemap2 tgt f) slc1 slc2
    toSlice tgt (ArraySlice.length1 slc1) (ArraySlice.length2 slc1)

/// Map a function to the values and index pairs of a leaf and
/// return a new leaf instance. This writes to the underlying
/// target array.
let inline mapi f slc (tgt : _ Target) =
    ArraySlice.iteri (writemapi tgt f) slc
    toSlice tgt (ArraySlice.length1 slc) (ArraySlice.length2 slc)

/// Use the offset stored in tgt to iterate over some array slice.
let inline iteri f slc (tgt : _ Target) =
    ArraySlice.iteri (fun i j v -> f (tgt.i + i) (tgt.j + j) v) slc

/// Write the elements of slc in reverse horizontal order into
/// target.
let inline hrev slc (tgt : _ Target) =
    ArraySlice.iteri (fun i j v -> write tgt i (ArraySlice.length2 slc - 1 - j) v) slc
    toSlice tgt (ArraySlice.length1 slc) (ArraySlice.length2 slc)

/// Write the elements of slc in reverse vertical order into
/// target.
let inline vrev slc (tgt : _ Target) =
    ArraySlice.iteri (fun i j v -> write tgt (ArraySlice.length1 slc - 1 - i) j v) slc
    toSlice tgt (ArraySlice.length1 slc) (ArraySlice.length2 slc)

/// Write the elements of slc in transposed order into target.
let inline transpose slc (tgt : _ Target) =
    let tgt' = { tgt with i = tgt.j; j = tgt.i } // Target must be transposed, too.
    ArraySlice.iteri (fun i j v -> write tgt' j i v) slc
    toSlice tgt' (ArraySlice.length2 slc) (ArraySlice.length1 slc)
