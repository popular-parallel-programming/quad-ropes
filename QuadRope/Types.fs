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

module RadTrees.Types

[<CustomEquality;NoComparison>]
type 'a ArraySlice when 'a : equality = ArraySlice of int * int * int * int * 'a [,] with

    static member private equals (ArraySlice (i0, j0, h0, w0, arr0)) (ArraySlice (i1, j1, h1, w1, arr1)) =
        if h0 = h1 && w0 = w1 then
            let mutable eq = true
            for i in 0 .. h0 - 1 do
                for j in 0 .. w0 - 1 do
                    eq <- eq && arr0.[i + i0, j + j0] = arr1.[i + i1, j + j1]
            eq
        else
            false

    override this.Equals(o) =
        match o with
            | :? ('a ArraySlice) as other -> ArraySlice.equals this other
            | _ -> false

    override this.GetHashCode() =
        match this with
            | ArraySlice (i, j, h, w, arr) ->
                let hc = 17 + i + j
                hc * h * w * hash arr

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a QuadRope when 'a : equality =
    | Empty
    | Leaf of 'a ArraySlice
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope
    | Slice of int * int * int * int * 'a QuadRope

/// Number of rows in a rectangular tree.
let rows = function
    | Empty -> 0
    | Leaf (ArraySlice (_, _, h, _, _)) -> h
    | Node (_, h, _, _, _, _, _) -> h
    | Slice (_, _, h, _, _) -> h

/// Number of columns in a rectangular tree.
let cols = function
    | Empty -> 0
    | Leaf (ArraySlice (_, _, _, w, _)) -> w
    | Node (_, _, w, _, _, _, _) -> w
    | Slice (_, _, _, w, _) -> w

/// Depth of a rectangular tree.
let rec depth = function
    | Empty -> 0
    | Leaf _ -> 0
    | Node (d, _, _, _, _, _, _) -> d
    | Slice (_, _, _, _, qr) -> depth qr

let isEmpty = function
    | Empty -> true
    | _ -> false
