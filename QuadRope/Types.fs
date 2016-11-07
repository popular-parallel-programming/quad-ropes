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
type 'a ArraySlice when 'a : equality = { r : int; c : int; h : int ; w : int; vals : 'a [,] }

type 'a ArraySlice with
    static member private equals a b =
        if a.h = b.h && a.w = b.w then
            let mutable eq = true
            for i in 0 .. a.h - 1 do
                for j in 0 .. a.w - 1 do
                    eq <- eq && a.vals.[i + a.r, j + a.c] = b.vals.[i + b.r, j + b.c]
            eq
        else
            false

    override this.Equals(o) =
        match o with
            | :? ('a ArraySlice) as other -> ArraySlice.equals this other
            | _ -> false

    override this.GetHashCode() =
        (17 + this.r + this.c) * this.h * this.w * hash this.vals

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a QuadRope when 'a : equality =
    | Empty
    | Leaf of 'a ArraySlice
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope
    | Slice of int * int * int * int * 'a QuadRope

/// Number of rows in a rectangular tree.
let rows = function
    | Empty -> 0
    | Leaf slc -> slc.h
    | Node (_, h, _, _, _, _, _) -> h
    | Slice (_, _, h, _, _) -> h

/// Number of columns in a rectangular tree.
let cols = function
    | Empty -> 0
    | Leaf slc -> slc.w
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
