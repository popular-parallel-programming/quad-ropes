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

module QuadRope.Types

/// A simple view on 2D arrays. Members r and c are row- and
/// column-offset. Members h and w are height and width of the
/// view. Member vals is the original array.
type 'a ArraySlice when 'a : equality =
    internal { rowOff : int; // Offset in the data array.
               colOff : int;
               rowStride : int; // Stride direction; could be different than 1 and -1.
               colStride : int
               rows : int; // Size of the array.
               cols : int;
               data : 'a [,] } // Data array.

    override slc.ToString() =
        Array2D.init slc.rows
                     slc.cols
                     (fun i j -> slc.data.[slc.rowOff + i * slc.rowStride,
                                           slc.colOff + j * slc.colStride])
        |> sprintf "%A"



/// The quad rope type. A quad rope is either empty, a leaf containing
/// a (small) array or a node that joins either two quad ropes in
/// horizontal or vertical direction.
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a QuadRope when 'a : equality =
    internal
    | Empty
    | Leaf of vs      : 'a ArraySlice

    | HCat of sparse  : bool
            * depth   : int
            * rows    : int // rows a = rows b
            * cols    : int
            * left    : 'a QuadRope
            * right   : 'a QuadRope

    | VCat of sparse  : bool
            * depth   : int
            * rows    : int
            * cols    : int // cols a = cols b
            * left    : 'a QuadRope
            * right   : 'a QuadRope

    | Slice of rowOff : int
             * colOff : int
             * rows   : int
             * cols   : int
             * qr     : 'a QuadRope

    | Sparse of rows  : int
              * cols  : int
              * v     : 'a


    override qr.ToString() =
        match qr with
            | Empty ->
                "QuadRope.Empty"
            | Leaf slc ->
                sprintf "QuadRope.Leaf (%A)" (slc.ToString())
            | HCat (_, _, _, _, a, b) ->
                sprintf "QuadRope.HCat (%A,\n\t%A)" a b
            | VCat (_, _, _, _, a, b) ->
                sprintf "QuadRope.VCat (%A,\n\t%A)" a b
            | Slice (r, c, m, n, q) ->
                sprintf "QuadRope.Slice (r=%d, c=%d, m=%d, n=%d,\n\t%A" r c m n q
            | Sparse (m, n, v) ->
                sprintf "QuadRope.Sparse (m=%d, n=%d, %A)" m n v
