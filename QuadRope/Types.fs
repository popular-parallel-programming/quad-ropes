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

module QuadRopes.Types

/// A simple view on 2D arrays. Members r and c are row- and
/// column-offset. Members h and w are height and width of the
/// view. Member vals is the original array.
type 'a ArraySlice when 'a : equality =
    internal { r : int; c : int; h : int ; w : int; vals : 'a [,] }


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
