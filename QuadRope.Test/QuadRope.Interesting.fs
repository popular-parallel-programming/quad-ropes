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

/// Interesting properties about quad ropes.
module RadTrees.Test.Interesting.QuadRope

open FsCheck
open RadTrees
open Types

type Handle = class end

let ``sparse product equals reduce (*)`` (a : int QuadRope) =
    let a = QuadRope.map float a
    let sparse = QuadRope.SparseDouble.prod a
    let dense = QuadRope.reduce (*) 1.0 a
    let d = abs (sparse - dense)
    if not (d < 0.0001) then
        printfn "sparse = %A, dense = %A, d = %A" sparse dense d
    System.Double.IsNaN d || d < 0.0001

let ``parallel sparse product equals reduce (*)`` (a : int QuadRope) =
    let a = QuadRope.map float a
    let sparse = Parallel.QuadRope.SparseDouble.prod a
    let dense = QuadRope.reduce (*) 1.0 a
    let d = abs (sparse - dense)
    if not (d < 0.0001) then
        printfn "sparse = %A, dense = %A, d = %A" sparse dense d
    System.Double.IsNaN d || d < 0.0001
