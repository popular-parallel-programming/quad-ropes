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

module QuadRopes.Test.Parallel.QuadRope

open FsCheck
open QuadRopes
open QuadRopes.Types

type Handle = class end

let ``parallel init equal to sequential`` (NonNegativeInt h) (NonNegativeInt w) =
    (0 < h && 0 < w) ==> lazy (
        QuadRope.zip (=) (QuadRope.init h w (*)) (Parallel.QuadRope.init h w (*))
        |> QuadRope.reduce (&&) true)


let sqr x = x * x


let ``parallel hmapreduce equal to sequential`` (a : int QuadRope) =
    QuadRope.equals (QuadRope.hmapreduce sqr (*) 1 a) (Parallel.QuadRope.hmapreduce sqr (*) 1 a)


let ``parallel vmapreduce equal to sequential`` (a : int QuadRope) =
    QuadRope.equals (QuadRope.vmapreduce sqr (*) 1 a) (Parallel.QuadRope.vmapreduce sqr (*) 1 a)


let ``parallel mapreduce equal to sequential``  (a : int QuadRope) =
    QuadRope.mapreduce sqr (*) 1 a = Parallel.QuadRope.mapreduce sqr (*) 1 a


let ``parallel map equal to sequential`` (a : int QuadRope) =
    QuadRope.equals (QuadRope.map sqr a) (Parallel.QuadRope.map sqr a)


let ``parallel hrev equal to sequential`` (a : int QuadRope) =
    QuadRope.equals (QuadRope.hrev a) (Parallel.QuadRope.hrev a)


let ``parallel vrev equal to sequential`` (a : int QuadRope) =
    QuadRope.equals (QuadRope.vrev a) (Parallel.QuadRope.vrev a)


let ``parallel transpose equal to sequential`` (a : int QuadRope) =
    QuadRope.equals (QuadRope.transpose a) (Parallel.QuadRope.transpose a)


let ``parallel zip equal to sequential`` (a : int QuadRope) (b : int QuadRope) =
    (QuadRope.rows a = QuadRope.rows b && QuadRope.cols a = QuadRope.cols b)
    ==> lazy (QuadRope.equals (QuadRope.zip (+) a b) (Parallel.QuadRope.zip (+) a b))


let ``parallel scan equal to sequential`` (a : int QuadRope) =
    let sum a b c d = a + b + c + d
    QuadRope.equals (QuadRope.scan sum 0 a) (Parallel.QuadRope.scan sum 0 a)


let ``parallel toArray2D equal to sequential`` (a : int QuadRope) =
    QuadRope.toArray2D a = Parallel.QuadRope.toArray2D a
