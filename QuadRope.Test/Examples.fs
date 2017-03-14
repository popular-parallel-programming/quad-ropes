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

/// Interesting properties about quad ropes.
module RadTrees.Test.Examples

open FsCheck
open RadTrees
open RadTrees.Types
open RadTrees.Examples

module Utils =
    let isEmpty a =
        Array2D.length1 a = 0 || Array2D.length2 a = 0

type Handle = class end



let ``factorize equals array variant`` (a : int [,]) =
    not (Utils.isEmpty a) ==> lazy (
        Factorize.QuadRope.factorize (QuadRope.fromArray2D a)
        |> QuadRope.toArray2D = Factorize.Array2D.factorize a)


let ``fibonacci equals array variant`` (NonNegativeInt n) =
    Fibonacci.QuadRope.fibseq n |> QuadRope.toArray2D = Fibonacci.Array2D.fibseq n


let ``mmult equals array variant`` (a : float [,]) (b : float [,]) =
    (not (Utils.isEmpty a) && not (Utils.isEmpty b) && Array2D.length1 a = Array2D.length2 b) ==> lazy (
        MatMult.QuadRope.mmult (QuadRope.fromArray2D a) (QuadRope.fromArray2D b)
        |> QuadRope.toArray2D = MatMult.Array2D.mmult a b)


let ``sieve equals array variant`` (NonNegativeInt n) =
    Sieve.QuadRope.sieve n |> QuadRope.toArray2D = Sieve.Array2D.sieve n


let ``smith-waterman equals array variant`` (a : string) (b : string) =
    (String.length a > 0 && String.length b > 0) ==> lazy (
        SmithWaterman.QuadRope.align a b = SmithWaterman.Array2D.align a b)


let ``vdc equals array variant`` (NonNegativeInt n) =
    n < 20 ==> lazy (
        VanDerCorput.QuadRope.vanDerCorput n |> QuadRope.toArray2D = VanDerCorput.Array2D.vanDerCorput n)
