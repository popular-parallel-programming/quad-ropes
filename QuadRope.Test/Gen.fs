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

namespace QuadRope.Test

module Gen =
    open FsCheck
    open QuadRope
    open QuadRope.Types

    let maxSize = 14
    let size = Gen.choose (1, maxSize)


    /// Construct a quad rope of size h * w that is either of
    /// arbitrary integers of a sparse quad rope of 0s or 1s.
    let genRopeOf h w =
        Gen.oneof [ Gen.map QuadRope.fromArray2D (Gen.array2DOfDim (h, w) Arb.generate<int>)
                    Gen.constant (QuadRope.create h w 0)
                    Gen.constant (QuadRope.create h w 1) ]


    /// Generate an arbitrary quad rope of max size * size elements.
    let genRope =
        gen { let! h = size
              let! w = size
              return! genRopeOf h w }


    /// Generate a transformation (e.g. hrev, vrev or transpose) that
    /// can directly be applied to a quad rope to modify its shape.
    let genTransform : Gen<_ QuadRope -> _ QuadRope> =
        Gen.oneof [ Gen.constant QuadRope.hrev
                    Gen.constant QuadRope.vrev
                    Gen.constant QuadRope.transpose ]


    /// Generate a quad rope that has possibly been transformed by
    /// reversal or similar.
    let genTransRope =
        gen { let! trans = Gen.listOf genTransform |> Gen.map (List.fold (>>) id)
              let! qr = genRope
              return trans qr }


    /// Generate a possibly sliced quad rope.
    let genSliceRope =
        Gen.oneof [ genTransRope
                    gen { let! i = size
                          let! j = size
                          let! h = size
                          let! w = size
                          let! qr = genTransRope
                          return QuadRope.slice i j h w qr } ]


    // Check that two quad ropes a and b can be concatenated.
    let canCat f g (a, b) =
        f a = f b && g a + g b <= 10 * maxSize


    let canHCat = canCat QuadRope.rows QuadRope.cols
    let canVCat = canCat QuadRope.cols QuadRope.rows


    /// Generate concatenated quad ropes. This is interesting, because
    /// somehow transformed quad ropes combined together result in
    /// more complicated internal structures.
    let genCatRope =
        Gen.oneof [ genSliceRope
                    Gen.map ((<||) QuadRope.hcat) (Gen.where canHCat (Gen.two genSliceRope))
                    Gen.map ((<||) QuadRope.vcat) (Gen.where canVCat (Gen.two genSliceRope)) ]


    /// Deconstruct a quad rope if it consists of concatenation nodes.
    let shrink qr =
        let a = QuadRope.leftBranch qr
        let b = QuadRope.rightBranch qr
        seq { if a.IsSome then yield a.Value
              if b.IsSome then yield b.Value
              if a.IsSome && b.IsSome then yield QuadRope.empty }


    type QuadRopeGen =
        static member IntQuadRope () =
            {
                new Arbitrary<int QuadRope>() with
                override x.Generator = genCatRope
                override x.Shrinker rope = shrink rope
            }

        static member FloatQuadRope () =
            {
                new Arbitrary<float QuadRope>() with
                override x.Generator = Gen.map (QuadRope.map float) genCatRope
                override x.Shrinker rope = shrink rope
            }


    let register () =
        Arb.register<QuadRopeGen>()
