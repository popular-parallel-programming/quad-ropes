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

namespace RadTrees.Test

module Gen =
    open FsCheck
    open RadTrees
    open Types

    let maxSize = 14
    let size = Gen.choose (1, maxSize)

    let genRopeOf h w =
        gen { return QuadRope.init h w (*) }

    let genRope =
        gen { let! h = size
              let! w = size
              return! genRopeOf h w }

    let genRevRope =
        Gen.oneof (seq { yield genRope;
                         yield Gen.map QuadRope.hrev genRope;
                         yield Gen.map QuadRope.vrev genRope;
                         yield Gen.map (QuadRope.hrev >> QuadRope.vrev) genRope;
                         yield Gen.map (QuadRope.vrev >> QuadRope.hrev) genRope })

    let genSliceRope =
        Gen.oneof (seq { yield genRevRope;
                         yield gen { let! i = size
                                     let! j = size
                                     let! h = size
                                     let! w = size
                                     let! qr = genRevRope
                                     return QuadRope.slice i j h w qr }
                         })

    let genCatRope =
        let eq f (a, b) = f a = f b
        let hs = Gen.oneof (seq { yield Gen.where (eq QuadRope.rows) (Gen.two genRevRope) })
        let vs = Gen.oneof (seq { yield Gen.where (eq QuadRope.cols) (Gen.two genRevRope) })
        Gen.oneof ( seq { yield genRevRope;
                          yield Gen.map (fun (l, r) -> QuadRope.hcat l r) hs;
                          yield Gen.map (fun (u, l) -> QuadRope.vcat u l) vs })

    let shrink = function
        | Node (_, _, _, ne, nw, sw, se) ->
            seq { if not (QuadRope.isEmpty ne) then yield ne
                  if not (QuadRope.isEmpty nw) then yield nw
                  if not (QuadRope.isEmpty sw) then yield sw
                  if not (QuadRope.isEmpty se) then yield se
                  yield QuadRope.Empty }
        | _ -> Seq.empty

    type QuadRopeGen =
        static member QuadRope () =
            { new Arbitrary<int QuadRope>() with
              override x.Generator = genCatRope
              override x.Shrinker rope = shrink rope
            }

    let register () =
        Arb.register<QuadRopeGen>()
