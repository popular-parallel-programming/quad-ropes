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
