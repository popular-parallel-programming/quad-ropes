namespace RadTrees.Test

module Gen =
    open FsCheck
    open RadTrees

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
        let hs = Gen.oneof (seq { yield Gen.suchThat (eq QuadRope.rows) (Gen.two genRevRope) })
        let vs = Gen.oneof (seq { yield Gen.suchThat (eq QuadRope.cols) (Gen.two genRevRope) })
        Gen.oneof ( seq { yield genRevRope;
                          yield Gen.map (fun (l, r) -> QuadRope.hcat l r) hs;
                          yield Gen.map (fun (u, l) -> QuadRope.vcat u l) vs })

    let shrink = function
        | Node (_, _, _, ne, nw, sw, se) ->
            seq { yield ne; yield nw; yield sw; yield se
                  if QuadRope.rows ne = QuadRope.rows nw then yield QuadRope.makeFlatNode nw ne
                  if QuadRope.cols nw = QuadRope.cols sw then yield QuadRope.makeThinNode nw sw }
        | _ -> Seq.empty

    type QuadRopeGen =
        static member QuadRope () =
            { new Arbitrary<int QuadRope>() with
              override x.Generator = genCatRope
              override x.Shrinker rope = shrink rope
            }

    let register () =
        Arb.register<QuadRopeGen>()
