#r "../packages/FsCheck/lib/net45/FsCheck.dll"

#load "Utils.fs"
#load "QuadRope.fs"

open FsCheck
open RadTrees

let (.=.) l r =
        l = r |@ sprintf "%A, %A" l r

(* Registering QuadRope generator. *)
module Setup =

    let genRope =
        gen { let! h = Gen.choose (1, 10)
              let! w = Gen.choose (1, 10)
              return QuadRope.init h w (*) }

    let genWeirdRope =
        Gen.oneof (seq { yield genRope;
                         yield gen { let! rope = genRope;
                                     return QuadRope.hrev rope };
                         yield gen { let! rope = genRope;
                                     return QuadRope.vrev rope };
                         yield gen { let! rope = genRope
                                     return QuadRope.hrev (QuadRope.vrev rope) }})

    type QuadRopeGen =
        static member QuadRope () =
            Arb.fromGen genWeirdRope

    Arb.register<QuadRopeGen>()


(* All test functions are members of QuadRopeTest. *)
module Test =

    type QuadRopeTest =

        static member ``init produces correct height`` (NonNegativeInt h) (NonNegativeInt w) =
            let rope = QuadRope.init h w (*)
            QuadRope.rows rope = h

        static member ``init produces correct width`` (NonNegativeInt h) (NonNegativeInt w) =
            let rope = QuadRope.init h w (*)
            QuadRope.cols rope = w

        static member ``init produces correct values`` (NonNegativeInt h) (NonNegativeInt w) (NonNegativeInt i) (NonNegativeInt j) =
            (i < h && j < w) ==>
            lazy ((QuadRope.get (QuadRope.init h w (*)) i j) = i * j)

        static member ``hcat width is equal to width sum`` (a : int QuadRope) (b : int QuadRope)  =
            (QuadRope.rows a = QuadRope.rows b) ==>
            lazy (QuadRope.cols a + QuadRope.cols b = QuadRope.cols (QuadRope.hcat a b))

        static member ``vcat height is equal to height sum`` (a : int QuadRope) (b : int QuadRope)  =
            (QuadRope.cols a = QuadRope.cols b) ==>
            lazy (QuadRope.rows a + QuadRope.rows b = QuadRope.rows (QuadRope.vcat a b))

        static member ``get jumps horizontal leaves correctly`` (a : int QuadRope) (b : int QuadRope) =
            (QuadRope.rows a = QuadRope.rows b) ==>
            lazy (let ab = QuadRope.hcat a (QuadRope.vrev b)
                  let i = QuadRope.cols ab - 1
                  let j = QuadRope.rows ab - 1
                  QuadRope.get ab i j)

        static member ``get jumps vertical leaves correctly`` (a : int QuadRope) (b : int QuadRope) =
            (QuadRope.cols a = QuadRope.cols b) ==>
            lazy (let ab = QuadRope.vcat a (QuadRope.hrev b)
                  let i = QuadRope.cols ab - 1
                  let j = QuadRope.rows ab - 1
                  QuadRope.get ab i j)

    Check.QuickAll<QuadRopeTest>()
