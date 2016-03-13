#r "../packages/FsCheck/lib/net45/FsCheck.dll"

#load "Utils.fs"
#load "QuadRope.fs"

open FsCheck
open RadTrees

(* Registering QuadRope generator. *)
module Setup =

    let genRope =
        gen { let! h = Gen.choose (1, 10)
              let! w = Gen.choose (1, 10)
              return QuadRope.init h w (*) }

    type QuadRopeGen =
        static member QuadRope () =
            Arb.fromGen genRope

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

        static member ``hcat width is equal to width sum`` (a : int QuadRope) (b : int QuadRope)  =
            (QuadRope.rows a = QuadRope.rows b) ==>
            lazy (QuadRope.cols a + QuadRope.cols b = QuadRope.cols (QuadRope.hcat a b))

        static member ``vcat height is equal to height sum`` (a : int QuadRope) (b : int QuadRope)  =
            (QuadRope.cols a = QuadRope.cols b) ==>
            lazy (QuadRope.rows a + QuadRope.rows b = QuadRope.rows (QuadRope.vcat a b))

    Check.QuickAll<QuadRopeTest>()
