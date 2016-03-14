#r "../packages/FsCheck/lib/net45/FsCheck.dll"

#load "Utils.fs"
#load "QuadRope.fs"

open FsCheck
open RadTrees

let (.=.) l r =
        l = r |@ sprintf "%A, %A" l r

let rec maintainsTight a =
    match a with
        | Empty
        | Leaf _ -> true
        | Node (_, _, _, ne, nw, Empty, Empty) ->
            maintainsTight ne && maintainsTight nw
        | Node (_, _, _, Empty, nw, sw, Empty) ->
            maintainsTight nw && maintainsTight sw
        | Node (_, _, _, _, Empty, _, _)
        | Node (_, _, _, _, _, Empty, _) -> false
        | Node (_, _, _, ne, nw, sw, se) ->
            maintainsTight ne && maintainsTight nw && maintainsTight sw && maintainsTight se

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

        (* Hight of generated rope is equal to height parameter. *)
        static member ``init produces correct height`` (NonNegativeInt h) (NonNegativeInt w) =
            let rope = QuadRope.init h w (*)
            QuadRope.rows rope = h

        (* Width of generated rope is equal to width parameter. *)
        static member ``init produces correct width`` (NonNegativeInt h) (NonNegativeInt w) =
            let rope = QuadRope.init h w (*)
            QuadRope.cols rope = w

        (* Wavefront initialization with multiplication produces correct values at positions. *)
        static member ``init produces correct values`` (NonNegativeInt h) (NonNegativeInt w) (NonNegativeInt i) (NonNegativeInt j) =
            (i < h && j < w) ==>
            lazy ((QuadRope.get (QuadRope.init h w (*)) i j) = i * j)

        (* The width of hcat of two ropes is equal to the sum of their widths. *)
        static member ``hcat width is equal to width sum`` (a : int QuadRope) (b : int QuadRope)  =
            (QuadRope.rows a = QuadRope.rows b) ==>
            lazy (QuadRope.cols a + QuadRope.cols b = QuadRope.cols (QuadRope.hcat a b))

        (* The height of vcat of two ropes is equal to the sum of their heights. *)
        static member ``vcat height is equal to height sum`` (a : int QuadRope) (b : int QuadRope)  =
            (QuadRope.cols a = QuadRope.cols b) ==>
            lazy (QuadRope.rows a + QuadRope.rows b = QuadRope.rows (QuadRope.vcat a b))

        static member ``hrev maintains uper-left invariant`` (a: int QuadRope) =
            maintainsTight (QuadRope.hrev a)

        static member ``vrev maintains uper-left invariant`` (a: int QuadRope) =
            maintainsTight (QuadRope.vrev a)

        (* hrev puts values in the correct position and get can access them. *)
        static member ``get accesses hrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
            let h = QuadRope.rows a
            let w = QuadRope.cols a
            (i < h && j < w) ==>
            lazy (QuadRope.get (QuadRope.hrev a) i j = QuadRope.get a i (w - j - 1))

        (* vrec puts values in the correct position and get can access them. *)
        static member ``get accesses vrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
            let h = QuadRope.rows a
            let w = QuadRope.cols a
            (i < h && j < w) ==>
            lazy (QuadRope.get (QuadRope.vrev a) i j .=. QuadRope.get a (h - i - 1) j)

        static member ``get accesses hcat correctly`` (a : int QuadRope) (b : int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
            let h = QuadRope.rows a
            let w = QuadRope.cols a + QuadRope.cols b
            (i < h && j < w && QuadRope.rows a = QuadRope.rows b) ==>
            lazy (let ab = QuadRope.hcat a b
                  let v = QuadRope.get ab i j
                  if j < QuadRope.cols a then
                      v .=. QuadRope.get a i j
                  else
                      v .=. QuadRope.get b i (j - QuadRope.cols a))

        static member ``get accesses vcat correctly`` (a : int QuadRope) (b : int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
            let h = QuadRope.rows a + QuadRope.cols b
            let w = QuadRope.cols a
            (i < h && j < w && QuadRope.cols a = QuadRope.cols b) ==>
            lazy (let ab = QuadRope.vcat a b
                  let v = QuadRope.get ab i j
                  if i < QuadRope.rows a then
                      v .=. QuadRope.get a i j
                  else
                      v .=. QuadRope.get b (i - QuadRope.rows a) j)


    Check.QuickAll<QuadRopeTest>()
