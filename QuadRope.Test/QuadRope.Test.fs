namespace RadTrees.Test

module QuadRopeTest =
    open FsCheck
    open RadTrees

    module Utils =
        let (.=.) l r =
                l = r |@ sprintf "%A, %A" l r

        let (.<=.) l r =
                l <= r |@ sprintf "%A, %A" l r

        let makeIndices h w =
            seq { for i in 0..h - 1 do
                  for j in 0..w - 1 ->
                  i, j }

        let rec maintainsTight = function
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

        let access rope (i, j) =
            try
                ignore (QuadRope.get rope i j)
                true |@ ""
            with
                | e  -> false |@ sprintf "(i, j) = (%d, %d)\nrope = %A\nexception = %A" i j rope e

    open Utils
    type QuadRopeTest =

        (* Hight of generated rope is equal to height parameter. *)
        static member ``init produces correct height`` (NonNegativeInt h) (NonNegativeInt w) =
            (0 < h && 0 < w) ==>
            lazy (let rope = QuadRope.init h w (*)
                  QuadRope.rows rope = h)

        (* Width of generated rope is equal to width parameter. *)
        static member ``init produces correct width`` (NonNegativeInt h) (NonNegativeInt w) =
            (0 < h && 0 < w) ==>
            lazy (let rope = QuadRope.init h w (*)
                  QuadRope.cols rope = w)

        (* Wavefront initialization with multiplication produces correct values at positions. *)
        static member ``init produces correct values`` (NonNegativeInt h) (NonNegativeInt w) =
            (0 < h && 0 < w) ==>
            lazy (let rope = QuadRope.init h w (*)
                  Seq.forall (fun (i, j) -> QuadRope.get rope i j = i * j) (makeIndices h w))

        static member ``get is always inside bounds`` (a : int QuadRope) =
            Seq.reduce (.&.) (Seq.map (access a) (makeIndices (QuadRope.rows a) (QuadRope.cols a)))

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

        static member ``hrev of hrev is identity`` (a : int QuadRope) =
            a = QuadRope.hrev (QuadRope.hrev a)

        static member ``vrev of vrev is identity`` (a : int QuadRope) =
            a = QuadRope.vrev (QuadRope.vrev a)

        (* hrev puts values in the correct position and get can access them. *)
        static member ``get accesses hrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
            let h = QuadRope.rows a
            let w = QuadRope.cols a
            (i < h && j < w) ==>
            lazy (QuadRope.get (QuadRope.hrev a) i j .=. QuadRope.get a i ((w - 1) - j))

        (* vrec puts values in the correct position and get can access them. *)
        static member ``get accesses vrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
            let h = QuadRope.rows a
            let w = QuadRope.cols a
            (i < h && j < w) ==>
            lazy (QuadRope.get (QuadRope.vrev a) i j .=. QuadRope.get a ((h - 1) - i) j)

        static member ``get accesses hcat correctly`` (a : int QuadRope) (b : int QuadRope) =
            (QuadRope.rows a = QuadRope.rows b) ==>
            lazy (let ab = QuadRope.hcat a b
                  Seq.reduce (.&.) (Seq.map (access ab) (makeIndices (QuadRope.rows ab) (QuadRope.cols ab))))

        static member ``get accesses vcat correctly`` (a : int QuadRope) (b : int QuadRope) =
            (QuadRope.cols a = QuadRope.cols b) ==>
            lazy (let ab = QuadRope.vcat a b
                  Seq.reduce (.&.) (Seq.map (access ab) (makeIndices (QuadRope.rows ab) (QuadRope.cols ab))))

        static member ``hsplit produces ropes of correct width`` (a : int QuadRope) (NonNegativeInt w) =
            w <= QuadRope.cols a ==>
            lazy (let b = QuadRope.split a 0 0 (QuadRope.rows a) w
                  QuadRope.cols b .=. w |@ sprintf "%A" b)

        static member ``hsplit2 produces two ropes of correct width`` (a : int QuadRope) (NonNegativeInt w) =
            w <= QuadRope.cols a ==>
            lazy (let b, c = QuadRope.hsplit2 a w
                  QuadRope.cols a .=. QuadRope.cols b + QuadRope.cols c)

        static member ``vsplit produces ropes of correct height`` (a : int QuadRope) (NonNegativeInt h) =
            h <= QuadRope.rows a ==>
            lazy (let b = QuadRope.split a 0 0 h (QuadRope.cols a)
                  QuadRope.rows b .=. h |@ sprintf "%A" b)

        static member ``vsplit2 produces two ropes of correct height`` (a : int QuadRope) (NonNegativeInt h) =
            h <= QuadRope.rows a ==>
            lazy (let b, c = QuadRope.vsplit2 a h
                  QuadRope.rows a .=. QuadRope.rows b + QuadRope.rows c)

        static member ``balanceH maintains layout`` (a : int QuadRope) =
            not (QuadRope.isBalancedH a) ==>
            lazy (let b = QuadRope.hbalance a
                  let indices = makeIndices (QuadRope.rows a) (QuadRope.cols a)
                  Seq.reduce (.&.) (Seq.map (fun (i, j) -> QuadRope.get a i j .=. QuadRope.get b i j) indices))

        static member ``balanceH maintains or improves depth`` (a : int QuadRope) =
            not (QuadRope.isBalancedH a) ==>
            lazy (let b = QuadRope.hbalance a
                  QuadRope.depth b .<=. QuadRope.depth a)

        static member ``balanceV maintains layout`` (a : int QuadRope) =
            not (QuadRope.isBalancedV a) ==>
            lazy (let b = QuadRope.vbalance a
                  let indices = makeIndices (QuadRope.rows a) (QuadRope.cols a)
                  Seq.reduce (.&.) (Seq.map (fun (i, j) -> QuadRope.get a i j .=. QuadRope.get b i j) indices))

        static member ``balanceV maintains or improves depth`` (a : int QuadRope) =
            not (QuadRope.isBalancedV a) ==>
            lazy (let b = QuadRope.vbalance a
                  QuadRope.depth b .<=. QuadRope.depth a)

        static member ``toRows yields correct number of scalars`` (a : int QuadRope) =
            QuadRope.rows a * QuadRope.cols a .=. Seq.length (Seq.concat (QuadRope.toRows a))

        static member ``toRows yields scalars in correct order`` (a : int QuadRope) =
            let indices = makeIndices (QuadRope.rows a) (QuadRope.cols a)
            let scalars = Seq.map (fun (i, j) -> QuadRope.get a i j) indices
            Seq.reduce (.&.) (Seq.map2 (.=.) scalars (Seq.concat (QuadRope.toRows a)))

        static member ``hfold maintains order`` (a : int QuadRope) =
            let cons xs x = x :: xs
            let empties = QuadRope.init (QuadRope.rows a) 1 (fun _ _ -> [])
            let rows0 = QuadRope.hfold cons empties a
            Seq.map (Seq.toList >> List.rev) (QuadRope.toRows a) .=. Seq.concat (QuadRope.toRows rows0)
