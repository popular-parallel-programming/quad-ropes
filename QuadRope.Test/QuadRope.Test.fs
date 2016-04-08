namespace RadTrees.Test

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
module QuadRopeTest =

    type Handle = class end

    let ``DEBUG leaf sizes enabled`` () =
        QuadRope.h_max = 4 && QuadRope.w_max = 4

    (* Hight of generated rope is equal to height parameter. *)
    let ``init produces correct height`` (NonNegativeInt h) (NonNegativeInt w) =
        (0 < h && 0 < w) ==>
        lazy (let rope = QuadRope.init h w (*)
              QuadRope.rows rope = h)

    (* Width of generated rope is equal to width parameter. *)
    let ``init produces correct width`` (NonNegativeInt h) (NonNegativeInt w) =
        (0 < h && 0 < w) ==>
        lazy (let rope = QuadRope.init h w (*)
              QuadRope.cols rope = w)

    (* Wavefront initialization with multiplication produces correct values at positions. *)
    let ``init produces correct values`` (NonNegativeInt h) (NonNegativeInt w) =
        (0 < h && 0 < w) ==>
        lazy (let rope = QuadRope.init h w (*)
              Seq.forall (fun (i, j) -> QuadRope.get rope i j = i * j) (makeIndices h w))

    let ``get is always inside bounds`` (a : int QuadRope) =
        Seq.reduce (.&.) (Seq.map (access a) (makeIndices (QuadRope.rows a) (QuadRope.cols a)))

    (* The width of hcat of two ropes is equal to the sum of their widths. *)
    let ``hcat width is equal to width sum`` (a : int QuadRope) (b : int QuadRope)  =
        (QuadRope.rows a = QuadRope.rows b) ==>
        lazy (QuadRope.cols a + QuadRope.cols b = QuadRope.cols (QuadRope.hcat a b))

    (* The height of vcat of two ropes is equal to the sum of their heights. *)
    let ``vcat height is equal to height sum`` (a : int QuadRope) (b : int QuadRope)  =
        (QuadRope.cols a = QuadRope.cols b) ==>
        lazy (QuadRope.rows a + QuadRope.rows b = QuadRope.rows (QuadRope.vcat a b))

    let ``hrev maintains uper-left invariant`` (a: int QuadRope) =
        maintainsTight (QuadRope.hrev a)

    let ``vrev maintains uper-left invariant`` (a: int QuadRope) =
        maintainsTight (QuadRope.vrev a)

    let ``hrev of hrev is identity`` (a : int QuadRope) =
        a = QuadRope.hrev (QuadRope.hrev a)

    let ``vrev of vrev is identity`` (a : int QuadRope) =
        a = QuadRope.vrev (QuadRope.vrev a)

    (* hrev puts values in the correct position and get can access them. *)
    let ``get accesses hrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
        let h = QuadRope.rows a
        let w = QuadRope.cols a
        (i < h && j < w) ==>
        lazy (QuadRope.get (QuadRope.hrev a) i j .=. QuadRope.get a i ((w - 1) - j))

    (* vrec puts values in the correct position and get can access them. *)
    let ``get accesses vrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
        let h = QuadRope.rows a
        let w = QuadRope.cols a
        (i < h && j < w) ==>
        lazy (QuadRope.get (QuadRope.vrev a) i j .=. QuadRope.get a ((h - 1) - i) j)

    let ``get accesses hcat correctly`` (a : int QuadRope) (b : int QuadRope) =
        (QuadRope.rows a = QuadRope.rows b) ==>
        lazy (let ab = QuadRope.hcat a b
              Seq.reduce (.&.) (Seq.map (access ab) (makeIndices (QuadRope.rows ab) (QuadRope.cols ab))))

    let ``get accesses vcat correctly`` (a : int QuadRope) (b : int QuadRope) =
        (QuadRope.cols a = QuadRope.cols b) ==>
        lazy (let ab = QuadRope.vcat a b
              Seq.reduce (.&.) (Seq.map (access ab) (makeIndices (QuadRope.rows ab) (QuadRope.cols ab))))

    let ``hsplit produces ropes of correct width`` (a : int QuadRope) (NonNegativeInt w) =
        w <= QuadRope.cols a ==>
        lazy (let b = QuadRope.split a 0 0 (QuadRope.rows a) w
              QuadRope.cols b .=. w |@ sprintf "%A" b)

    let ``hsplit2 produces two ropes of correct width`` (a : int QuadRope) (NonNegativeInt w) =
        w <= QuadRope.cols a ==>
        lazy (let b, c = QuadRope.hsplit2 a w
              QuadRope.cols a .=. QuadRope.cols b + QuadRope.cols c)

    let ``vsplit produces ropes of correct height`` (a : int QuadRope) (NonNegativeInt h) =
        h <= QuadRope.rows a ==>
        lazy (let b = QuadRope.split a 0 0 h (QuadRope.cols a)
              QuadRope.rows b .=. h |@ sprintf "%A" b)

    let ``vsplit2 produces two ropes of correct height`` (a : int QuadRope) (NonNegativeInt h) =
        h <= QuadRope.rows a ==>
        lazy (let b, c = QuadRope.vsplit2 a h
              QuadRope.rows a .=. QuadRope.rows b + QuadRope.rows c)

    let ``balanceH maintains layout`` (a : int QuadRope) =
        not (QuadRope.isBalancedH a) ==>
        lazy (let b = QuadRope.hbalance a
              let indices = makeIndices (QuadRope.rows a) (QuadRope.cols a)
              Seq.reduce (.&.) (Seq.map (fun (i, j) -> QuadRope.get a i j .=. QuadRope.get b i j) indices))

    let ``balanceH maintains or improves depth`` (a : int QuadRope) =
        not (QuadRope.isBalancedH a) ==>
        lazy (let b = QuadRope.hbalance a
              QuadRope.depth b .<=. QuadRope.depth a)

    let ``balanceV maintains layout`` (a : int QuadRope) =
        not (QuadRope.isBalancedV a) ==>
        lazy (let b = QuadRope.vbalance a
              let indices = makeIndices (QuadRope.rows a) (QuadRope.cols a)
              Seq.reduce (.&.) (Seq.map (fun (i, j) -> QuadRope.get a i j .=. QuadRope.get b i j) indices))

    let ``balanceV maintains or improves depth`` (a : int QuadRope) =
        not (QuadRope.isBalancedV a) ==>
        lazy (let b = QuadRope.vbalance a
              QuadRope.depth b .<=. QuadRope.depth a)

    let ``toRows yields correct number of scalars`` (a : int QuadRope) =
        QuadRope.rows a * QuadRope.cols a .=. Seq.length (Seq.concat (QuadRope.toRows a))

    let ``toRows yields scalars in correct order`` (a : int QuadRope) =
        let indices = makeIndices (QuadRope.rows a) (QuadRope.cols a)
        let scalars = Seq.map (fun (i, j) -> QuadRope.get a i j) indices
        Seq.reduce (.&.) (Seq.map2 (.=.) scalars (Seq.concat (QuadRope.toRows a)))

    let ``map modifies all values`` (a : int QuadRope) (f : int -> int) =
        let h = QuadRope.rows a
        let w = QuadRope.cols a
        let b = QuadRope.map f a
        let check (i, j) = f (QuadRope.get a i j) .=. QuadRope.get b i j
        Seq.reduce (.&.) (Seq.map check (makeIndices h w))

    let ``hreduce produces thin ropes`` (a : int QuadRope) (f : int -> int -> int) =
        QuadRope.cols (QuadRope.hreduce f a) .=. 1

    let ``vreduce produces thin ropes`` (a : int QuadRope) (f : int -> int -> int) =
        QuadRope.rows (QuadRope.vreduce f a) .=. 1

    let ``map + reduce equals mapreduce`` (a : int QuadRope) (f : int -> int) (g : int -> int -> int) =
        QuadRope.mapHreduce f g a = QuadRope.hreduce g (QuadRope.map f a) &&
            QuadRope.mapVreduce f g a = QuadRope.vreduce g (QuadRope.map f a)

    let ``hfilter removes elements correctly`` (a : int QuadRope) (Fun p) =
        QuadRope.rows a = 1 ==> lazy QuadRope.forall p (QuadRope.hfilter p a)

    let ``hfold maintains order`` (a : int QuadRope) =
        let cons xs x = x :: xs
        let empties = QuadRope.init (QuadRope.rows a) 1 (fun _ _ -> [])
        let b = QuadRope.hfold cons empties a
        (Seq.map (Seq.toList >> List.rev) (QuadRope.toRows a) |> List.ofSeq) .=. (Seq.concat (QuadRope.toRows b) |> List.ofSeq)
