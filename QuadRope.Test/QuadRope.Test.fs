namespace RadTrees.Test

open FsCheck
open RadTrees
open Types

module Utils =

    let makeIndices h w =
        seq { for i in 0..h - 1 do
              for j in 0..w - 1 ->
              i, j }

    let makeIndicesFrom rope =
        makeIndices (QuadRope.rows rope) (QuadRope.cols rope)

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
        | Slice _ as rope -> maintainsTight rope

    let access rope (i, j) =
        try
            ignore (QuadRope.get rope i j)
            true
        with
            | _ -> false

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
        Seq.forall (access a) (makeIndicesFrom a)

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
        lazy (QuadRope.get (QuadRope.hrev a) i j = QuadRope.get a i ((w - 1) - j))

    (* vrec puts values in the correct position and get can access them. *)
    let ``get accesses vrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
        let h = QuadRope.rows a
        let w = QuadRope.cols a
        (i < h && j < w) ==>
        lazy (QuadRope.get (QuadRope.vrev a) i j = QuadRope.get a ((h - 1) - i) j)

    let ``get accesses hcat correctly`` (a : int QuadRope) (b : int QuadRope) =
        (QuadRope.rows a = QuadRope.rows b) ==>
        lazy (let ab = QuadRope.hcat a b
              Seq.forall (access ab) (makeIndicesFrom ab))

    let ``get accesses vcat correctly`` (a : int QuadRope) (b : int QuadRope) =
        (QuadRope.cols a = QuadRope.cols b) ==>
        lazy (let ab = QuadRope.vcat a b
              Seq.forall (access ab) (makeIndicesFrom ab))

    let ``get accesses slice correctly`` (a : int QuadRope) (NonNegativeInt i)
                                                            (NonNegativeInt j)
                                                            (NonNegativeInt h)
                                                            (NonNegativeInt w) =
        (i < QuadRope.rows a && i < h && j < QuadRope.cols a && j < w) ==>
        lazy (let b = QuadRope.slice a i j h w
              Seq.forall
                (fun (i0, j0) -> QuadRope.get b i0 j0 = QuadRope.get a (i + i0) (j + j0))
                (makeIndicesFrom b))

    let ``slices never nest`` (a: int QuadRope) =
       (* Using generated limits for slices results in very few run tests, this is more reliable. *)
       let b, _ = QuadRope.vsplit2 a (QuadRope.rows a / 2)
       let c, _ = QuadRope.vsplit2 b (QuadRope.rows b / 2)
       match c with
           | Slice (_, _, _, _, Slice _) -> false
           | _ -> true

    let ``slice computes correct indices`` (a : int QuadRope)
                                           (NonNegativeInt i)
                                           (NonNegativeInt j)
                                           (NonNegativeInt h)
                                           (NonNegativeInt w) =
        let b = QuadRope.slice a i j h w
        if QuadRope.rows a <= i || h = 0 || QuadRope.cols a <= j || w = 0 then
            QuadRope.isEmpty b
        else
            QuadRope.rows b = min (QuadRope.rows a - i) h &&
            QuadRope.cols b = min (QuadRope.cols a - j) w

    let ``recursive slice computes correct indices`` (a : int QuadRope) (d : NonNegativeInt) =
        let i = 1
        let j = 1
        let h = QuadRope.rows a - 1
        let w = QuadRope.cols a - 1
        let b = QuadRope.slice a i j h w
        QuadRope.rows (QuadRope.slice b (i + d.Get) j h w) <= QuadRope.rows b &&
        QuadRope.cols (QuadRope.slice b i (j + d.Get) h w) <= QuadRope.cols b &&
        QuadRope.rows (QuadRope.slice b (i - d.Get) j h w) <= QuadRope.rows b &&
        QuadRope.cols (QuadRope.slice b i (j - d.Get) h w) <= QuadRope.cols b &&
        QuadRope.rows (QuadRope.slice b i j (h + d.Get) w) <= QuadRope.rows b &&
        QuadRope.cols (QuadRope.slice b i j h (w + d.Get)) <= QuadRope.cols b &&
        QuadRope.rows (QuadRope.slice b i j (h - d.Get) w) <= QuadRope.rows b &&
        QuadRope.cols (QuadRope.slice b i j h (w - d.Get)) <= QuadRope.cols b

    let ``hslice produces ropes of correct width`` (a : int QuadRope) (NonNegativeInt w) =
        w <= QuadRope.cols a ==>
        lazy (let b = QuadRope.slice a 0 0 (QuadRope.rows a) w
              QuadRope.cols b = w)

    let ``hsplit2 produces two ropes of correct width`` (a : int QuadRope) (NonNegativeInt w) =
        w <= QuadRope.cols a ==>
        lazy (let b, c = QuadRope.hsplit2 a w
              QuadRope.cols b = w && QuadRope.cols c = QuadRope.cols a - w)

    let ``vslice produces ropes of correct height`` (a : int QuadRope) (NonNegativeInt h) =
        h <= QuadRope.rows a ==>
        lazy (let b = QuadRope.slice a 0 0 h (QuadRope.cols a)
              QuadRope.rows b = h)

    let ``vsplit2 produces two ropes of correct height`` (a : int QuadRope) (NonNegativeInt h) =
        h <= QuadRope.rows a ==>
        lazy (let b, c = QuadRope.vsplit2 a h
              (QuadRope.rows b = h) && (QuadRope.rows c = QuadRope.rows a - h))

    let ``balanceH maintains layout`` (a : int QuadRope) =
        not (QuadRope.isBalancedH a) ==>
        lazy (let b = QuadRope.hbalance a
              Seq.forall (fun (i, j) -> QuadRope.get a i j = QuadRope.get b i j) (makeIndicesFrom a))

    let ``balanceH maintains or improves depth`` (a : int QuadRope) =
        not (QuadRope.isBalancedH a) ==>
        lazy (let b = QuadRope.hbalance a
              QuadRope.depth b <= QuadRope.depth a)

    let ``balanceV maintains layout`` (a : int QuadRope) =
        not (QuadRope.isBalancedV a) ==>
        lazy (let b = QuadRope.vbalance a
              Seq.forall (fun (i, j) -> QuadRope.get a i j = QuadRope.get b i j) (makeIndicesFrom a))

    let ``balanceV maintains or improves depth`` (a : int QuadRope) =
        not (QuadRope.isBalancedV a) ==>
        lazy (let b = QuadRope.vbalance a
              QuadRope.depth b <= QuadRope.depth a)

    let ``toRows yields correct number of scalars`` (a : int QuadRope) =
        QuadRope.rows a * QuadRope.cols a = Seq.length (Seq.concat (QuadRope.toRows a))

    let ``toRows yields scalars in correct order`` (a : int QuadRope) =
        let indices = makeIndices (QuadRope.rows a) (QuadRope.cols a)
        let scalars = Seq.map (fun (i, j) -> QuadRope.get a i j) indices
        Seq.forall2 (=) scalars (Seq.concat (QuadRope.toRows a))

    let ``toCols yields correct number of scalars`` (a : int QuadRope) =
        QuadRope.rows a * QuadRope.cols a = Seq.length (Seq.concat (QuadRope.toCols a))

    let ``map modifies all values`` (a : int QuadRope) (f : int -> int) =
        let h = QuadRope.rows a
        let w = QuadRope.cols a
        let b = QuadRope.map f a
        let check (i, j) = f (QuadRope.get a i j) = QuadRope.get b i j
        Seq.forall check (makeIndices h w)

    let ``hreduce produces thin ropes`` (a : int QuadRope) (f : int -> int -> int) =
        (not (QuadRope.isEmpty a)) ==> lazy (QuadRope.cols (QuadRope.hreduce f a) = 1)

    let ``vreduce produces flat ropes`` (a : int QuadRope) (f : int -> int -> int) =
        (not (QuadRope.isEmpty a)) ==> lazy (QuadRope.rows (QuadRope.vreduce f a) = 1)

    let ``reduce equals hreduce + vreduce`` (a : int QuadRope) =
        (not (QuadRope.isEmpty a)) ==>
          lazy (QuadRope.reduce (+) a = QuadRope.get (QuadRope.hreduce (+) (QuadRope.vreduce (+) a)) 0 0)

    let ``map + reduce equals mapreduce`` (a : int QuadRope) (f : int -> int) (g : int -> int -> int) =
        QuadRope.hmapreduce f g a = QuadRope.hreduce g (QuadRope.map f a) &&
            QuadRope.vmapreduce f g a = QuadRope.vreduce g (QuadRope.map f a)

    let ``hfilter removes elements correctly`` (a : int QuadRope) (Fun p) =
        QuadRope.rows a = 1 ==> lazy QuadRope.forall p (QuadRope.hfilter p a)

    let ``vfilter removes elements correctly`` (a : int QuadRope) (Fun p) =
        QuadRope.cols a = 1 ==> lazy QuadRope.forall p (QuadRope.vfilter p a)

    let snoc xs x = x :: xs

    let ``hfold maintains order`` (a : int QuadRope) =
        let empties = QuadRope.initAll (QuadRope.rows a) 1 []
        let b = QuadRope.hfold snoc empties a
        (Seq.map (Seq.toList >> List.rev) (QuadRope.toRows a) |> List.ofSeq)
            = (Seq.concat (QuadRope.toRows b) |> List.ofSeq)

    let ``vfold maintains order`` (a : int QuadRope) =
        let empties = QuadRope.initAll 1 (QuadRope.cols a) []
        let b = QuadRope.vfold snoc empties a
        (Seq.map (Seq.toList >> List.rev) (QuadRope.toCols a) |> List.ofSeq)
            = (Seq.concat (QuadRope.toCols b) |> List.ofSeq)

    let ``last of hscan equals hfold`` (a : int QuadRope) =
        let b = QuadRope.map List.singleton a
        let states = QuadRope.initAll (QuadRope.rows b) 1 []
        let c = QuadRope.hscan (@) (fun _ -> []) b
        let d = QuadRope.hfold (@) states b
        let x = List.tryLast (Seq.toList (Seq.map Seq.toList (QuadRope.toCols c)))
        let y = List.tryLast (Seq.toList (Seq.map Seq.toList (QuadRope.toCols d)))
        x = y

    let ``last of vscan equals vfold`` (a : int QuadRope) =
        let b = QuadRope.map List.singleton a
        let states = QuadRope.initAll 1 (QuadRope.cols b) []
        let c = QuadRope.vscan (@) (fun _ -> []) b
        let d = QuadRope.vfold (@) states b
        let x = List.tryLast (Seq.toList (Seq.map Seq.toList (QuadRope.toRows c)))
        let y = List.tryLast (Seq.toList (Seq.map Seq.toList (QuadRope.toRows d)))
        x = y

    let ``hscan's elements are strictly ordered`` (a : int QuadRope) =
        let b = QuadRope.map List.singleton a
        let c = QuadRope.hscan (@) (fun _ -> []) b
        QuadRope.forallRows (fun (x, y) -> List.length x < List.length y) c

    let ``vscan's elements are strictly ordered`` (a : int QuadRope) =
        let b = QuadRope.map List.singleton a
        let c = QuadRope.vscan (@) (fun _ -> []) b
        QuadRope.forallCols (fun (x, y) -> List.length x < List.length y) c

    let ``transpose of transpose is identity`` (a : int QuadRope) =
        QuadRope.transpose (QuadRope.transpose a) = a

    let ``zip ignores internal shape`` (a : int QuadRope) (b : int QuadRope) (f : int -> int -> int) =
        (QuadRope.rows a = QuadRope.rows b && QuadRope.cols a = QuadRope.cols b) ==>
        lazy (let c = QuadRope.zip f a b
              let indices = makeIndices (QuadRope.rows c) (QuadRope.cols c)
              Seq.forall (fun (i, j) -> QuadRope.get c i j = f (QuadRope.get a i j) (QuadRope.get b i j)) indices)
