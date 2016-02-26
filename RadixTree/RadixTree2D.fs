namespace RadixTree

open RadixTree

module RadixTree2D =

    type 'a tree2D =
        | Leaf of 'a [,]
        | Node of int * 'a tree2D [,]

    let mkLeaf vs =
        Leaf vs

    let mkNode depth ns =
        Node (depth, ns)

    let fromArray bits arr =
        let rdx = Bits.radix bits
        let chunk arr =
            Array2D.chunkBySize rdx rdx arr
        let rec build depth arr =
            if Array2D.isSingleton arr then
                arr.[0, 0]
            else
                build (depth + 1) (Array2D.map (mkNode depth) (chunk arr))
        build 1 (Array2D.map mkLeaf (chunk arr))

    let fromArray1 bits arr =
        fromArray bits (Array2D.makeSingleCol arr)

    let fromArray2 bits arr =
        fromArray bits (Array2D.makeSingleRow arr)

    let gen bits n m f =
        (* TODO: This is quite inefficient actually. Allocate trees directly instead! *)
        fromArray bits (Array2D.init n m f)

    let rec get bits root i j =
        match root with
        | Leaf vs -> vs.[i &&& Bits.mask bits, j &&& Bits.mask bits]
        | Node (depth, ns) ->
            let i0 = Bits.index bits depth i
            let j0 = Bits.index bits depth j
            get bits ns.[i0, j0] i j

    let rec getCol bits i root =
        match root with
        | Leaf vs -> Leaf (Array2D.col vs (i &&& Bits.mask bits))
        | Node (d, ns) -> Node (d, Array2D.map (getCol bits i) (Array2D.col ns (Bits.index bits d i)))

    let rec getRow bits i root =
        match root with
        | Leaf vs -> Leaf (Array2D.row vs (i &&& Bits.mask bits))
        | Node (d, ns) -> Node (d, Array2D.map (getRow bits i) (Array2D.row ns (Bits.index bits d i)))

    let rec set bits root i j v =
        match root with
        | Leaf vs -> Leaf (Array2D.set vs (i &&& Bits.mask bits) (j &&& Bits.mask bits) v)
        | Node (depth, ns) ->
            let i0 = Bits.index bits depth i
            let j0 = Bits.index bits depth j
            Node (depth, Array2D.set ns i0 j0 (set bits ns.[i0, j0] i j v))

    let rec private isFull1 bits root =
        match root with
        | Leaf vs -> Array2D.length1 vs = Bits.radix bits
        | Node (_, ns) -> Array2D.length1 ns = Bits.radix bits || isFull1 bits (Array2D.bottomRight ns)

    let rec private isFull2 bits root =
        match root with
        | Leaf vs -> Array2D.length2 vs = Bits.radix bits
        | Node (_, ns) -> Array2D.length2 ns = Bits.radix bits || isFull2 bits (Array2D.bottomRight ns)

    let appendCol bits root col =
        let radix = Bits.radix bits
        let full = isFull1 bits
        let rec app root col =
            match root, col with
            | Leaf vs, Leaf _ when full root -> root
            | Leaf vs, Leaf v -> Leaf (Array2D.cat1 vs v)
            | Node (d, ns), Node(_, n) ->
                if full (Array2D.bottomRight ns) then
                    if not (full root) then
                        Node (d, Array2D.cat1 ns n)
                    else
                        Node (d + 1, Array2D.makeSingleRow2 root col)
                else
                    Node (d, Array2D.zipLast1 app ns n)
            | _ -> failwith "Trees must be of the same size."
        app root col

    let appendRow bits root col =
        let radix = Bits.radix bits
        let full = isFull1 bits
        let rec app root col =
            match root, col with
            | Leaf vs, Leaf _ when full root -> root
            | Leaf vs, Leaf v -> Leaf (Array2D.cat2 vs v)
            | Node (d, ns), Node(_, n) ->
                if full (Array2D.bottomRight ns) then
                    if not (full root) then
                        Node (d, Array2D.cat2 ns n)
                    else
                        Node (d + 1, Array2D.makeSingleCol2 root col)
                else
                    Node (d, Array2D.zipLast2 app ns n)
            | _ -> failwith "Trees must be of the same size."
        app root col

    type 'a RadixTree2D private (root) =
        static member bits = 2
        member this.root = root

        member this.get i j : 'a =
            get RadixTree2D<_>.bits this.root i j

        member this.set i j v =
            RadixTree2D (set RadixTree2D<_>.bits this.root i j v)

        static member fromArray arr =
            RadixTree2D (fromArray RadixTree2D<_>.bits arr)

        static member init n m f =
            RadixTree2D (gen RadixTree2D<_>.bits n m f)
