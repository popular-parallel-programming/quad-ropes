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

    let rec set bits root i j v =
        match root with
        | Leaf vs -> Leaf (Array2D.set vs (i &&& Bits.mask bits) (j &&& Bits.mask bits) v)
        | Node (depth, ns) ->
            let i0 = Bits.index bits depth i
            let j0 = Bits.index bits depth j
            Node (depth, Array2D.set ns i0 j0 (set bits ns.[i0, j0] i j v))

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
