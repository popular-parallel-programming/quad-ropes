namespace RadTrees

type 'a QuadRope =
    | Empty
    | Leaf of 'a [,]
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope

module QuadRope =

    let height = function
        | Empty -> 0
        | Leaf vs -> Array2D.length1 vs
        | Node (_, h, _, _, _, _, _) -> h

    let width = function
        | Empty -> 0
        | Leaf vs -> Array2D.length2 vs
        | Node (_, _, w, _, _, _, _) -> w

    let depth = function
        | Empty -> 0
        | Leaf _ -> 1
        | Node (d, _, _, _, _, _, _) -> d

    let maxSize = 4

    let vcat upper lower =
        if width upper <> width lower then failwith "Trees must be of same width!"
        match upper, lower with
            | Leaf us, Leaf ls when height lower + height upper <= maxSize -> Leaf (Array2D.init (fun i j ->
