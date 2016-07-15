module RadTrees.Examples

open RadTrees

let inline pow x y = System.Math.Pow ((float x), (float y))
let (^) = pow

module Array2D =

    let singleton = Array2D.singleton

    let (@) = Array2D.cat2
    let map = Array2D.map

    let next i is =
        is @ (singleton i) @ (map ((+) i) is)

    let rec vanDerCorput n =
        if n <= 1 then
            singleton 0.5
        else
            next (2.0 ^ -n) (vanDerCorput (n - 1))

    module Parallel =
        let (@) = Parallel.Array2D.cat2
        let map = Parallel.Array2D.map

        let next i is =
            is @ (singleton i) @ (map ((+) i) is)

        let rec vanDerCorput n =
            if n <= 1 then
                singleton 0.5
            else
                next (2.0 ^ -n) (vanDerCorput (n - 1))


module QuadRope =

    let (@) = QuadRope.hcat

    let next i is =
        is @ (QuadRope.singleton i) @ (QuadRope.map ((+) i) is)

    let rec vanDerCorput n =
        if n <= 1 then
            QuadRope.singleton 0.5
        else
            next (2.0 ^ -n) (vanDerCorput (n - 1))

    module Parallel =
        let next i is =
            is @ (QuadRope.singleton i) @ (Parallel.QuadRope.map ((+) i) is)

        let rec vanDerCorput n =
            if n <= 1 then
                QuadRope.singleton 0.5
            else
                next (2.0 ^ -n) (vanDerCorput (n - 1))
