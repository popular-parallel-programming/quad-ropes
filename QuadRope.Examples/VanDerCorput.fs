module RadTrees.Examples.VanDerCorput

open RadTrees

module private Math =
    let inline pow x y = System.Math.Pow ((float x), (float y))

module QuadRope =
    let private next i is =
        QuadRope.hcat (QuadRope.hcat is (QuadRope.singleton i)) (QuadRope.map ((+) i) is)

    let rec vanDerCorput n =
        if n <= 1 then
            QuadRope.singleton 0.5
        else
            next (Math.pow 2.0 -n) (vanDerCorput (n - 1))

module Array2D =
    let private next i is =
        Array2D.cat1 (Array2D.cat1 is (Array2D.singleton i)) (Array2D.map ((+) i) is)

    let rec vanDerCorput n =
        if n <= 1 then
            Array2D.singleton 0.5
        else
            next (Math.pow 2.0 -n) (vanDerCorput (n - 1))
