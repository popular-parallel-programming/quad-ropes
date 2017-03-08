module RadTrees.Examples.VanDerCorput

open RadTrees

module private Math =
    let inline pow x y = System.Math.Pow ((float x), (float y))


module QuadRope =
    let private vdcBuilder map =
        let next i is =
            QuadRope.hcat is (QuadRope.hcat (QuadRope.singleton i) (map ((+) i) is))

        let rec vdc n =
            if n <= 1 then
                QuadRope.singleton 0.5
            else
                next (Math.pow 2.0 -n) (vdc (n - 1))

        vdc

    let vanDerCorput = vdcBuilder QuadRope.map
    let vanDerCorputPar = vdcBuilder Parallel.QuadRope.map



module Array2D =
    let private vdcBuilder cat map =
        let next i is =
            cat is (cat (Array2DExt.singleton i) (map ((+) i) is))

        let rec vdc n =
            if n <= 1 then
                Array2DExt.singleton 0.5
            else
                next (Math.pow 2.0 -n) (vdc (n - 1))

        vdc


    let vanDerCorput = vdcBuilder Array2DExt.cat1 Array2DExt.map
    let vanDerCorputPar = vdcBuilder Parallel.Array2DExt.cat1 Parallel.Array2DExt.map
