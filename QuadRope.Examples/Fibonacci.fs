module RadTrees.Examples.Fibonacci

open RadTrees

module QuadRope =
    let rec fibseq n =
        match n with
            | 0 -> QuadRope.singleton 0.0
            | 1 -> QuadRope.hcat (QuadRope.singleton 1.0) (QuadRope.singleton 1.0)
            | _ ->
                let prefix = fibseq (n-1)
                let fa = QuadRope.get prefix 0 (n-2)
                let fb = QuadRope.get prefix 0 (n-1)
                QuadRope.hcat prefix (QuadRope.singleton (fa + fb))


module Array2D =
    let rec fibseq n =
        match n with
            | 0 -> Array2D.singleton 0.0
            | 1 -> Array2D.cat2 (Array2D.singleton 1.0) (Array2D.singleton 1.0)
            | _ ->
                let prefix = fibseq (n-1)
                let fa = Array2D.get prefix 0 (n-2)
                let fb = Array2D.get prefix 0 (n-1)
                Array2D.cat1 prefix (Array2D.singleton (fa + fb))
