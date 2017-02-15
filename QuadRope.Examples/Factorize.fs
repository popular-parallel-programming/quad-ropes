module RadTrees.Examples.Factorize

open RadTrees
open Types

module QuadRope =
    let private factorizeBuilder map =
        let factorize vals =
            let rec factorize p k ks =
                if p < k * k then
                    ks
                else if p % k = 0 then
                    factorize (p / k) k ks
                else
                    factorize p (k + 1) (QuadRope.hcat (QuadRope.singleton k) ks)
            map (fun p -> factorize p 2 Empty) vals

        factorize

    let factorize = factorizeBuilder QuadRope.map
    let factorizePar = factorizeBuilder Parallel.QuadRope.map


module Array2D =
    let private factorizeBuilder map =
        let factorize vals =
            let rec factorize p k ks =
                if p < k * k then
                    ks
                else if p % k = 0 then
                    factorize (p / k) k ks
                else
                    factorize p (k + 1) (Array2D.cat1 (Array2D.singleton k) ks)
            map (fun p -> factorize p 2 (Array2D.zeroCreate 0 1)) vals
        factorize

    let factorize = factorizeBuilder Array2D.map
    let factorizePar = factorizeBuilder Parallel.Array2D.map
