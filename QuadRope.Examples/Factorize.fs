module QuadRopes.Examples.Factorize

open QuadRopes
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
                    factorize p (k + 1) (k :: ks)
            map (fun p -> factorize p 2 []) vals

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
                    factorize p (k + 1) (k :: ks)
            map (fun p -> factorize p 2 []) vals
        factorize


    let factorize = factorizeBuilder Array2D.map
    let factorizePar = factorizeBuilder Parallel.Array2DExt.map
