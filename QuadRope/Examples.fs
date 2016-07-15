module RadTrees.Examples

open RadTrees

module private Functions =
    let inline pow x y = System.Math.Pow ((float x), (float y))

open Functions

module Array2D =
    let (@) = Array2D.cat2
    let map = Array2D.map
    let singleton = Array2D.singleton
    let empty = Array2D.initZeros 1 0

    let next i is =
        is @ (singleton i) @ (map ((+) i) is)

    let rec vanDerCorput n =
        if n <= 1 then
            singleton 0.5
        else
            next (pow 2.0 -n) (vanDerCorput (n - 1))

    let factorize arr =
        let rec factorize p k ks =
            if p < k * k then
                ks
            else if p % k = 0 then
                factorize (p / k) k ks
            else
                factorize p (k + 1) ((singleton k) @ ks)
        map (fun p -> factorize p 2 empty) arr

    module Parallel =
        let (@) = Parallel.Array2D.cat2
        let map = Parallel.Array2D.map

        let next i is =
            is @ (singleton i) @ (map ((+) i) is)

        let rec vanDerCorput n =
            if n <= 1 then
                singleton 0.5
            else
                next (pow 2.0 -n) (vanDerCorput (n - 1))

        let factorize arr =
            let rec factorize p k ks =
                if p < k * k then
                    ks
                else if p % k = 0 then
                    factorize (p / k) k ks
                else
                    factorize p (k + 1) ((singleton k) @ ks)
            map (fun p -> factorize p 2 empty) arr

module QuadRope =
    let (@) = QuadRope.hcat
    let map = QuadRope.map
    let singleton = QuadRope.singleton
    let empty = Types.QuadRope.Empty

    let next i is =
        is @ (singleton i) @ (map ((+) i) is)

    let rec vanDerCorput n =
        if n <= 1 then
            singleton 0.5
        else
            next (pow 2.0 -n) (vanDerCorput (n - 1))

    let factorize arr =
        let rec factorize p k ks =
            if p < k * k then
                ks
            else if p % k = 0 then
                factorize (p / k) k ks
            else
                factorize p (k + 1) ((singleton k) @ ks)
        map (fun p -> factorize p 2 empty) arr

    module Parallel =
        let map = Parallel.QuadRope.map

        let next i is =
            is @ (singleton i) @ (map ((+) i) is)

        let rec vanDerCorput n =
            if n <= 1 then
                singleton 0.5
            else
                next (pow 2.0 -n) (vanDerCorput (n - 1))

        let factorize arr =
            let rec factorize p k ks =
                if p < k * k then
                    ks
                else if p % k = 0 then
                    factorize (p / k) k ks
                else
                    factorize p (k + 1) ((singleton k) @ ks)
            map (fun p -> factorize p 2 empty) arr
