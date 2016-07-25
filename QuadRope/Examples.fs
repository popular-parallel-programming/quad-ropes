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
    let get = Array2D.get

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

    let rec fibseq n =
        match n with
            | 0 -> singleton 0.0
            | 1 -> singleton 1.0 @ singleton 1.0
            | _ ->
                let prefix = fibseq (n-1)
                let fa = get prefix 0 (n-2)
                let fb = get prefix 0 (n-1)
                prefix @ singleton (fa + fb)

    let transpose = Array2D.transpose
    let rows = Array2D.length1
    let init = Array2D.init
    let cols = Array2D.length2
    let slice = Array2D.slice
    let zip = Array2D.map2
    let reduce = Array2D.reduce

    let matmult lm rm =
        let trm = transpose rm
        let elm = init (rows lm) (cols rm) (fun i _ -> slice lm  i 0 1 (cols lm))
        let erm = init (rows lm) (cols rm) (fun _ j -> slice trm j 0 1 (cols trm))
        zip (fun l r -> reduce (+) (zip (*) l r)) elm erm

    module Parallel =
        let (@) = Parallel.Array2D.cat1
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
    let get = QuadRope.get

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

    let rec fibseq n =
        match n with
            | 0 -> singleton 0.0
            | 1 -> singleton 1.0 @ singleton 1.0
            | _ ->
                let prefix = fibseq (n-1)
                let fa = get prefix 0 (n-2)
                let fb = get prefix 0 (n-1)
                prefix @ singleton (fa + fb)

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
