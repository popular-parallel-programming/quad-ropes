// Copyright (c) 2016 Florian Biermann, fbie@itu.dk

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:

// * The above copyright notice and this permission notice shall be
//   included in all copies or substantial portions of the Software.

// * The software is provided "as is", without warranty of any kind,
//   express or implied, including but not limited to the warranties of
//   merchantability, fitness for a particular purpose and
//   noninfringement. In no event shall the authors or copyright holders be
//   liable for any claim, damages or other liability, whether in an action
//   of contract, tort or otherwise, arising from, out of or in connection
//   with the software or the use or other dealings in the software.

module QuadRopes.Examples

open QuadRopes
open QuadRopes.Types
open QuadRopes.Utils.Tasks

module private Functions =
    let inline pow x y = System.Math.Pow ((float x), (float y))

open Functions

module Array2D =
    let (@) = Array2D.cat2
    let map = Array2D.map
    let singleton = Array2D.singleton
    let empty = Array2D.initZeros 1 0
    let get = Array2D.get
    let set = Array2D.set
    let transpose = Array2D.transpose
    let rows = Array2D.length1
    let init = Array2D.init
    let cols = Array2D.length2
    let slice = Array2D.slice
    let zip = Array2D.map2
    let reduce = Array2D.reduce

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
                factorize p (k + 1) (k :: ks)
        map (fun p -> factorize p 2 []) arr

    let rec fibseq n =
        match n with
            | 0 -> singleton 0.0
            | 1 -> singleton 1.0 @ singleton 1.0
            | _ ->
                let prefix = fibseq (n-1)
                let fa = get prefix 0 (n-2)
                let fb = get prefix 0 (n-1)
                prefix @ singleton (fa + fb)

    /// The sieve of Erastothenes
    let sieve n =
        /// Find next element that is larger than p; assumes ns is sorted.
        let rec find p i ns =
            if i >= rows ns then
                p
            else
                let p' = get ns i 0
                if p' <= p then
                    find p (i + 1) ns
                else
                    p'

        /// Enumerate all multiples of p in ns and set them to 0.
        let rec enumerate p c ns =
            let pc = p * c
            if pc < rows ns then
                enumerate p (c + 1) (set ns pc 0 0)
            else
                ns

        /// Mark multiples of p; all unmarked values are primes.
        let rec sieve lim p ns =
            let ns' = enumerate p 2 ns
            let p' = find p (p + 1) ns'
            if p' <= lim then
                sieve lim p' ns'
            else
                ns'

        // Call recursive function with initial args.
        sieve (int (sqrt (float n))) 2 (init n 1 (+))

    let mmult (lm : double [,]) rm =
        let trm = transpose rm
        init (rows lm)
             (cols rm)
             (fun i j ->
              let lr = slice i 0 1 (cols lm) lm
              let rr = slice j 0 1 (cols trm) trm
              reduce (+) (zip (*) lr rr))

    let mmultImp (lm : double [,]) rm =
        let res = Array2D.zeroCreate (rows lm) (cols rm)
        for i = 0 to Array2D.length1 lm - 1 do
            for j = 0 to Array2D.length2 rm - 1 do
                for k = 0 to Array2D.length2 lm - 1 do
                    res.[i, j] <- res.[i, j] + lm.[i, k] * rm.[k, j]
        res

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
                    factorize p (k + 1) (k :: ks)
            map (fun p -> factorize p 2 []) arr

        let transpose = Parallel.Array2D.transpose
        let init = Parallel.Array2D.init
        let slice = Array2D.slice
        let zip = Parallel.Array2D.map2
        let reduce = Parallel.Array2D.reduce

        let mmult (lm : double [,]) rm =
            let trm = transpose rm
            init (rows lm)
                 (cols rm)
                 (fun i j ->
                  let lr = slice i 0 1 (cols lm) lm
                  let rr = slice j 0 1 (cols trm) trm
                  reduce (+) (zip (*) lr rr))

        let mmultImp (lm : double [,]) (rm : double [,]) =
            let tm = Array2D.zeroCreate (rows lm) (cols rm)
            parfor 0 (Array2D.length1 lm) (fun i ->
                for j = 0 to Array2D.length2 rm - 1 do
                    for k = 0 to Array2D.length2 lm - 1 do
                        tm.[i, j] <- tm.[i, j] + lm.[i, k] * rm.[k, j])
            tm

module QuadRope =
    let (@) = QuadRope.hcat
    let map = QuadRope.map
    let singleton = QuadRope.singleton
    let empty = Types.QuadRope.Empty
    let get = QuadRope.get
    let set = QuadRope.set
    let transpose = QuadRope.transpose
    let rows = QuadRope.rows
    let init = QuadRope.init
    let cols = QuadRope.cols
    let slice = QuadRope.slice
    let zip = QuadRope.zip
    let reduce = QuadRope.reduce

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
                factorize p (k + 1) (k :: ks)
        map (fun p -> factorize p 2 []) arr

    let rec fibseq n =
        match n with
            | 0 -> singleton 0.0
            | 1 -> singleton 1.0 @ singleton 1.0
            | _ ->
                let prefix = fibseq (n-1)
                let fa = get prefix 0 (n-2)
                let fb = get prefix 0 (n-1)
                prefix @ singleton (fa + fb)

    /// The sieve of Erastothenes.
    let sieve n =
        /// Find next element that is larger than p; assumes ns is sorted.
        let rec find p i ns =
            if i >= rows ns then
                p
            else
                let p' = get ns i 0
                if p' <= p then
                    find p (i + 1) ns
                else
                    p'

        /// Enumerate all multiples of p in ns and set them to 0.
        let rec enumerate p c ns =
            let pc = p * c
            if pc < rows ns then
                enumerate p (c + 1) (set ns pc 0 0)
            else
                ns

        /// Mark multiples of p; all unmarked values are primes.
        let rec sieve lim p ns =
            let ns' = enumerate p 2 ns
            let p' = find p (p + 1) ns'
            if p' <= lim then
                sieve lim p' ns'
            else
                ns'

        // Call recursive function with initial args.
        sieve (int (sqrt (float n))) 2 (init n 1 (+))

    let sum = QuadRope.SparseDouble.sum
    let pointwise = QuadRope.SparseDouble.pointwise

    let mmult (lm : double QuadRope) rm =
        let trm = transpose rm
        init (rows lm)
             (cols rm)
             (fun i j ->
              let lr = slice i 0 1 (cols lm) lm
              let rr = slice j 0 1 (cols trm) trm
              sum (pointwise lr rr))

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
                    factorize p (k + 1) (k :: ks)
            map (fun p -> factorize p 2 []) arr

        let transpose = Parallel.QuadRope.transpose
        let init = Parallel.QuadRope.init
        let zip = Parallel.QuadRope.zip
        let reduce = Parallel.QuadRope.reduce

        let sum = Parallel.QuadRope.SparseDouble.sum
        let pointwise = Parallel.QuadRope.SparseDouble.pointwise

        let mmult (lm : double QuadRope) rm =
            let trm = transpose rm
            init (rows lm)
                 (cols rm)
                 (fun i j ->
                  let lr = slice i 0 1 (cols lm) lm
                  let rr = slice j 0 1 (cols trm) trm
                  sum (pointwise lr rr))
