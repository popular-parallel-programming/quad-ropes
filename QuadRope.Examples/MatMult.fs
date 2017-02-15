module RadTrees.Examples.MatMult

open RadTrees

module QuadRope =
    let private mmultBuilder pointwise transpose init sum =
        let mmult lm rm =
            let trm = transpose rm
            init (QuadRope.rows lm)
                 (QuadRope.cols rm)
                 (fun i j ->
                  let lr = QuadRope.slice i 0 1 (QuadRope.cols lm) lm
                  let rr = QuadRope.slice j 0 1 (QuadRope.cols trm) trm
                  sum (pointwise lr rr))
        mmult

    let mmult = mmultBuilder QuadRope.SparseDouble.pointwise
                             QuadRope.transpose
                             QuadRope.init
                             QuadRope.SparseDouble.sum

    let mmultPar = mmultBuilder Parallel.QuadRope.SparseDouble.pointwise
                                Parallel.QuadRope.transpose
                                Parallel.QuadRope.init
                                Parallel.QuadRope.SparseDouble.sum


module Array2D =
    let private mmultBuilder pointwise transpose init sum =
        let mmult lm rm =
            let trm = transpose rm
            init (Array2D.length1 lm)
                 (Array2D.length2 rm)
                 (fun i j ->
                  let lr = Array2D.slice i 0 1 (Array2D.length2 lm) lm
                  let rr = Array2D.slice j 0 1 (Array2D.length2 trm) trm
                  sum (pointwise lr rr))
        mmult

    let mmult = mmultBuilder (Array2D.map2 (*))
                             Array2D.transpose
                             Array2D.init
                             (Array2D.reduce (+))

    let mmultPar = mmultBuilder (Parallel.Array2D.map2 (*))
                                Parallel.Array2D.transpose
                                Parallel.Array2D.init
                                (Parallel.Array2D.reduce (+))

module Imperative =
    let mmult (lm : double [,]) rm =
        let res = Array2D.zeroCreate (Array2D.length1 lm) (Array2D.length2 rm)
        for i = 0 to Array2D.length1 lm - 1 do
            for k = 0 to Array2D.length2 lm - 1 do
                for j = 0 to Array2D.length2 rm - 1 do
                    res.[i, j] <- res.[i, j] + lm.[i, k] * rm.[k, j]
        res

    let mmultPar (lm : double [,]) (rm : double [,]) =
        let tm = Array2D.zeroCreate (Array2D.length1 lm) (Array2D.length2 rm)
        Utils.Tasks.parfor 0 (Array2D.length1 lm) (fun i ->
            for k = 0 to Array2D.length2 lm - 1 do
                for j = 0 to Array2D.length2 rm - 1 do
                    tm.[i, j] <- tm.[i, j] + lm.[i, k] * rm.[k, j])
        tm
