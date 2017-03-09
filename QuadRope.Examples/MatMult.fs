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

    let mmultOpt = mmultBuilder QuadRope.SparseDouble.pointwise
                                Parallel.QuadRope.transpose
                                Parallel.QuadRope.init
                                QuadRope.SparseDouble.sum


module Array2D =

    let private mmultBuilder pointwise transpose init sum =
        let mmult (lm : float [,]) rm =
            let trm = transpose rm
            init (Array2D.length1 lm)
                 (Array2D.length2 rm)
                 (fun i j ->
                  let lr = Array2DExt.slice i 0 1 (Array2D.length2 lm) lm
                  let rr = Array2DExt.slice j 0 1 (Array2D.length2 trm) trm
                  sum (pointwise lr rr))
        mmult


    let mmult = mmultBuilder (Array2DExt.map2 (*))
                             Array2DExt.transpose
                             Array2D.init
                             (Array2DExt.reduce (+))


    let mmultPar = mmultBuilder (Parallel.Array2DExt.map2 (*))
                                Parallel.Array2DExt.transpose
                                Parallel.Array2DExt.init
                                (Parallel.Array2DExt.reduce (+))



module Imperative =

    let mmult (lm : float [,]) rm =
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


module ImperativeQuadRope =

    let private mmultBuilder mmult toArray2D lm rm =
        QuadRope.fromArray2D (mmult (toArray2D lm) (toArray2D rm))

    let mmult    = mmultBuilder Imperative.mmult QuadRope.toArray2D
    let mmultPar = mmultBuilder Imperative.mmultPar Parallel.QuadRope.toArray2D
