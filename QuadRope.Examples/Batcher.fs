module QuadRope.Examples.Batcher

open QuadRope.Types
open QuadRope

module Common =
    let batcherBuilder init append len rev slice zipWith =
        let bottop xs =
            let l = len xs in
            let l2 = l / 2 in
            slice 0 0 (l2) 1 xs, slice (l2) 0 (l - 1) 1 xs
        in
        let rec bitonic_sort xs =
            if len xs = 1 then xs
            else
                let bot, top = bottop xs in
                let mins = zipWith min bot top in
                let maxs = zipWith max bot top in
                append (bitonic_sort mins) (bitonic_sort maxs)
        in
        let rec batcher_sort xs =
            if len xs = 1 then xs
            else
                let bot, top = bottop xs in
                let bot' = batcher_sort bot in
                let top' = batcher_sort top in
                bitonic_sort (append bot' (rev top'))
        in
        batcher_sort

module QuadRope =
    let batcher : int QuadRope -> int QuadRope =
        Common.batcherBuilder QuadRope.init
                              QuadRope.vcat
                              QuadRope.rows
                              QuadRope.vrev
                              QuadRope.slice
                              QuadRope.zip

    let batcherPar : int QuadRope -> int QuadRope =
        Common.batcherBuilder Parallel.QuadRope.init
                              QuadRope.vcat
                              QuadRope.rows
                              QuadRope.vrev
                              QuadRope.slice
                              Parallel.QuadRope.zip

module Array2D =
    let batcher : int [,] -> int [,] =
        Common.batcherBuilder Array2D.init
                              Array2DExt.cat1
                              Array2D.length1
                              Array2DExt.rev1
                              Array2DExt.slice
                              Array2DExt.map2

    let batcherPar : int [,] -> int [,] =
        Common.batcherBuilder Parallel.Array2DExt.init
                              Parallel.Array2DExt.cat1
                              Array2D.length1
                              Parallel.Array2DExt.rev1
                              Array2DExt.slice
                              Parallel.Array2DExt.map2
