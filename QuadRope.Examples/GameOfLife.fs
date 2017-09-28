// Copyright (c) 2017 Florian Biermann, fbie@itu.dk

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

module QuadRope.Examples.GameOfLife

open QuadRope

module Common =

    /// Count the number of neighbors as prefixes
    let count c d r x =
        snd c + snd d + snd r, snd x


    /// Evolve a cell based on its surrounding cells.
    let evolve (count, state) =
        match count with
            | 2 -> state // Live on
            | 3 -> 1     // Live on or be born.
            | _ -> 0     // All other cases involve death.

open Common

module QuadRope =

    // The types seem too complicated without either excessive
    // annotations or using the actual functions. The latter seems
    // more straight forward, so we just copy the code for now.

    module Seq =

        let map = QuadRope.map
        let zip = QuadRope.zip
        let scan = QuadRope.scan
        let hrev = QuadRope.hrev
        let vrev = QuadRope.vrev

        let private rev = hrev >> vrev

        /// Conway's Game of Life, using 2D-scan, reversal and zip.
        let rec internal gameOfLife n world =
            match n with
                | 0 -> world
                | n ->
                    let states = map (fun x -> 0, x) world
                    let counts = zip (fun a b -> fst a + fst b, snd a)
                                     (scan count (0, 0) states)
                                     (rev (scan count (0, 0) (rev states)))
                    gameOfLife (n - 1) (map evolve counts)


    module Par =

        let map = Parallel.QuadRope.map
        let zip = Parallel.QuadRope.zip
        let scan = Parallel.QuadRope.scan
        let hrev = Parallel.QuadRope.hrev
        let vrev = Parallel.QuadRope.vrev

        let private rev = hrev >> vrev

        /// Conway's Game of Life, using 2D-scan, reversal and zip.
        let rec internal gameOfLife n world =
            match n with
                | 0 -> world
                | n ->
                    let states = map (fun x -> 0, x) world
                    let counts = zip (fun a b -> fst a + fst b, snd a)
                                     (scan count (0, 0) states)
                                     (rev (scan count (0, 0) (rev states)))
                    gameOfLife (n - 1) (map evolve counts)



    /// Sequential Game of Life.
    let gameOfLife = Seq.gameOfLife

    /// Parallel Game of Life.
    let gameOfLifePar = Par.gameOfLife


module Array2D =

    let private rev = Array2DExt.rev1 >> Array2DExt.rev2


    /// Conway's Game of Life, using 2D-scan, reversal and zip.
    let rec gameOfLife n world =
        match n with
            | 0 -> world
            | n ->
                let states = Array2D.map (fun x -> 0, x) world
                let counts = Array2DExt.map2 (fun a b -> fst a + fst b, snd a)
                                             (Array2DExt.scan count (0, 0) states)
                                             (rev (Array2DExt.scan count (0, 0) (rev states)))
                gameOfLife (n - 1) (Array2D.map evolve counts)
