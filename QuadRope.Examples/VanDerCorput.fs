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

module QuadRope.Examples.VanDerCorput

open QuadRope

module private Math =
    let inline pow x y = System.Math.Pow ((float x), (float y))


module QuadRope =
    let private vdcBuilder map =
        let next i is =
            QuadRope.hcat is (QuadRope.hcat (QuadRope.singleton i) (map ((+) i) is))

        let rec vdc n =
            if n <= 1 then
                QuadRope.singleton 0.5
            else
                next (Math.pow 2.0 -n) (vdc (n - 1))

        vdc

    let vanDerCorput = vdcBuilder QuadRope.map
    let vanDerCorputPar = vdcBuilder Parallel.QuadRope.map



module Array2D =
    let private vdcBuilder cat map =
        let next i is =
            cat is (cat (Array2DExt.singleton i) (map ((+) i) is))

        let rec vdc n =
            if n <= 1 then
                Array2DExt.singleton 0.5
            else
                next (Math.pow 2.0 -n) (vdc (n - 1))

        vdc


    let vanDerCorput = vdcBuilder Array2DExt.cat2 Array2D.map
    let vanDerCorputPar = vdcBuilder Parallel.Array2DExt.cat2 Parallel.Array2DExt.map
