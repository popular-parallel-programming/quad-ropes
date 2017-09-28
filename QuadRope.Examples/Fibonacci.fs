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

module QuadRope.Examples.Fibonacci

open QuadRope

module QuadRope =
    let rec fibseq n =
        match n with
            | 0 -> QuadRope.singleton 0.0
            | 1 -> QuadRope.hcat (QuadRope.singleton 1.0) (QuadRope.singleton 1.0)
            | _ ->
                let prefix = fibseq (n-1)
                let fa = QuadRope.get prefix 0 (n-2)
                let fb = QuadRope.get prefix 0 (n-1)
                QuadRope.hcat prefix (QuadRope.singleton (fa + fb))



module Array2D =
    let rec fibseq n =
        match n with
            | 0 -> Array2DExt.singleton 0.0
            | 1 -> Array2DExt.cat2 (Array2DExt.singleton 1.0) (Array2DExt.singleton 1.0)
            | _ ->
                let prefix = fibseq (n-1)
                let fa = Array2D.get prefix 0 (n-2)
                let fb = Array2D.get prefix 0 (n-1)
                Array2DExt.cat2 prefix (Array2DExt.singleton (fa + fb))
