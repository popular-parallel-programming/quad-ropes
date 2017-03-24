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

module QuadRopes.Examples.Factorize

open QuadRopes

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
