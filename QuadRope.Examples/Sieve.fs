module QuadRopes.Examples.Sieve

open QuadRopes

module QuadRope =


    /// Find next element that is larger than p; assumes ns is sorted.
    let rec private find p i ns =
        if i >= QuadRope.rows ns then
            p
        else
            let p' = QuadRope.get ns i 0
            if p' <= p then
                find p (i + 1) ns
            else
                p'


    /// Enumerate all multiples of p in ns and set them to 0.
    let rec private enumerate p c ns =
        let pc = p * c
        if pc < QuadRope.rows ns then
            enumerate p (c + 1) (QuadRope.set ns pc 0 0)
        else
                ns


    /// The sieve of Erastothenes
    let sieve n =
        /// Mark multiples of p; all unmarked values are primes.
        let rec sieve lim p ns =
            let ns' = enumerate p 2 ns
            let p' = find p (p + 1) ns'
            if p' <= lim then
                sieve lim p' ns'
            else
                ns'

        // Call recursive function with initial args.
        sieve (int (sqrt (float n))) 2 (QuadRope.init n 1 (+))



module Array2D =

    /// Find next element that is larger than p; assumes ns is sorted.
    let rec private find p i ns =
        if i >= Array2D.length1 ns then
            p
        else
            let p' = Array2D.get ns i 0
            if p' <= p then
                find p (i + 1) ns
            else
                p'


    /// Enumerate all multiples of p in ns and set them to 0.
    let rec private enumerate p c ns =
        let pc = p * c
        if pc < Array2D.length1 ns then
            enumerate p (c + 1) (Array2DExt.set ns pc 0 0)
        else
            ns


    /// The sieve of Erastothenes
    let sieve n =
        /// Mark multiples of p; all unmarked values are primes.
        let rec sieve lim p ns =
            let ns' = enumerate p 2 ns
            let p' = find p (p + 1) ns'
            if p' <= lim then
                sieve lim p' ns'
            else
                ns'

        // Call recursive function with initial args.
        sieve (int (sqrt (float n))) 2 (Array2D.init n 1 (+))
