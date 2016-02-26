namespace RadixTree

module Bits =

    let inline radix bits =
        1 <<< bits

    let inline mask bits =
        1 <<< (bits - 1)

    let inline index bits depth i =
        (i >>> (depth * bits)) &&& mask bits

module Array2D =

    (* Return a fresh copy of arr with the value at i,j replaced with v. *)
    let set arr i j v =
        let arr0 = Array2D.copy arr
        arr0.[i, j] <- v
        arr0

    let inline slice arr imin jmin imax jmax =
        Array2D.init (imax - imin) (jmax - jmin) (fun i j -> Array2D.get arr (imin + i) (jmin + j))

    (* Chunk the array into subarrays of size n * m. The arrays to the
       right and in the bottom might be smaller if the width and height
       are not multiples of n and m, respectively. *)
    let chunkBySize n m arr =
        let subArr imin jmin =
            let imax = (min ((imin + 1) * n) (Array2D.length1 arr))
            let jmax = (min ((jmin + 1) * m) (Array2D.length2 arr))
            slice arr (imin * n) (jmin * m) imax jmax
        (* TODO: Optimize below. *)
        let l1 = if Array2D.length1 arr <= n then 1 else Array2D.length1 arr / n + 1
        let l2 = if Array2D.length2 arr <= m then 1 else Array2D.length2 arr / m + 1
        Array2D.init l1 l2 subArr

    let inline isSingleton arr =
        Array2D.length1 arr = 1 && Array2D.length2 arr = 1

    let hcat left right =
        let l1 = min (Array2D.length1 left) (Array2D.length1 right)
        let l2 = Array2D.length2 left + Array2D.length2 right
        let l2l = Array2D.length2 left
        Array2D.init l1 l2 (fun i j -> if j < l2l then left.[i, j] else right.[i, j - l2l])

    let vcat left right =
        let l1 = Array2D.length1 left + Array2D.length1 right
        let l2 = min (Array2D.length2 left) (Array2D.length2 right)
        let l1l = Array2D.length1 left
        Array2D.init l1 l2 (fun i j -> if i < l1l then left.[i, j] else right.[i - l1l, j])
