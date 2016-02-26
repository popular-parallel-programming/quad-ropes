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

    let inline slice arr xmin ymin xmax ymax =
        Array2D.init (xmax - xmin) (ymax - ymin) (fun x y -> Array2D.get arr (xmin + x) (ymin + y))

    let width = Array2D.length1
    let height = Array2D.length2

    (* Chunk the array into subarrays of size n * m. The arrays to the
       right and in the bottom might be smaller if the width and height
       are not multiples of n and m, respectively. *)
    let chunkBySize n m arr =
        let subArr xmin ymin =
            let xmax = (min ((xmin + 1) * n) (width arr))
            let ymax = (min ((ymin + 1) * m) (height arr))
            slice arr (xmin * n) (ymin * m) xmax ymax
        (* TODO: Optimize below. *)
        let w = if width arr <= n then 1 else width arr / n + 1
        let h = if height arr <= m then 1 else height arr / m + 1
        Array2D.init w h subArr

    let inline isSingleColumn arr =
        width arr = 1

    let inline isSingleRow arr =
        height arr = 1

    let inline isSingleton arr =
        width arr = 1 && height arr = 1
