namespace RadixTree

module Bits =

    let inline radix bits =
        1 <<< bits

    let inline mask bits =
        (radix bits) - 1

    let inline index bits depth i =
        (i >>> (depth * bits)) &&& mask bits

module Array2D =

    (* Return a fresh copy of arr with the value at i,j replaced with v. *)
    let set arr i j v =
        let arr0 = Array2D.copy arr
        arr0.[i, j] <- v
        arr0

    let slice arr imin jmin imax jmax =
        Array2D.init (imax - imin) (jmax - jmin) (fun i j -> Array2D.get arr (imin + i) (jmin + j))

    let head1 arr =
        slice arr 0 1 0 (Array2D.length2 arr)

    let head2 arr =
        slice arr 0 (Array2D.length1 arr) 0 1

    let tail1 arr =
        slice arr 1 (Array2D.length1 arr) 0 (Array2D.length2 arr)

    let tail2 arr =
        slice arr 0 (Array2D.length1 arr) 1 (Array2D.length2 arr)

    let init1 arr =
        slice arr 0 (Array2D.length1 arr - 1) 0 (Array2D.length2 arr)

    let init2 arr =
        slice arr 0 (Array2D.length1 arr) 0 (Array2D.length2 arr - 1)

    let last1 arr =
        let l1 = Array2D.length1 arr
        slice arr (l1 - 1) l1 0 (Array2D.length2 arr)

    let last2 arr =
        let l2 = Array2D.length2 arr
        slice arr 0 (Array2D.length1 arr) (l2 - 1) l2

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

    let hmerge f left right =
        let l1 = min (Array2D.length1 left) (Array2D.length1 right)
        let l2 = Array2D.length2 left + Array2D.length2 right - 1
        let l2l = Array2D.length2 left
        let merge i j =
            if j < l2l then
                left.[i, j]
            else if j = l2l then
                f left.[i, j] right.[i, 0]
            else
                right.[i, j - l2l + 1]
        Array2D.init l1 l2 merge

    let vmerge f left right =
        let l1 = min (Array2D.length1 left) (Array2D.length1 right)
        let l2 = Array2D.length2 left + Array2D.length2 right - 1
        let l1l = Array2D.length1 left
        let merge i j =
            if i < l1l then
                left.[i, j]
            else if i = l1l then
                f left.[i, j] right.[0, j]
            else
                right.[i - l1l + 1, j]
        Array2D.init l1 l2 merge

    let bottomLeft arr =
        Array2D.get arr (Array2D.length1 arr - 1) (Array2D.length2 arr - 1)

    let map2 f ass bss =
        let l1 = min (Array2D.length1 ass) (Array2D.length1 bss)
        let l2 = min (Array2D.length2 ass) (Array2D.length2 bss)
        Array2D.init l1 l2 (fun i j -> f ass.[i, j] bss.[i, j])

    let mapTop f arr =
        Array2D.mapi (fun i _ e -> if i = 0 then f e else e)

    let mapBottom f arr  =
        let bot = Array2D.length1 arr - 1
        Array2D.mapi (fun i _ e -> if i = bot then f e else e)

    let mapLeft f arr =
        Array2D.mapi (fun _ j e -> if j = 0 then f e else e)

    let mapRight f arr  =
        let right = Array2D.length2 arr - 1
        Array2D.mapi (fun _ j e -> if j = right then f e else e)

    let makeSingleColumn2 a b =
        Array2D.init 2 1 (fun i _ -> if i = 0 then a else b)

    let makeSingleColumn arr =
        Array2D.init (Array.length arr) 1 (fun i _ -> arr.[i])

    let makeSingleRow2 a b =
        Array2D.init 1 2 (fun _ j -> if j = 0 then a else b)

    let makeSingleRow arr =
        Array2D.init 1 (Array.length arr) (fun _ j -> arr.[j])
