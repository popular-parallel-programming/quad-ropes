namespace RadTrees

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

    let subArr arr i j h w =
        if i <= 0 && Array2D.length1 arr <= h && j <= 0 && Array2D.length2 arr <= w then
            arr
        else if Array2D.length1 arr <= i || h <= 0 then
            Array2D.zeroCreate 0 w
        else if Array2D.length2 arr <= j || w <= 0 then
            Array2D.zeroCreate h 0
        else
            let i0 = max 0 i
            let j0 = max 0 j
            let h0 = min h (Array2D.length1 arr)
            let w0 = min w (Array2D.length2 arr)
            Array2D.initBased i0 j0 h0 w0 (Array2D.get arr)

    let slice arr imin jmin imax jmax =
         subArr arr imin jmin (imax - imin) (jmax - jmin)

    let inline isSingleton arr =
        Array2D.length1 arr = 1 && Array2D.length2 arr = 1

    (* Concatenate two arrays in first dimension. *)
    let cat1 left right =
        let l1 = Array2D.length1 left + Array2D.length1 right
        let l2 = min (Array2D.length2 left) (Array2D.length2 right)
        let l1l = Array2D.length1 left
        Array2D.init l1 l2 (fun i j -> if i < l1l then left.[i, j] else right.[i - l1l, j])

    (* Concatenate two arrays in second dimension. *)
    let cat2 left right =
        let l1 = min (Array2D.length1 left) (Array2D.length1 right)
        let l2 = Array2D.length2 left + Array2D.length2 right
        let l2l = Array2D.length2 left
        Array2D.init l1 l2 (fun i j -> if j < l2l then left.[i, j] else right.[i, j - l2l])

    (* Revert an array in first dimension. *)
    let rev1 arr =
        let i0 = Array2D.length1 arr - 1
        Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> arr.[i0 - i, j])

    (* Revert an array in second dimension. *)
    let rev2 arr =
        let j0  = Array2D.length2 arr - 1
        Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> arr.[i, j0 - j])

    (* Fold each column of a 2D array, calling state with each row to get the state. *)
    let fold1 f state arr =
        let fold _ j =
            Seq.fold f (state j) (seq { for i in 0 .. Array2D.length1 arr - 1 -> arr.[i, j] })
        Array2D.init 1 (Array2D.length2 arr) fold

    (* Fold each column of a 2D array, calling state with each column to get the state. *)
    let fold2 f state arr =
        let fold i _ =
            Seq.fold f (state i) (seq { for j in 0 .. Array2D.length2 arr - 1 -> arr.[i, j] })
        Array2D.init (Array2D.length1 arr) 1 fold

    (* Initialize a 2D array with all zeros. *)
    let initZeros h w =
        Array2D.init h w (fun _ _ -> 0)

module Fibonacci =

    let private fibs =
        let rec fibr n n0 n1 =
            let n2 = n0 + n1
            seq { yield (n, n2); yield! fibr (n + 1) n1 n2 }
        seq { yield (0, 0); yield (1, 1); yield (2, 1); yield! fibr 3 1 1 } |> Seq.cache

    (* Return the nth Fibonacci number and cache it. *)
    let fib n =
        (Seq.item n >> snd) fibs

    (* Return the n of the first Fibonacci number that is greater than m. *)
    let nth m =
        fst (Seq.find (snd >> ((<) m)) fibs)

module Tuple =

    let tupled f =
        (fun (x, y) -> f x y)

module Option =

    let getDefault v s =
        match s with
            | None -> v
            | Some s -> s
