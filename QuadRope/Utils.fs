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
        if i <= 0 && Array2D.length1 arr <= i + h && j <= 0 && Array2D.length2 arr <= j + w then
            arr
        else
            let i0 = max 0 i
            let j0 = max 0 j
            let h0 = min h (Array2D.length1 arr - i0)
            let w0 = min w (Array2D.length2 arr - j0)
            Array2D.initBased i0 j0 h0 w0 (Array2D.get arr)

    let slice arr imin jmin imax jmax =
         subArr arr imin jmin (imax - imin) (jmax - jmin)

    let inline isSingleton arr =
        Array2D.length1 arr = 1 && Array2D.length2 arr = 1

    let append1 bss bs =
        let l1 = Array2D.length1 bss
        Array2D.init (l1 + 1) (Array2D.length2 bss) (fun i j -> if i < l1 then bss.[i, j] else Array.get bs j)

    let append2 bss bs =
        let l2 = Array2D.length2 bss
        Array2D.init (Array2D.length1 bss) (l2 + 1) (fun i j -> if j < l2 then bss.[i, j] else Array.get bs j)

    let cat1 left right =
        let l1 = Array2D.length1 left + Array2D.length1 right
        let l2 = min (Array2D.length2 left) (Array2D.length2 right)
        let l1l = Array2D.length1 left
        Array2D.init l1 l2 (fun i j -> if i < l1l then left.[i, j] else right.[i - l1l, j])

    let cat2 left right =
        let l1 = min (Array2D.length1 left) (Array2D.length1 right)
        let l2 = Array2D.length2 left + Array2D.length2 right
        let l2l = Array2D.length2 left
        Array2D.init l1 l2 (fun i j -> if j < l2l then left.[i, j] else right.[i, j - l2l])

    let rev1 arr =
        let i0 = Array2D.length1 arr - 1
        Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> arr.[i0 - i, j])

    let rev2 arr =
        let j0  = Array2D.length2 arr - 1
        Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> arr.[i, j0 - j])

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
