module RadTrees.Utils

module Tasks =
    open System.Threading.Tasks

    let inline private task (f : unit -> 'a) =
        Task<'a>.Factory.StartNew(System.Func<'a>(f))

    let inline private result (t : _ Task) =
        t.Result

    let inline private await2 (t0 : _ Task) t1 =
        Task.WaitAll(t0, t1)

    let inline private await4 (t0 : _ Task) t1 t2 t3 =
        Task.WaitAll(t0, t1, t2, t3)

    /// Execute two functions f and g in parallel. This function
    /// blocks the current thread until f and g are computed and
    /// returns their (unwrapped) results.
    let par2 f g =
        let ft = task f
        let gt = task g
        await2 ft gt
        result ft, result gt

    /// Execute four functions in parallel. This function blocks the
    /// current thread until all functions are computed and returns
    /// their (unwrapped) results.
    let par4 f g h k =
        let ft = task f
        let gt = task g
        let ht = task h
        let kt = task k
        await4 ft gt ht kt
        result ft, result gt, result ht, result kt

module Bits =

    let inline radix bits =
        1 <<< bits

    let inline mask bits =
        (radix bits) - 1

    let inline index bits depth i =
        (i >>> (depth * bits)) &&& mask bits

module Array2D =

    let inline call f arr =
        f 0 0 (Array2D.length1 arr) (Array2D.length2 arr) arr

    /// Return a fresh copy of arr with the value at i,j replaced with v.
    let inline set arr i j v =
        let arr0 = Array2D.copy arr
        arr0.[i, j] <- v
        arr0

    let inline slice arr i j h w =
        if i <= 0 && j <= 0 && Array2D.length1 arr <= h && Array2D.length2 arr <= w then
            arr
        else
            let i0 = max 0 i
            let j0 = max 0 j
            Array2D.init (min h (Array2D.length1 arr - i0)) (min w (Array2D.length2 arr - j0)) (fun i j -> arr.[i0 + i, j0 + j])

    let inline isSingleton arr =
        Array2D.length1 arr = 1 && Array2D.length2 arr = 1

    /// Concatenate two arrays in first dimension.
    let inline cat1 left right =
        if Array2D.length2 left <> Array2D.length2 right then failwith "length2 must be equal!"
        let l1 = Array2D.length1 left + Array2D.length1 right
        let l2 = Array2D.length2 left
        let l1l = Array2D.length1 left
        Array2D.init l1 l2 (fun i j -> if i < l1l then left.[i, j] else right.[i - l1l, j])

    /// Concatenate two arrays in second dimension.
    let inline cat2 left right =
        if Array2D.length1 left <> Array2D.length1 right then failwith "length1 must be equal!"
        let l1 = Array2D.length1 left
        let l2 = Array2D.length2 left + Array2D.length2 right
        let l2l = Array2D.length2 left
        Array2D.init l1 l2 (fun i j -> if j < l2l then left.[i, j] else right.[i, j - l2l])

    /// Revert an array in first dimension.
    let inline revBased1 i0 j0 h w (arr : _ [,]) =
        let h0 = h - 1
        Array2D.init h w (fun i j -> arr.[h0 - (i0 + i), j0 + j])

    /// Revert an array in second dimension.
    let inline revBased2 i0 j0 h w (arr : _ [,]) =
        let w0 = w - 1
        Array2D.init h w (fun i j -> arr.[i0 + i, w0 - (j0 + j)])

    let inline rev1 arr = call revBased1 arr
    let inline rev2 arr = call revBased2 arr

    /// Fold each column of a 2D array, calling state with each column to get the state.
    let inline foldBased1 f state i0 j0 h w (arr : _ [,]) =
        let inline fold _ j =
            Seq.fold f (state j) (seq { for i in i0 .. i0 + h - 1 -> arr.[i, j0 + j] })
        Array2D.init 1 w fold

    /// Fold each row of a 2D array, calling state with each row to get the state.
    let inline foldBased2 f state i0 j0 h w (arr : _ [,]) =
        let inline fold i _ =
            Seq.fold f (state i) (seq { for j in j0 .. j0 + w - 1 -> arr.[i0 + i, j] })
        Array2D.init h 1 fold

    let inline fold1 f state arr = call (foldBased1 f state) arr
    let inline fold2 f state arr = call (foldBased2 f state) arr

    let inline exclusiveScan f s p g =
        let inline unfold (i, s) =
            if p i then
                let s' = f s (g i)
                Some (s', (i + 1, s'))
            else
                None
        Array.unfold unfold (0, s)

    // Compute the column-wise prefix sum for f.
    let inline scanBased1 f state i0 j0 h w (arr : _ [,]) =
        let arr' = [| for j in 0 .. w - 1 ->
                       exclusiveScan f (state j) ((>) h) (fun i -> Array2D.get arr (i0 + i) (j0 + j)) |]
        Array2D.init h w (fun i j -> Array.get (Array.get arr' j) i)

    // Compute the row-wise prefix sum for f.
    let inline scanBased2 f state i0 j0 h w (arr : _ [,]) =
        array2D [| for i in 0 .. h - 1 ->
                    exclusiveScan f (state i) ((>) w) (fun j -> Array2D.get arr (i0 + i) (j0 + j)) |]

    let inline scan1 f state arr = call (scanBased1 f state) arr
    let inline scan2 f state arr = call (scanBased2 f state) arr

    let inline map2 f (arr0 : _ [,]) (arr1 : _ [,]) =
        Array2D.init (Array2D.length1 arr0) (Array2D.length2 arr0) (fun i j -> f arr0.[i, j] arr1.[i, j])

    /// Reduce each column of a 2D array.
    let inline mapReduceBased1 f g i0 j0 h w (arr : _ [,]) =
        let inline reduce _ j =
            Seq.reduce g (seq { for i in i0 .. i0 + h - 1 -> f arr.[i0 + i, j0 + j] })
        Array2D.init 1 w reduce

    /// Reduce each row of a 2D array.
    let inline mapReduceBased2 f g i0 j0 h w (arr : _ [,]) =
        let inline reduce i _ =
            Seq.reduce g (seq { for j in j0 .. j0 + w - 1 -> f arr.[i0 + i, j0 + j] })
        Array2D.init h 1 reduce

    let inline mapReduce1 f g arr = call (mapReduceBased1 f g) arr
    let inline mapReduce2 f g arr = call (mapReduceBased2 f g) arr

    let inline reduceBased1 f i0 j0 h w arr = mapReduceBased1 id f i0 j0 h w arr
    let inline reduceBased2 f i0 j0 h w arr = mapReduceBased2 id f i0 j0 h w arr

    let inline reduce1 f arr = call (reduceBased1 f) arr
    let inline reduce2 f arr = call (reduceBased2 f) arr

    let inline mapReduceBased f g i0 j0 h w (arr : _ [,]) =
        Seq.reduce g (Seq.map f (seq { for i in i0 .. h - 1 do for j in j0 .. w - 1 -> arr.[i, j] }))

    let inline mapReduce f g arr = call (mapReduceBased f g) arr
    let inline reduceBased f i0 j0 h w arr = mapReduceBased id f i0 j0 h w arr
    let inline reduce f arr = call (reduceBased f) arr

    let inline sortBased1 p i0 j0 h w (arr : _ [,]) =
        let inline sort j =
            Array.sortBy p [| for i in i0 .. i0 + h - 1 -> arr.[i, + j] |]
        let arr' = [| for j in j0 .. j0 + w - 1 -> sort j |]
        Array2D.init h w (fun i j -> Array.get (Array.get arr' j) i)

    let inline sortBased2 p i0 j0 h w (arr : _ [,]) =
        let inline sort i =
            Array.sortBy p [| for j in j0 .. j0 + w - 1 -> arr.[i, j] |]
        array2D [| for i in i0 .. i0 + h - 1 -> sort i |]

    let inline sort1 p arr = call p arr
    let inline sort2 p arr = call p arr

    /// Initialize a 2D array with all zeros.
    let inline initZeros h w =
        Array2D.init h w (fun _ _ -> 0)

module Fibonacci =

    let private fibs =
        let rec fibr n n0 n1 =
            let n2 = n0 + n1
            seq { yield (n, n2); yield! fibr (n + 1) n1 n2 }
        seq { yield (0, 0); yield (1, 1); yield (2, 1); yield! fibr 3 1 1 } |> Seq.cache

    /// Return the nth Fibonacci number and cache it.
    let fib n =
        (Seq.item n >> snd) fibs

    /// Return the n of the first Fibonacci number that is greater than m.
    let nth m =
        fst (Seq.find (snd >> ((<=) m)) fibs)

/// This module contains a bunch of functions that convert a C#
/// function into an F# function conveniently. More versions for
/// even more parameters will probably be added in the future.
module Functions =
    open System

    let inline toFunc0 (f : 'a Func) =
        let f0 () =
            f.Invoke()
        f0

    let inline toFunc1 (f : ('b0, 'a) Func) =
        let f1 b0 =
            f.Invoke(b0)
        f1

    let inline toFunc2 (f : ('b0, 'b1, 'a) Func) =
        let f2 b0 b1 =
            f.Invoke(b0, b1)
        f2

    let inline toFunc3 (f : ('b0, 'b1, 'b2, 'a) Func) =
        let f3 b0 b1 b2 =
            f.Invoke(b0, b1, b2)
        f3

    let inline toFunc4 (f : ('b0, 'b1, 'b2, 'b3, 'a) Func) =
        let f4 b0 b1 b2 b3 =
            f.Invoke(b0, b1, b2, b3)
        f4
