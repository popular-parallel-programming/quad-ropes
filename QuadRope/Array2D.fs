module RadTrees.Array2D

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
        Array2D.init (min h (Array2D.length1 arr - i0))
                     (min w (Array2D.length2 arr - j0))
                     (fun i j -> arr.[i0 + i, j0 + j])

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
let inline rev1 (arr : _ [,]) =
    let h0 = Array2D.length1 arr - 1
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> arr.[h0 - i, j])

/// Revert an array in second dimension.
let inline rev2 (arr : _ [,]) =
    let w0 = Array2D.length2 arr - 1
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> arr.[i, w0 - j])

/// Fold each column of a 2D array, calling state with each column to get the state.
let inline fold1 f state (arr : _ [,]) =
    let fold = fun _ j -> Seq.fold f (state j) (seq { for i in 0 .. Array2D.length1 arr - 1 -> arr.[i, j] })
    Array2D.init 1 (Array2D.length2 arr) fold

/// Fold each row of a 2D array, calling state with each row to get the state.
let inline fold2 f state (arr : _ [,]) =
    let fold = fun i _ -> Seq.fold f (state i) (seq { for j in 0 .. Array2D.length2 arr - 1 -> arr.[i, j] })
    Array2D.init (Array2D.length1 arr) 1 fold

let inline exclusiveScan f s p g =
    let unfold = (fun (i, s) ->
        if p i then
            let s' = f s (g i)
            Some (s', (i + 1, s'))
        else
            None)
    Array.unfold unfold (0, s)

/// Compute the column-wise prefix sum for f.
let inline scan1 f state (arr : _ [,]) =
    let arr' = [| for j in 0 .. Array2D.length2 arr - 1 ->
                   exclusiveScan f (state j) ((>) (Array2D.length1 arr)) (fun i -> Array2D.get arr i j) |]
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> Array.get (Array.get arr' j) i)

/// Compute the row-wise prefix sum for f.
let inline scan2 f state (arr : _ [,]) =
    array2D [| for i in 0 .. Array2D.length1 arr - 1 ->
                exclusiveScan f (state i) ((>) (Array2D.length2 arr)) (fun j -> Array2D.get arr i j) |]

let inline map2 f (arr0 : _ [,]) (arr1 : _ [,]) =
    Array2D.init (Array2D.length1 arr0) (Array2D.length2 arr0) (fun i j -> f arr0.[i, j] arr1.[i, j])

/// Reduce each column of a 2D array.
let inline mapReduce1 f g (arr : _ [,]) =
    Array2D.init 1 (Array2D.length2 arr)
                   (fun _ j -> Seq.reduce g (seq { for i in 0 .. Array2D.length1 arr - 1 -> f arr.[i, j] }))

/// Reduce each row of a 2D array.
let inline mapReduce2 f g (arr : _ [,]) =
    Array2D.init (Array2D.length1 arr) 1
                 (fun i _ -> Seq.reduce g (seq { for j in 0 .. Array2D.length2 arr - 1 -> f arr.[i, j] }))

let inline reduce1 f arr = mapReduce1 id f arr
let inline reduce2 f arr = mapReduce2 id f arr

let inline mapReduce f g (arr : _ [,]) =
    Seq.reduce g (Seq.map f (seq { for i in 0 .. (Array2D.length1 arr - 1) do for j in 0 .. (Array2D.length2 arr - 1) -> arr.[i, j] }))

let inline reduce f arr = mapReduce id f arr

let inline sort1 p (arr : _ [,]) =
    let sort = fun j -> Array.sortBy p [| for i in 0 .. Array2D.length1 arr - 1 -> arr.[i, + j] |]
    let arr' = [| for j in 0 .. Array2D.length2 arr - 1 -> sort j |]
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> Array.get (Array.get arr' j) i)

let inline sort2 p (arr : _ [,]) =
    let sort = fun i -> Array.sortBy p [| for j in 0 .. Array2D.length2 arr - 1 -> arr.[i, j] |]
    array2D [| for i in 0 .. Array2D.length1 arr - 1 -> sort i |]

/// Initialize a 2D array with all zeros.
let inline initZeros h w =
    Array2D.init h w (fun _ _ -> 0)

let inline transpose arr =
    Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun i j -> arr.[j, i])

let inline filter1 p arr =
    let vs = Seq.filter p (Seq.init (Array2D.length1 arr) (fun i -> arr.[i, 0])) |> Array.ofSeq
    Array2D.init (Array.length vs) 1 (fun i _ -> Array.get vs i)

let inline filter2 p arr =
    let vs = Seq.filter p (Seq.init (Array2D.length2 arr) (Array2D.get arr 0)) |> Array.ofSeq
    Array2D.init 1 (Array.length vs) (fun _ j -> Array.get vs j)
