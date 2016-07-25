module RadTrees.Array2D

/// Return a fresh copy of arr with the value at i,j replaced with v.
let set arr i j v =
    let arr0 = Array2D.copy arr
    arr0.[i, j] <- v
    arr0

/// Allocate a new array of the required dimensions with values taken
/// from the original array.
let slice arr i j h w =
    if i <= 0 && j <= 0 && Array2D.length1 arr <= h && Array2D.length2 arr <= w then
        arr
    else
        let i0 = max 0 i
        let j0 = max 0 j
        Array2D.init (min h (Array2D.length1 arr - i0))
                     (min w (Array2D.length2 arr - j0))
                     (fun i j -> arr.[i0 + i, j0 + j])

let singleton v =
    Array2D.create 1 1 v

/// True if array contains only a single element.
let isSingleton arr =
    Array2D.length1 arr = 1 && Array2D.length2 arr = 1

/// True if array contains no elements.
let isEmpty arr =
    Array2D.length1 arr = 0 || Array2D.length2 arr = 0

/// Concatenate two arrays in first dimension.
let cat1 left right =
    if Array2D.length2 left <> Array2D.length2 right then failwith "length2 must be equal!"
    let l1 = Array2D.length1 left + Array2D.length1 right
    let l2 = Array2D.length2 left
    let l1l = Array2D.length1 left
    Array2D.init l1 l2 (fun i j -> if i < l1l then left.[i, j] else right.[i - l1l, j])

/// Concatenate two arrays in second dimension.
let cat2 left right =
    if Array2D.length1 left <> Array2D.length1 right then failwith "length1 must be equal!"
    let l1 = Array2D.length1 left
    let l2 = Array2D.length2 left + Array2D.length2 right
    let l2l = Array2D.length2 left
    Array2D.init l1 l2 (fun i j -> if j < l2l then left.[i, j] else right.[i, j - l2l])

/// Revert an array in first dimension.
let rev1 (arr : _ [,]) =
    let h0 = Array2D.length1 arr - 1
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> arr.[h0 - i, j])

/// Revert an array in second dimension.
let rev2 (arr : _ [,]) =
    let w0 = Array2D.length2 arr - 1
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> arr.[i, w0 - j])

/// Fold each column of a 2D array, calling state with each column to get the state.
let fold1 f state (arr : _ [,]) =
    Array2D.init 1 (Array2D.length2 arr)
                   (fun _ j ->
                    let mutable acc = state j
                    for i in 0 .. Array2D.length1 arr - 1 do
                        acc <- f acc arr.[i, j]
                    acc)

/// Fold each row of a 2D array, calling state with each row to get the state.
let fold2 f state (arr : _ [,]) =
        Array2D.init (Array2D.length1 arr) 1
                 (fun i _ ->
                  let mutable acc = state i
                  for j in 0 .. Array2D.length2 arr - 1 do
                      acc <- f acc arr.[i, j]
                  acc)

let private exclusiveScan f s p g =
    let unfold = (fun (i, s) ->
        if p i then
            let s' = f s (g i)
            Some (s', (i + 1, s'))
        else
            None)
    Array.unfold unfold (0, s)

/// Compute the column-wise prefix sum for f.
let scan1 f state (arr : _ [,]) =
    let arr' = [| for j in 0 .. Array2D.length2 arr - 1 ->
                   exclusiveScan f (state j) ((>) (Array2D.length1 arr)) (fun i -> Array2D.get arr i j) |]
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> Array.get (Array.get arr' j) i)

/// Compute the row-wise prefix sum for f.
let scan2 f state (arr : _ [,]) =
    array2D [| for i in 0 .. Array2D.length1 arr - 1 ->
                exclusiveScan f (state i) ((>) (Array2D.length2 arr)) (fun j -> Array2D.get arr i j) |]

let map2 f (arr0 : _ [,]) (arr1 : _ [,]) =
    Array2D.init (Array2D.length1 arr0) (Array2D.length2 arr0) (fun i j -> f arr0.[i, j] arr1.[i, j])

/// Reduce each column of a 2D array.
let mapReduce1 f g (arr : _ [,]) =
    Array2D.init 1 (Array2D.length2 arr)
                   (fun _ j ->
                    let mutable acc = f arr.[0, j]
                    for i in 1 .. Array2D.length1 arr - 1 do
                        acc <- g acc (f arr.[i, j])
                    acc)

/// Reduce each row of a 2D array.
let mapReduce2 f g (arr : _ [,]) =
    Array2D.init (Array2D.length1 arr) 1
                 (fun i _ ->
                  let mutable acc = f arr.[i, 0]
                  for j in 1 .. Array2D.length2 arr - 1 do
                      acc <- g acc (f arr.[i, j])
                  acc)

let reduce1 f arr = mapReduce1 id f arr
let reduce2 f arr = mapReduce2 id f arr

/// Map a function f to all values in the array and combine the
/// results using g.
let mapReduce f g (arr : _ [,]) =
    let mutable acc = f arr.[0, 0]
    for j in 1 .. Array2D.length2 arr - 1 do
        acc <- g acc (f arr.[0, j])
    for i in 1 .. Array2D.length1 arr - 1 do
        for j in 0 .. Array2D.length2 arr - 1 do
            acc <- g acc (f arr.[i, j])
    acc

let reduce f arr = mapReduce id f arr

let sort1 p (arr : _ [,]) =
    let sort = fun j -> Array.sortBy p [| for i in 0 .. Array2D.length1 arr - 1 -> arr.[i, + j] |]
    let arr' = [| for j in 0 .. Array2D.length2 arr - 1 -> sort j |]
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun i j -> Array.get (Array.get arr' j) i)

let sort2 p (arr : _ [,]) =
    let sort = fun i -> Array.sortBy p [| for j in 0 .. Array2D.length2 arr - 1 -> arr.[i, j] |]
    array2D [| for i in 0 .. Array2D.length1 arr - 1 -> sort i |]

/// Initialize a 2D array with all zeros.
let initZeros h w =
    Array2D.init h w (fun _ _ -> 0)

let transpose arr =
    Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun i j -> arr.[j, i])

let filter1 p arr =
    let vs = Seq.filter p (Seq.init (Array2D.length1 arr) (fun i -> arr.[i, 0])) |> Array.ofSeq
    Array2D.init (Array.length vs) 1 (fun i _ -> Array.get vs i)

let filter2 p arr =
    let vs = Seq.filter p (Seq.init (Array2D.length2 arr) (Array2D.get arr 0)) |> Array.ofSeq
    Array2D.init 1 (Array.length vs) (fun _ j -> Array.get vs j)
