namespace RadTrees

type 'a Array2DView =
    | All of 'a [,]
    | View of int * int * int * int * 'a [,]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array2DView =

    let inline private all arr =
        All arr

    let inline private view i j h w arr =
        View (i, j, h, w, arr)

    let inline private toArray f = function
        | All arr ->
            all (f 0 0 (Array2D.length1 arr) (Array2D.length2 arr) arr)
        | View (i0, j0, h, w, arr) ->
            all (f i0 j0 h w arr)

    let inline private toScalar f = function
        | All arr ->
            f 0 0 (Array2D.length1 arr) (Array2D.length2 arr) arr
        | View (i0, j0, h, w, arr) ->
            f i0 j0 h w arr

    let inline length1 varr =
        match varr with
            | All arr -> Array2D.length1 arr
            | View (_, _, h, _, _) -> h

    let inline length2 varr =
        match varr with
            | All arr -> Array2D.length2 arr
            | View (_, _, _, w, _) -> w

    let inline init h w f =
        all (Array2D.init h w f)

    let inline initZeros h w =
        init h w (fun _ _ -> 0)

    let inline get varr i j =
        match varr with
            | All arr -> arr.[i, j]
            | View (i0, j0, _, _, arr) -> arr.[i0 + i, j0 + j]

    let inline set varr i j v =
        match varr with
            | All arr ->
                let arr' = Array2D.copy arr
                arr'.[i, j] <- v
                all arr'
            | View (i0, j0, h, w, arr) ->
                let arr' = Array2D.init h w (fun i j -> arr.[i0 + i, j0 + j])
                arr'.[i, j] <- v
                view i0 j0 h w arr'

    let inline write varr i j v =
        match varr with
            | All arr ->
                arr.[i, j] <- v
                all arr
            | _ ->
                set varr i j v

    let inline subArr i j h w varr =
        if i <= 0 && j <= 0 && length1 varr <= h && length2 varr <= w then
            varr
        else
            match varr with
                | All arr ->
                    let i0 = max 0 i
                    let j0 = max 0 j
                    let h0 = min (Array2D.length1 arr - i0) h
                    let w0 = min (Array2D.length2 arr - j0) w
                    view i0 j0 h0 w0 arr
                | View (i0, j0, h0, w0, arr) ->
                    view (min (i0 + (max 0 i)) i0) (min j0 (j0 + (max 0 j))) (min h0 h) (min w0 w) arr

    let inline map f = function
        | All arr -> all (Array2D.map f arr)
        | View (i0, j0, h, w, arr) -> all (Array2D.init h w (fun i j -> f arr.[i0 + i, j0 + j]))

    let inline mapi f = function
        | All arr -> all (Array2D.mapi f arr)
        | View (i0, j0, h, w, arr) -> all (Array2D.init h w (fun i j -> f i j arr.[i0 + i, j0 + j]))

    let inline copy rope = map id rope

    let inline rev1 varr = toArray Array2D.revBased1 varr
    let inline rev2 varr = toArray Array2D.revBased2 varr

    let inline cat1 upper lower =
        if length2 upper <> length2 lower then failwith "length2 must be equal!"
        let l1 = length1 upper + length1 lower
        let l2 = length2 lower
        let l1u = length1 upper
        all (Array2D.init l1 l2 (fun i j -> if i < l1u then get upper i j else get lower (i - l1u) j))

    let inline cat2 left right =
        if length1 left <> length1 right then failwith "length1 must be equal!"
        let l1 = length1 left
        let l2 = length2 left + length2 right
        let l2l = length2 left
        all (Array2D.init l1 l2 (fun i j -> if j < l2l then get left i j else get right i (j - l2l)))

    let inline fold1 f state varr = toArray (Array2D.foldBased1 f state) varr
    let inline fold2 f state varr = toArray (Array2D.foldBased2 f state) varr

    let inline mapreduce1 f g varr = toArray (Array2D.mapReduceBased1 f g) varr
    let inline mapreduce2 f g varr = toArray (Array2D.mapReduceBased2 f g) varr

    let inline reduce1 f arr = mapreduce1 id f arr
    let inline reduce2 f arr = mapreduce2 id f arr

    let inline mapreduce f g varr = toScalar (Array2D.mapReduceBased f g) varr
    let inline reduce f varr = toScalar (Array2D.reduceBased f) varr

    let inline scan1 f state varr = toArray (Array2D.scanBased1 f state) varr
    let inline scan2 f state varr = toArray (Array2D.scanBased2 f state) varr

    let inline filter1 p varr =
        let vs = Seq.filter p (Seq.init (length1 varr) (fun i -> get varr i 0)) |> Array.ofSeq
        init (Array.length vs) 1 (fun i _ -> Array.get vs i)

    let inline filter2 p varr =
        let vs = Seq.filter p (Seq.init (length2 varr) (get varr 0)) |> Array.ofSeq
        init 1 (Array.length vs) (fun _ j -> Array.get vs j)

    let inline sort1 p varr = toArray (Array2D.sortBased1 p) varr
    let inline sort2 p varr = toArray (Array2D.sortBased2 p) varr

    let inline transpose varr =
        all (Array2D.init (length2 varr) (length1 varr) (fun j i -> get varr i j))
