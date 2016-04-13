namespace RadTrees

type 'a ViewArray2D =
    | Array of 'a [,]
    | View of int * int * int * int * 'a [,]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ViewArray2D =

    let inline private array arr =
        Array arr

    let inline private view i j h w arr =
        View (i, j, h, w, arr)

    let inline private call f = function
        | Array arr ->
            array (f 0 0 (Array2D.length1 arr) (Array2D.length2 arr) arr)
        | View (i0, j0, h, w, arr) ->
            array (f i0 j0 h w arr)

    let inline length1 varr =
        match varr with
            | Array arr -> Array2D.length1 arr
            | View (_, _, h, _, _) -> h

    let inline length2 varr =
        match varr with
            | Array arr -> Array2D.length2 arr
            | View (_, _, _, w, _) -> w

    let inline init h w f =
        array (Array2D.init h w f)

    let inline initZeros h w =
        init h w (fun _ _ -> 0)

    let inline get varr i j =
        match varr with
            | Array arr -> arr.[i, j]
            | View (i0, j0, _, _, arr) -> arr.[i0 + i, j0 + j]

    let inline set varr i j v =
        match varr with
            | Array arr ->
                let arr' = Array2D.copy arr
                arr'.[i, j] <- v
                array arr'
            | View (i0, j0, h, w, arr) ->
                let arr' = Array2D.init h w (fun i j -> arr.[i0 + i, j0 + j])
                arr'.[i, j] <- v
                view i0 j0 h w arr'

    let inline write varr i j v =
        match varr with
            | Array arr ->
                arr.[i, j] <- v
                array arr
            | _ ->
                set varr i j v

    let inline subArr i j h w varr =
        if i <= 0 && j <= 0 && length1 varr <= h && length2 varr <= w then
            varr
        else
            match varr with
            | Array arr ->
                    view (max 0 i)
                         (max 0 j)
                         (min (Array2D.length1 arr - i) h)
                         (min (Array2D.length2 arr - j) w) arr
                | View (i0, j0, h0, w0, arr) ->
                    view (min (i0 + (max 0 i)) i0) (min j0 (j0 + (max 0 j))) (min h0 h) (min w0 w) arr

    let inline map f = function
        | Array arr -> array (Array2D.map f arr)
        | View (i0, j0, h, w, arr) -> array (Array2D.init h w (fun i j -> f arr.[i0 + i, j0 + j]))

    let inline copy rope = map id rope

    let inline rev1 varr = call Array2D.revBased1 varr
    let inline rev2 varr = call Array2D.revBased2 varr

    let inline cat1 upper lower =
        if length2 upper <> length2 lower then failwith "length2 must be equal!"
        let l1 = length1 upper + length1 lower
        let l2 = length2 lower
        let l1u = length1 upper
        array (Array2D.init l1 l2 (fun i j -> if i < l1u then get upper i j else get lower (i - l1u) j))

    let inline cat2 left right =
        if length1 left <> length1 right then failwith "length1 must be equal!"
        let l1 = length1 left
        let l2 = length2 left + length2 right
        let l2l = length2 left
        array (Array2D.init l1 l2 (fun i j -> if j < l2l then get left i j else get right i (j - l2l)))

    let inline fold1 f state varr = call (Array2D.foldBased1 f state) varr
    let inline fold2 f state varr = call (Array2D.foldBased2 f state) varr

    let inline mapreduce1 f g varr = call (Array2D.mapReduceBased1 f g) varr
    let inline mapreduce2 f g varr = call (Array2D.mapReduceBased2 f g) varr

    let inline reduce1 f arr = mapreduce1 id f arr
    let inline reduce2 f arr = mapreduce2 id f arr

    let inline scan1 f state varr = call (Array2D.scanBased1 f state) varr
    let inline scan2 f state varr = call (Array2D.scanBased2 f state) varr

    let inline filter1 p varr =
        let vs = Seq.filter p (Seq.init (length1 varr) (fun i -> get varr i 0)) |> Array.ofSeq
        init (Array.length vs) 1 (fun i _ -> Array.get vs i)

    let inline filter2 p varr =
        let vs = Seq.filter p (Seq.init (length2 varr) (get varr 0)) |> Array.ofSeq
        init 1 (Array.length vs) (fun _ j -> Array.get vs j)

    let inline sort1 p varr = call (Array2D.sortBased1 p) varr
    let inline sort2 p varr = call (Array2D.sortBased2 p) varr
