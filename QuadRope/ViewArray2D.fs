namespace RadTrees

type 'a ViewArray2D =
    | Array of 'a [,]
    | View of int * int * int * int * 'a [,]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ViewArray2D =

    let array arr =
        Array arr

    let view i j h w arr =
        View (i, j, h, w, arr)

    let length1 = function
        | Array arr -> Array2D.length1 arr
        | View (_, _, h, _, _) -> h

    let length2 = function
        | Array arr -> Array2D.length2 arr
        | View (_, _, _, w, _) -> w

    let inline init h w f =
        array (Array2D.init h w f)

    let inline initZeros h w =
        init h w (fun _ _ -> 0)

    let get lar i j =
        match lar with
            | Array arr -> arr.[i, j]
            | View (i0, j0, _, _, arr) -> arr.[i0 + i, j0 + j]

    let set lar i j v =
        match lar with
            | Array arr ->
                let arr' = Array2D.copy arr
                arr'.[i, j] <- v
                array arr'
            | View (i0, j0, h, w, arr) ->
                let arr' = Array2D.init h w (fun i j -> arr.[i0 + i, j0 + j])
                arr'.[i, j] <- v
                view i0 j0 h w arr'

    let write lar i j v =
        match lar with
            | Array arr ->
                arr.[i, j] <- v
                array arr
            | _ ->
                set lar i j v

    let subArr i j h w = function
        | Array arr -> view i j h w arr
        | View (i0, j0, _, _, arr) -> view (i0 + i) (j0 + j) h w arr

    let map f = function
        | Array arr -> array (Array2D.map f arr)
        | View (i0, j0, h, w, arr) -> array (Array2D.init h w (fun i j -> f arr.[i0 + i, j0 + j]))

    let rev1 = function
        | Array arr -> Array (Array2D.rev1 arr)
        | View (i0, j0, h, w, arr) -> Array (Array2D.init h w (fun i j -> arr.[(h - 1) - i0 + i, j0 + j]))

    let rev2 = function
        | Array arr -> Array (Array2D.rev2 arr)
        | View (i0, j0, h, w, arr) -> Array (Array2D.init h w (fun i j -> arr.[i0 + i, (w - 1) - j0 + j]))

    let cat1 upper lower =
        if length2 upper <> length2 lower then failwith "length2 must be equal!"
        let l1 = length1 upper + length1 lower
        let l2 = length2 lower
        let l1u = length1 upper
        Array (Array2D.init l1 l2 (fun i j -> if i < l1u then get upper i j else get lower (i - l1u) j))

    let cat2 left right =
        if length1 left <> length1 right then failwith "length1 must be equal!"
        let l1 = length1 left
        let l2 = length2 left + length2 right
        let l2l = length2 left
        Array (Array2D.init l1 l2 (fun i j -> if j < l2l then get left i j else get right i (j - l2l)))

    let fold1 f state = function
        | Array arr -> Array (Array2D.fold1 f state arr)
        | View (i0, j0, h, w, arr) ->
            let fold _ j = Seq.fold f (state j) (seq { for i in i0 .. i0 + h - 1 -> arr.[i, j0 + j] })
            Array (Array2D.init 1 w fold)

    let fold2 f state = function
        | Array arr -> Array (Array2D.fold2 f state arr)
        | View (i0, j0, h, w, arr) ->
            let fold i _ = Seq.fold f (state i) (seq { for j in j0 .. j0 + w - 1 -> arr.[i0 + i, j] })
            Array (Array2D.init h 1 fold)

    let reduce1 f = function
        | Array arr -> Array (Array2D.reduce1 f arr)
        | View (i0, j0, h, w, arr) ->
            let reduce _ j = Seq.reduce f (seq { for i in i0 .. i0 + h - 1 -> arr.[i, j0 + j] })
            Array (Array2D.init 1 w reduce)

    let reduce2 f = function
        | Array arr -> Array (Array2D.reduce2 f arr)
        | View (i0, j0, h, w, arr) ->
            let reduce i _ = Seq.reduce f (seq { for j in j0 .. j0 + w - 1 -> arr.[i0 + i, j] })
            Array (Array2D.init h 1 reduce)
