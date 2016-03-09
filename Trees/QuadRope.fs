namespace RadTrees

type 'a QuadRope =
    | Empty
    | Leaf of 'a [,]
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope

module QuadRope =

    (* The maximal size of a leaf array in any direction. *)
    let maxSize = 4
    let maxDepth = 4

    (* Initialize Fibonacci numbers at module load time. *)
    ignore (Fibonacci.fib maxDepth)

    (* Number of rows in a rectangular tree. *)
    let rows = function
        | Empty -> 0
        | Leaf vs -> Array2D.length1 vs
        | Node (_, h, _, _, _, _, _) -> h

    (* Number of columns in a rectangular tree. *)
    let cols = function
        | Empty -> 0
        | Leaf vs -> Array2D.length2 vs
        | Node (_, _, w, _, _, _, _) -> w

    (* Depth of a rectangular tree. *)
    let depth = function
        | Empty -> 0
        | Leaf _ -> 1
        | Node (d, _, _, _, _, _, _) -> d

    (* Produces a "thin" node. *)
    let vnode nw sw =
        match nw, sw with
            | Empty, Empty -> Empty
            | Empty, _ -> sw
            | _, Empty -> nw
            | _ ->
                let h = rows nw + rows sw
                let w = cols nw
                let d = max (depth nw) (depth sw)
                Node (d + 1, h, w, Empty, nw, sw, Empty)

    (* Produces a "flat" node. *)
    let hnode nw ne =
        match nw, ne with
            | Empty, Empty -> Empty
            | Empty, _ -> ne
            | _, Empty -> nw
            | _ ->
                let h = rows nw
                let w = cols nw + cols ne
                let d = max (depth nw) (depth ne)
                Node (d + 1, h, w, ne, nw, Empty, Empty)

    let makeLeaf vs =
        if Array2D.length1 vs = 0 || Array2D.length2 vs = 0 then
            Empty
        else
            Leaf vs

    let inline private withinRange root i j =
        i < rows root && j < cols root

    (* Get the value of a location in the tree. *)
    let rec get root i j =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Array2D.get vs i j
            | Node (_, h, w, ne, nw, sw, se) ->
                if withinRange nw i j then
                    get nw i j
                else
                    let j0 = j - (cols nw)
                    if withinRange ne i j0 then
                        get ne i j0
                    else
                        let i0 = i - (rows nw)
                        if withinRange sw i0 j then
                            get sw i0 j
                        else
                            get se i0 j0 (* Either contains or ends in out-of-bounds. *)

    (* Update a tree location wihtout modifying the original tree. *)
    let rec set root i j v =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Leaf (RadTrees.Array2D.set vs i j v)
            | Node (d, h, w, ne, nw, sw, se) ->
                if withinRange nw i j then
                    Node (d, h, w, ne, set nw i j v, sw, se)
                else
                    let j0 = j - (cols nw)
                    if withinRange ne i j0 then
                        Node (d, h, w, set ne i j0 v, nw, sw, se)
                    else
                        let i0 = i - (rows nw)
                        if withinRange sw i0 j then
                            Node (d, h, w, ne, nw, set sw i0 j v, se)
                        else
                            Node (d, h, w, ne, nw, sw, set se i0 j0 v)

    (* Write to a tree location destructively. *)
    let rec write root i j v =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> vs.[i, j] <- v
            | Node (_, h, w, ne, nw, sw, se) ->
                if withinRange nw i j then
                    write nw i j v
                else
                    let j0 = j - (cols nw)
                    if withinRange ne i j0 then
                        write ne i j0 v
                    else
                        let i0 = i - (rows nw)
                        if withinRange sw i0 j then
                            write sw i0 j v
                        else
                            write se i0 j0 v

    let inline private canCopyV us ls =
        Array2D.length1 us + Array2D.length1 ls <= maxSize

    (* Concatenate two trees vertically. *)
    let vcat upper lower =
        if cols upper <> cols lower then failwith "Trees must be of same width!"
        match upper, lower with
            | Leaf us, Leaf ls when canCopyV us ls ->
                Leaf (Array2D.cat1 us ls) (* Copying small arrays is ok. *)

            | (Node (ud, uh, uw, Leaf unes, Leaf unws, Empty, Empty),
               Node (ld, lh, lw, Leaf lnes, Leaf lnws, Empty, Empty)) when canCopyV unes lnes
                                                                        && canCopyV unws lnws ->
                hnode (Leaf (Array2D.cat1 unws lnws)) (Leaf (Array2D.cat1 unes lnes))

            | (Node (ud, uh, uw, une, unw, Empty, Empty),
               Node (ld, lh, lw, lne, lnw, Empty, Empty)) ->
                Node (max ud ld, uh + lh, uw, une, unw, lnw, lne) (* Concatenation of two "flat" nodes. *)

            | _, _ -> vnode upper lower (* Make a new thin node. *)

    let inline private canCopyH us ls =
        Array2D.length2 us + Array2D.length2 ls <= maxSize

    (* Concatenate two trees horizontally. *)
    let hcat left right =
        if rows left <> rows right then failwith "Trees must be of same width!"
        match left, right with
            | Leaf ls, Leaf rs when canCopyH ls rs ->
                Leaf (RadTrees.Array2D.cat2 ls rs) (* Copying small arrays is ok. *)

            | (Node (ld, lh, lw, Empty, Leaf lnws, Leaf lsws, Empty),
               Node (rd, rh, rw, Empty, Leaf rnws, Leaf rsws, Empty)) when canCopyH lnws rnws
                                                                        && canCopyH lsws rsws ->
                vnode (Leaf (Array2D.cat2 lnws rnws)) (Leaf (Array2D.cat2 lsws rsws))

            | (Node (ld, lh, lw, Empty, lnw, lsw, Empty),
               Node (rd, rh, rw, Empty, rnw, rsw, Empty)) ->
                Node (max ld rd, lh, lw + rw, rnw, lnw, lsw, rsw) (* Concatenation of two "thin" nodes. *)

            | _, _ -> hnode left right (* Make a new thin node. *)

    let makeNode ne nw sw se =
        vnode (hnode nw ne) (hnode sw se)

    (* Compute the "subrope" starting from indexes i, j taking h and w
       elements in vertical and horizontal direction. *)
    let rec split root i j h w =
        if i <= 0 && rows root <= h && j <= 0 && cols root <= w then
            root
        else
            match root with
                | Empty -> Empty
                | Leaf vs ->
                    makeLeaf (Array2D.subArr vs (max 0 i)
                                                (max 0 j)
                                                (min h (Array2D.length1 vs))
                                                (min w (Array2D.length2 vs)))
                | Node (_, _, _, ne, nw, sw, se) ->
                    let nnw = split nw  i             j             h              w
                    let nne = split ne  i            (j - cols nw)  h             (w - cols nnw)
                    let nsw = split sw (i - rows nw)  j            (h - rows nnw)  w
                    let nse = split se (i - rows ne) (j - cols sw) (h - rows nne) (w - cols nsw)
                    match nne, nsw, nse with
                        | Empty, _, Empty -> vnode nnw nsw
                        | _, Empty, Empty -> hnode nnw nne
                        | _               -> makeNode nne nnw nsw nse

    let rec hrev = function
        | Empty -> Empty
        | Leaf vs -> Leaf (Array2D.rev2 vs)
        | Node (d, h, w, ne, nw, sw, se) ->
            Node (d, h, w, hrev nw, hrev ne, hrev se, hrev sw)

    let rec vrev = function
        | Empty -> Empty
        | Leaf vs -> Leaf (Array2D.rev1 vs)
        | Node (d, h, w, ne, nw, sw, se) ->
            Node (d, h, w, vrev se, vrev sw, vrev nw, vrev ne)

    (* Generate a new tree without any intermediate values. *)
    let init h w f =
        let rec init0 h0 w0 h1 w1 f =
            let h = h1 - h0
            let w = w1 - w0
            let hpiv = h0 + h / 2
            let wpiv = w0 + w / 2
            if maxSize < h && maxSize < w then
                let nw = init0 h0 w0 hpiv wpiv f
                let ne = init0 h0 wpiv hpiv w1 f
                let sw = init0 hpiv w0 h1 wpiv f
                let se = init0 hpiv wpiv h1 w1 f
                makeNode ne nw sw se
            else if maxSize < h then
                let nw = init0 h0 w0 hpiv w1 f
                let sw = init0 hpiv w0 h1 w1 f
                vcat nw sw
            else if maxSize < w then
                let nw = init0 h0 w0 h1 wpiv f
                let ne = init0 h0 wpiv h1 w1 f
                hcat nw ne
            else
                Leaf (Array2D.init h w (fun i j -> f (i + h0) (j + w0)))
        init0 0 0 h w f

    let fromArray vss =
        init (Array2D.length1 vss) (Array2D.length2 vss) (Array2D.get vss)

    (* Iterate over a tree from the upper left to the lower right in
       row-first order. *)
    let rec toSeq = function
        | Empty -> Seq.empty
        | Leaf vs -> seq { for i in 0..Array2D.length1 vs do
                           for j in 0..Array2D.length2 vs ->
                           vs.[i, j] }
        | Node (_, _, _, ne, nw, sw, se) ->
            seq { yield! toSeq nw; yield! toSeq ne; yield! toSeq sw; yield! toSeq se }

    (* Bulk-operations on quad ropes: *)

    (* Apply a function to every element in the tree and preserves the
       tree structure. *)
    let rec map f root =
        match root with
            | Empty -> Empty
            | Leaf vs -> Leaf (Array2D.map f vs)
            | Node (d, h, w, ne, nw, sw, se) ->
                Node (d, h, w,
                      map f ne,
                      map f nw,
                      map f sw,
                      map f se)

    (* Fold from left to right and top-down in row-first order. *)
    let foldl f state root =
        Seq.fold f state (toSeq root)

    (* Fold from right to left and bottom-up in row-first order. *)
    let foldr f root state =
        Seq.foldBack f (toSeq root) state
