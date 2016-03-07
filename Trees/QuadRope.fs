namespace RadTrees

type 'a QuadRope =
    | Empty
    | Leaf of 'a [,]
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope

module QuadRope =

    let maxSize = 4

    let rows = function
        | Empty -> 0
        | Leaf vs -> Array2D.length1 vs
        | Node (_, h, _, _, _, _, _) -> h

    let cols = function
        | Empty -> 0
        | Leaf vs -> Array2D.length2 vs
        | Node (_, _, w, _, _, _, _) -> w

    let depth = function
        | Empty -> 0
        | Leaf _ -> 1
        | Node (d, _, _, _, _, _, _) -> d

    (* Procudes a "flat" node. *)
    let vnode ne nw =
        let h = rows nw
        let w = cols nw + cols ne
        let d = max (depth nw) (depth ne)
        Node (d + 1, h, w, ne, nw, Empty, Empty)

    (* Produces a "thin" node. *)
    let hnode nw sw =
        let h = rows nw + rows sw
        let w = cols nw
        let d = max (depth nw) (depth sw)
        Node (d + 1, h, w, Empty, nw, sw, Empty)

    let makeNode ne nw sw se =
        let d = max (max (depth ne) (depth nw)) (max (depth sw) (depth se))
        let h = rows ne + rows sw
        let w = cols nw + cols se
        Node (d + 1, h, w, ne, nw, sw, se)

    let init h w f =
        let rec init0 h0 w0 h1 w1 f =
            let h = h1 - h0
            let w = w1 - w0
            let hpiv = h0 + h / 2
            let wpiv = w0 + w / 2
            if maxSize < h && maxSize < w then
                let ne = init0 h0 wpiv hpiv w1 f
                let nw = init0 h0 w0 hpiv wpiv f
                let sw = init0 hpiv w0 h1 wpiv f
                let se = init0 hpiv wpiv h1 w1 f
                makeNode ne nw sw se
            else if maxSize < h then
                let nw = init0 h0 w0 hpiv w1 f
                let sw = init0 hpiv w0 h1 w1 f
                hnode nw sw
            else if maxSize < w then
                let ne = init0 h0 wpiv h1 w1 f
                let nw = init0 h0 w0 h1 wpiv f
                vnode ne nw
            else
                Leaf (Array2D.init h w (fun i j -> f (i + h0) (j + w0)))
        init0 0 0 h w f

    let fromArray vss =
        init (Array2D.length1 vss) (Array2D.length2 vss) (Array2D.get vss)

    let inline canCopyV us ls =
        Array2D.length1 us + Array2D.length1 ls <= maxSize

    let vcat upper lower =
        if cols upper <> cols lower then failwith "Trees must be of same width!"
        match upper, lower with
            | Leaf us, Leaf ls when canCopyV us ls ->
                Leaf (RadTrees.Array2D.cat1 us ls) (* Copying small arrays is ok. *)

            | (Node (ud, uh, uw, Leaf unes, Leaf unws, Empty, Empty),
               Node (ld, lh, lw, Leaf lnes, Leaf lnws, Empty, Empty)) when canCopyV unes lnes
                                                                        && canCopyV unws lnws ->
                Node (2, uh + lh, uw, Leaf (Array2D.cat1 unes lnes), Leaf (Array2D.cat1 unws lnws),
                      Empty, Empty) (* Unwrap and copy small arrays. *)

            | (Node (ud, uh, uw, une, unw, Empty, Empty),
               Node (ld, lh, lw, lne, lnw, Empty, Empty)) ->
                Node (max ud ld, uh + lh, uw, une, unw, lnw, lne) (* Concatenation of two "flat" nodes. *)

            | _, _ -> vnode upper lower (* Make a new thin node. *)

    let inline canCopyH us ls =
        Array2D.length2 us + Array2D.length2 ls <= maxSize

    let hcat left right =
        if rows left <> rows right then failwith "Trees must be of same width!"
        match left, right with
            | Leaf ls, Leaf rs when canCopyV ls rs ->
                Leaf (RadTrees.Array2D.cat2 ls rs) (* Copying small arrays is ok. *)

            | (Node (ld, lh, lw, Empty, Leaf lnws, Leaf lsws, Empty),
               Node (rd, rh, rw, Empty, Leaf rnws, Leaf rsws, Empty)) when canCopyH lnws rnws
                                                                        && canCopyH lsws rsws ->
                Node (2, rh, lw + rw, Leaf (Array2D.cat2 lnws rnws), Leaf (Array2D.cat2 lsws rsws),
                      Empty, Empty) (* Unwrap and copy small arrays. *)

            | (Node (ld, lh, lw, lne, lnw, Empty, Empty),
               Node (rd, rh, rw, rne, rnw, Empty, Empty)) ->
                Node (max ld rd, lh, lw + rw, lne, lnw, rnw, rne) (* Concatenation of two "flat" nodes. *)

            | _, _ -> hnode left right (* Make a new thin node. *)
