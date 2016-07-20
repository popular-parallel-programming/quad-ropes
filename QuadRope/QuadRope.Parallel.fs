[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module RadTrees.Parallel.QuadRope

open RadTrees
open Types
open Utils.Tasks

/// Generate a new tree in parallel.
let init h w f =
    let rec init h0 w0 h1 w1 =
        let h = h1 - h0
        let w = w1 - w0
        if h <= QuadRope.h_max && w <= QuadRope.w_max then
            QuadRope.leaf (Array2D.init h w (fun i j -> f (h0 + i) (w0 + j)))
        else if w <= QuadRope.w_max then
            let hpv = h0 + h / 2
            let n, s = par2 (fun () -> init h0 w0 hpv w1) (fun () -> init hpv w0 h1 w1)
            QuadRope.thinNode n s
        else if h <= QuadRope.h_max then
            let wpv = w0 + w / 2
            let w, e = par2 (fun () -> init h0 w0 h1 wpv) (fun () -> init h0 wpv h1 w1)
            QuadRope.flatNode w e
        else
            let hpv = h0 + h / 2
            let wpv = w0 + w / 2
            let ne, nw, sw, se = par4 (fun () -> init h0 wpv hpv w1)
                                      (fun () -> init h0 w0 hpv wpv)
                                      (fun () -> init hpv w0 h1 wpv)
                                      (fun () -> init hpv wpv h1 w1)
            QuadRope.node ne nw sw se
    init 0 0 h w

/// Reallocate a rope form the ground up in parallel. Sometimes,
/// this is the only way to improve performance of a badly
/// composed quad rope.
let reallocate rope =
    init (QuadRope.rows rope) (QuadRope.cols rope) (QuadRope.get rope)

/// Initialize a rope in parallel where all elements are
/// <code>e</code>.
let initAll h w e =
    init h w (fun _ _ -> e)

/// Initialize a rope in parallel with all zeros.
let initZeros h w = initAll h w 0

/// Apply a function f to all scalars in parallel.
let rec map f = function
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> map f ne) (fun () -> map f nw)
        QuadRope.flatNode nw0 ne0
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> map f nw) (fun () -> map f sw)
        QuadRope.thinNode nw0 sw0
    | Node (_, _, _, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun () -> map f ne)
                                      (fun () -> map f nw)
                                      (fun () -> map f sw)
                                      (fun () -> map f se)
        QuadRope.node ne0 nw0 sw0 se0
    | Slice _ as rope -> map f (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.map f rope

/// Fold each row of rope with f in parallel, starting with the
/// according state in states.
let hfold f states rope =
    if QuadRope.rows states <> QuadRope.rows rope then
        failwith "states and rope must have same height"
    let rec fold1 states = function
        | Empty -> states
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw sw) ne se
        | Slice _ as rope -> fold1 states (QuadRope.Slicing.reallocate rope)
        | rope -> QuadRope.hfold f states rope
    and fold2 states n s =
        let nstates, sstates = QuadRope.vsplit2 states (QuadRope.rows n)
        let nstates0, sstates0 = par2 (fun () -> fold1 nstates n) (fun () -> fold1 sstates s)
        QuadRope.thinNode nstates0 sstates0
    fold1 states rope

/// Fold each column of rope with f in parallel, starting with the
/// according state in states.
let vfold f states rope =
    if QuadRope.cols states <> QuadRope.cols rope then
        failwith "states and rope must have same width"
    let rec fold1 states = function
        | Empty -> states
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw ne) sw se
        | Slice _ as rope -> fold1 states (QuadRope.Slicing.reallocate rope)
        | rope -> QuadRope.vfold f states rope
    and fold2 states w e =
        let wstates, estates = QuadRope.hsplit2 states (QuadRope.cols w)
        let wstates0, estates0 = par2 (fun () -> fold1 wstates w) (fun () -> fold1 estates e)
        QuadRope.flatNode wstates0 estates0
    fold1 states rope

/// Zip implementation for the general case where we do not assume
/// that both ropes have the same internal structure.
let rec private genZip f lope rope =
    match lope with
        | Node (d, h, w, ne, nw, Empty, Empty) ->
            let ne0, nw0 =
                par2 (fun () -> genZip f ne (QuadRope.slice rope 0
                                                                 (QuadRope.cols nw)
                                                                 (QuadRope.rows ne)
                                                                 (QuadRope.cols ne)))
                     (fun () -> genZip f nw (QuadRope.slice rope 0
                                                                 0
                                                                 (QuadRope.rows nw)
                                                                 (QuadRope.cols nw)))
            Node (d, h, w, ne0, nw0, Empty, Empty)
        | Node (d, h, w, Empty, nw, sw, Empty) ->
            let nw0, sw0 =
                par2 (fun () -> genZip f nw (QuadRope.slice rope 0
                                                                 0
                                                                 (QuadRope.rows nw)
                                                                 (QuadRope.cols nw)))
                     (fun () -> genZip f sw (QuadRope.slice rope (QuadRope.rows nw)
                                                                 0
                                                                 (QuadRope.rows sw)
                                                                 (QuadRope.cols sw)))
            Node (d, h, w, Empty, nw0, sw0, Empty)
        | Node (d, h, w, ne, nw, sw, se) ->
            let ne0, nw0, sw0, se0 =
                par4 (fun () -> genZip f ne (QuadRope.slice rope 0
                                                                 (QuadRope.cols nw)
                                                                 (QuadRope.rows ne)
                                                                 (QuadRope.cols ne)))
                     (fun () -> genZip f nw (QuadRope.slice rope 0
                                                                 0
                                                                 (QuadRope.rows nw)
                                                                 (QuadRope.cols nw)))
                     (fun () -> genZip f sw (QuadRope.slice rope (QuadRope.rows nw)
                                                                 0
                                                                 (QuadRope.rows sw)
                                                                 (QuadRope.cols sw)))
                     (fun () -> genZip f se (QuadRope.slice rope (QuadRope.rows ne)
                                                                 (QuadRope.cols sw)
                                                                 (QuadRope.rows se)
                                                                 (QuadRope.cols se)))
            Node (d, h, w, ne0, nw0, sw0, se0)
        | Slice _ -> genZip f (QuadRope.Slicing.reallocate lope) rope
        | _ -> QuadRope.zip f lope rope

/// Zip function that assumes that the internal structure of two ropes
/// matches. If not, it falls back to the slow, general case.
let rec private fastZip f lope rope =
    match lope, rope with
        | Empty, Empty -> Empty
        | Leaf ls, Leaf rs when QuadRope.shapesMatch lope rope -> QuadRope.leaf (Array2D.map2 f ls rs)
        | Node (d, h, w, lne, lnw, Empty, Empty), Node (_, _, _, rne, rnw, Empty, Empty)
            when QuadRope.subShapesMatch lope rope ->
                let ne, nw = par2 (fun () -> fastZip f lne rne) (fun () -> fastZip f lnw rnw)
                Node (d, h, w, ne, nw, Empty, Empty)
        | Node (d, h, w, Empty, lnw, lsw, Empty), Node (_, _, _, Empty, rnw, rsw, Empty)
            when QuadRope.subShapesMatch lope rope ->
                let nw, sw = par2 (fun () -> fastZip f lnw rnw) (fun () -> fastZip f lsw rsw)
                Node (d, h, w, Empty, nw, sw, Empty)
        | Node (d, h, w, lne, lnw, lsw, lse), Node (_, _, _, rne, rnw, rsw, rse)
            when QuadRope.subShapesMatch lope rope ->
                let ne, nw, sw, se = par4 (fun () -> fastZip f lne rne)
                                          (fun () -> fastZip f lnw rnw)
                                          (fun () -> fastZip f lsw rsw)
                                          (fun () -> fastZip f lse rse)
                Node (d, h, w, ne, nw, sw, se)
        | Slice _, _ -> fastZip f (QuadRope.Slicing.reallocate lope) rope
        | _ -> genZip f lope rope

/// Apply f to each (i, j) of lope and rope.
let zip f lope rope =
    if QuadRope.cols lope <> QuadRope.cols rope || QuadRope.rows lope <> QuadRope.rows rope then
        failwith "ropes must have the same shape"
    fastZip f lope rope

/// Apply f to all scalars in parallel and reduce the results
/// row-wise using g.
let rec hmapreduce f g = function
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> hmapreduce f g ne) (fun () -> hmapreduce f g nw)
        match ne0 with
            | Empty -> nw0
            | _ -> zip g nw0 ne0
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> hmapreduce f g nw) (fun () -> hmapreduce f g sw)
        QuadRope.thinNode nw0 sw0
    | Node (_, _, _, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun () -> hmapreduce f g ne)
                                      (fun () -> hmapreduce f g nw)
                                      (fun () -> hmapreduce f g sw)
                                      (fun () -> hmapreduce f g se)
        let w = QuadRope.thinNode nw0 sw0
        let e = QuadRope.thinNode ne0 se0
        zip g w e
    | Slice _ as rope -> hmapreduce f g (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.hmapreduce f g rope

/// Apply f to all scalars in parallel and reduce the results
/// column-wise using g.
let rec vmapreduce f g = function
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> vmapreduce f g ne) (fun () -> vmapreduce f g nw)
        QuadRope.flatNode nw0 ne0
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> vmapreduce f g nw) (fun () -> vmapreduce f g sw)
        match sw0 with
            | Empty -> nw0
            | _ -> zip g nw0 sw0
    | Node (_, _, _, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun () -> vmapreduce f g ne)
                                      (fun () -> vmapreduce f g nw)
                                      (fun () -> vmapreduce f g sw)
                                      (fun () -> vmapreduce f g se)
        let n = QuadRope.flatNode nw0 ne0
        let s = QuadRope.flatNode sw0 se0
        zip g n s
    | Slice _ as rope -> vmapreduce f g (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.vmapreduce f g rope

/// Reduce all rows of rope by f.
let hreduce f rope = hmapreduce id f rope

/// Reduce all columns of rope by f.
let vreduce f rope = vmapreduce id f rope

/// Apply f to all values of the rope and reduce the resulting
/// values to a single scalar using g in parallel.
let rec mapreduce f g = function
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        let ne', nw' = par2 (fun () -> mapreduce f g ne) (fun () -> mapreduce f g nw)
        g ne' nw'
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        let nw', sw' = par2 (fun () -> mapreduce f g nw) (fun () -> mapreduce f g sw)
        g nw' sw'
    | Node (_, _, _, ne, nw, sw, se) ->
        let ne', nw', sw', se' = par4 (fun () -> mapreduce f g ne)
                                      (fun () -> mapreduce f g nw)
                                      (fun () -> mapreduce f g sw)
                                      (fun () -> mapreduce f g se)
        g (g ne' nw') (g sw' se')
    | Slice _ as rope -> mapreduce f g (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.mapreduce f g rope

/// Reduce all values of the rope to a single scalar in parallel.
let reduce f rope = mapreduce id f rope

/// Remove all elements from rope for which p does not hold in
/// parallel. Input rope must be of height 1.
let rec hfilter p = function
    | Node (_, 1, _, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> hfilter p ne) (fun () -> hfilter p nw)
        QuadRope.flatNode nw0 ne0
    | Slice _ as rope -> hfilter p (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.hfilter p rope

/// Remove all elements from rope for which p does not hold in
/// parallel. Input rope must be of width 1.
let rec vfilter p = function
    | Node (_, _, 1, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> vfilter p nw) (fun () -> vfilter p sw)
        QuadRope.thinNode nw0 sw0
    | Slice _ as rope -> vfilter p (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.vfilter p rope

/// Reverse the quad rope horizontally in parallel.
let rec hrev = function
    | Node (d, h, w, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> hrev nw) (fun () -> hrev sw)
        Node (d, h, w, Empty, nw0, sw0, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun() -> hrev ne)
                                      (fun() -> hrev nw)
                                      (fun() -> hrev sw)
                                      (fun() -> hrev se)
        Node (d, h, w, nw0, ne0, se0, sw0)
    | Slice _ as rope -> hrev (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.hrev rope

/// Reverse the quad rope vertically in parallel.
let rec vrev = function
    | Node (d, h, w, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> vrev ne) (fun () -> vrev nw)
        Node (d, h, w, ne0, nw0, Empty, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun() -> vrev ne)
                                      (fun() -> vrev nw)
                                      (fun() -> vrev sw)
                                      (fun() -> vrev se)
        Node (d, h, w, se0, sw0, nw0, ne0)
    | Slice _ as rope -> vrev (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.vrev rope


/// Transpose the quad rope in parallel. This is equal to swapping
/// indices, such that get rope i j = get rope j i.
let rec transpose = function
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        let nw0, ne0 = par2 (fun () -> transpose nw) (fun () -> transpose ne)
        QuadRope.thinNode nw0 ne0
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> transpose nw) (fun () -> transpose sw)
        QuadRope.flatNode nw0 sw0
    | Node (_, _, _, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun () -> transpose ne)
                                      (fun () -> transpose nw)
                                      (fun () -> transpose sw)
                                      (fun () -> transpose se)
        QuadRope.node sw0 nw0 ne0 se0
    | Slice _ as rope -> transpose (QuadRope.Slicing.reallocate rope)
    | rope -> QuadRope.transpose rope
