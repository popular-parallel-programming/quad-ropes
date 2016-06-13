[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module RadTrees.Parallel.QuadRope

open RadTrees
open Types
open Utils.Tasks

/// Generate a new tree in parallel.
let init h w f =
    let rec init0 h0 w0 h1 w1 =
        let h = h1 - h0
        let w = w1 - w0
        if h <= QuadRope.h_max && w <= QuadRope.w_max then
            QuadRope.init h w (fun i j -> f (h0 + i) (w0 + j))
        else if w <= QuadRope.w_max then
            let hpv = h0 + h / 2
            let n, s = par2 (fun () -> init0 h0 w0 hpv w1) (fun () -> init0 hpv w0 h1 w1)
            QuadRope.thinNode n s
        else if h <= QuadRope.h_max then
            let wpv = w0 + w / 2
            let w, e = par2 (fun () -> init0 h0 w0 h1 wpv) (fun () -> init0 h0 wpv h1 w1)
            QuadRope.flatNode w e
        else
            let hpv = h0 + h / 2
            let wpv = w0 + w / 2
            let ne, nw, sw, se = par4 (fun () -> init0 h0 wpv hpv w1)
                                      (fun () -> init0 h0 w0 hpv wpv)
                                      (fun () -> init0 hpv w0 h1 wpv)
                                      (fun () -> init0 hpv wpv h1 w1)
            QuadRope.node ne nw sw se
    init0 0 0 h w

/// Reallocate a rope form the ground up in parallel. Sometimes,
/// this is the only way to improve performance of a badly
/// composed quad rope.
let inline reallocate rope =
    init (QuadRope.rows rope) (QuadRope.cols rope) (QuadRope.get rope)

/// Initialize a rope in parallel where all elements are
/// <code>e</code>.
let inline initAll h w e =
    init h w (fun _ _ -> e)

/// Initialize a rope in parallel with all zeros.
let inline initZeros h w = initAll h w 0

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
    | Slice _ as rope -> map f (QuadRope.Slicing.slice rope)
    | rope -> QuadRope.map f rope

/// Apply f in parallel to each (i, j) of lope and rope.
let zip f lope rope =
    let rec zip0 f lope rope =
         match lope with
             | Node (d, h, w, ne, nw, sw, se) ->
                 let nw0, ne0, sw0, se0 =
                     par4 (fun () ->
                           zip0 f nw (QuadRope.slice rope 0 0
                                                          (QuadRope.rows nw)
                                                          (QuadRope.cols nw)))
                          (fun () ->
                           zip0 f ne (QuadRope.slice rope 0
                                                          (QuadRope.cols nw)
                                                          (QuadRope.rows ne)
                                                          (QuadRope.cols ne)))
                          (fun () ->
                           zip0 f sw (QuadRope.slice rope (QuadRope.rows nw)
                                                          0
                                                          (QuadRope.rows sw)
                                                          (QuadRope.cols sw)))
                          (fun () ->
                           zip0 f se (QuadRope.slice rope (QuadRope.rows ne)
                                                          (QuadRope.cols sw)
                                                          (QuadRope.rows se)
                                                          (QuadRope.cols se)))
                 Node (d, h, w, ne0, nw0, sw0, se0)
             | Slice _ -> zip0 f (QuadRope.Slicing.slice lope) rope
             | _ -> QuadRope.zip f lope rope
    if QuadRope.cols lope <> QuadRope.cols rope || QuadRope.rows lope <> QuadRope.rows rope then
        failwith "ropes must have the same shape"
    zip0 f lope rope

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
    | Slice _ as rope -> hmapreduce f g (QuadRope.Slicing.slice rope)
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
        let ne0, nw0, sw0, se0 = par4 (fun () -> hmapreduce f g ne)
                                      (fun () -> hmapreduce f g nw)
                                      (fun () -> hmapreduce f g sw)
                                      (fun () -> hmapreduce f g se)
        let n = QuadRope.flatNode nw0 ne0
        let s = QuadRope.flatNode sw0 se0
        zip g n s
    | Slice _ as rope -> vmapreduce f g (QuadRope.Slicing.slice rope)
    | rope -> QuadRope.vmapreduce f g rope

/// Reduce all rows of rope by f.
let inline hreduce f rope = hmapreduce id f rope

/// Reduce all columns of rope by f.
let inline vreduce f rope = vmapreduce id f rope

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
    | Slice _ as rope -> mapreduce f g (QuadRope.Slicing.slice rope)
    | rope -> QuadRope.mapreduce f g rope

/// Reduce all values of the rope to a single scalar in parallel.
let inline reduce f rope = mapreduce id f rope

/// Remove all elements from rope for which p does not hold in
/// parallel. Input rope must be of height 1.
let rec hfilter p = function
    | Node (_, 1, _, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> hfilter p ne) (fun () -> hfilter p nw)
        QuadRope.flatNode nw0 ne0
    | Slice _ as rope -> hfilter p (QuadRope.Slicing.slice rope)
    | rope -> QuadRope.hfilter p rope

/// Remove all elements from rope for which p does not hold in
/// parallel. Input rope must be of width 1.
let rec vfilter p = function
    | Node (_, _, 1, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> vfilter p nw) (fun () -> vfilter p sw)
        QuadRope.thinNode nw0 sw0
    | Slice _ as rope -> vfilter p (QuadRope.Slicing.slice rope)
    | rope -> QuadRope.vfilter p rope

/// Reverse the quad rope horizontally in parallel.
let rec hrev = function
    | Node (d, h, w, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun() -> hrev ne)
                                      (fun() -> hrev nw)
                                      (fun() -> hrev sw)
                                      (fun() -> hrev se)
        Node (d, h, w, nw0, ne0, se0, sw0)
    | Slice _ as rope -> hrev (QuadRope.Slicing.slice rope)
    | rope -> QuadRope.hrev rope

/// Reverse the quad rope vertically in parallel.
let rec vrev = function
    | Node (d, h, w, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun() -> vrev ne)
                                      (fun() -> vrev nw)
                                      (fun() -> vrev sw)
                                      (fun() -> vrev se)
        Node (d, h, w, sw0, se0, nw0, ne0)
    | Slice _ as rope -> vrev (QuadRope.Slicing.slice rope)
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
    | Slice _ as rope -> transpose (QuadRope.Slicing.slice rope)
    | rope -> QuadRope.transpose rope
