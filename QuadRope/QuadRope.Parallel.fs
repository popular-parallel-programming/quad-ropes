// Copyright (c) 2016 Florian Biermann, fbie@itu.dk

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:

// * The above copyright notice and this permission notice shall be
//   included in all copies or substantial portions of the Software.

// * The software is provided "as is", without warranty of any kind,
//   express or implied, including but not limited to the warranties of
//   merchantability, fitness for a particular purpose and
//   noninfringement. In no event shall the authors or copyright holders be
//   liable for any claim, damages or other liability, whether in an action
//   of contract, tort or otherwise, arising from, out of or in connection
//   with the software or the use or other dealings in the software.

[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module RadTrees.Parallel.QuadRope

open RadTrees
open Types
open Utils.Tasks

let private rows = QuadRope.rows
let private cols = QuadRope.cols
let private depth = QuadRope.depth
let private isEmpty = QuadRope.isEmpty
let private node = QuadRope.node
let private leaf = QuadRope.leaf
let private flatNode = QuadRope.flatNode
let private thinNode = QuadRope.thinNode

/// Generate a new quad rope in parallel.
let inline init h w (f : int -> int -> _) =
    let rec init h0 w0 h1 w1 (arr : _ [,]) =
        let h = h1 - h0
        let w = w1 - w0
        if h <= QuadRope.s_max && w <= QuadRope.s_max then
            for i in h0 .. h1 - 1 do
                for j in w0 .. w1 - 1 do
                    arr.[i, j] <- f i j
        else if w <= QuadRope.s_max then
            let hpv = h0 + (h >>> 1)
            par2 (fun () -> init h0 w0 hpv w1 arr) (fun () -> init hpv w0 h1 w1 arr) |> ignore
        else if h <= QuadRope.s_max then
            let wpv = w0 + (w >>> 1)
            par2 (fun () -> init h0 w0 h1 wpv arr) (fun () -> init h0 wpv h1 w1 arr) |> ignore
        else
            let hpv = h0 + (h >>> 1)
            let wpv = w0 + (w >>> 1)
            par4 (fun () -> init h0 wpv hpv w1 arr)
                 (fun () -> init h0 w0 hpv wpv arr)
                 (fun () -> init hpv w0 h1 wpv arr)
                 (fun () -> init hpv wpv h1 w1 arr) |> ignore
    let vals = Array2D.zeroCreate h w
    init 0 0 h w vals
    QuadRope.fromArray2D vals

/// Apply a function f to all scalars in parallel.
let map f root =
    let arr = Array2D.zeroCreate (rows root) (cols root)
    let rec map i j qr =
        match qr with
            | Empty -> Empty
            | Leaf vs ->
                ArraySlice.iteri (fun x y e -> arr.[x + i, y + j] <- f e) vs
                leaf (ArraySlice.makeSlice i j (rows qr) (cols qr) arr)
            | Node (_, _, _, ne, nw, Empty, Empty) ->
                let ne0, nw0 = par2 (fun () -> map i (j + cols nw) ne)
                                    (fun () -> map i j nw)
                flatNode nw0 ne0
            | Node (_, _, _, Empty, nw, sw, Empty) ->
                let nw0, sw0 = par2 (fun () -> map i j nw)
                                    (fun () -> map (i + rows nw) j sw)
                thinNode nw0 sw0
            | Node (_, _, _, ne, nw, sw, se) ->
                let ne0, nw0, sw0, se0 = par4 (fun () -> map i (cols nw + j) ne)
                                              (fun () -> map i j nw)
                                              (fun () -> map (i + rows nw) j sw)
                                              (fun () -> map (i + rows ne) (j + cols sw) se)
                node ne0 nw0 sw0 se0
            | Slice _ as qr -> map i j (QuadRope.Slicing.reallocate qr)
    map 0 0 root

/// Fold each row of rope with f in parallel, starting with the
/// according state in states.
let hfold f states qr =
    if rows states <> rows qr then
        invalidArg "states" "Must have the same height as qr."
    let rec fold1 states = function
        | Empty -> states
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw sw) ne se
        | Slice _ as qr -> fold1 states (QuadRope.Slicing.reallocate qr)
        | qr -> QuadRope.hfold f states qr
    and fold2 states n s =
        let nstates, sstates = QuadRope.vsplit2 states (rows n)
        let nstates0, sstates0 = par2 (fun () -> fold1 nstates n) (fun () -> fold1 sstates s)
        QuadRope.thinNode nstates0 sstates0
    fold1 states qr

/// Fold each column of rope with f in parallel, starting with the
/// according state in states.
let vfold f states qr =
    if cols states <> cols qr then
        invalidArg "states" "Must have the same width as qr."
    let rec fold1 states = function
        | Empty -> states
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw ne) sw se
        | Slice _ as qr -> fold1 states (QuadRope.Slicing.reallocate qr)
        | qr -> QuadRope.vfold f states qr
    and fold2 states w e =
        let wstates, estates = QuadRope.hsplit2 states (cols w)
        let wstates0, estates0 = par2 (fun () -> fold1 wstates w) (fun () -> fold1 estates e)
        QuadRope.flatNode wstates0 estates0
    fold1 states qr

/// Zip implementation for the general case where we do not assume
/// that both ropes have the same internal structure.
let rec private genZip f lqr rqr tgt =
    match lqr with
        | Node (d, h, w, ne, nw, Empty, Empty) ->
            let ne0, nw0 =
                par2 (fun () ->
                      let rne = QuadRope.slice 0 (cols nw) (rows ne) (cols ne) rqr
                      genZip f ne rne (Target.ne tgt lqr))
                     (fun () ->
                      let rnw = QuadRope.slice 0 0 (rows nw) (cols nw) rqr
                      genZip f nw rnw tgt)
            Node (d, h, w, ne0, nw0, Empty, Empty)
        | Node (d, h, w, Empty, nw, sw, Empty) ->
            let nw0, sw0 =
                par2 (fun () ->
                      let rnw = QuadRope.slice 0 0 (rows nw) (cols nw) rqr
                      genZip f nw rnw tgt)
                     (fun () ->
                      let rsw = QuadRope.slice (rows nw)  0 (rows sw) (cols sw) rqr
                      genZip f sw rsw (Target.sw tgt lqr))
            Node (d, h, w, Empty, nw0, sw0, Empty)
        | Node (d, h, w, ne, nw, sw, se) ->
            let ne0, nw0, sw0, se0 =
                par4 (fun () ->
                      let rne = QuadRope.slice 0 (cols nw) (rows ne) (cols ne) rqr
                      genZip f ne rne (Target.ne tgt lqr))
                     (fun () ->
                      let rnw = QuadRope.slice 0 0 (rows nw) (cols nw) rqr
                      genZip f nw rnw tgt)
                     (fun () ->
                      let rsw = QuadRope.slice (rows nw) 0 (rows sw) (cols sw) rqr
                      genZip f sw rsw (Target.sw tgt lqr))
                     (fun () ->
                      let rse = QuadRope.slice (rows ne) (cols sw) (rows se) (cols se) rqr
                      genZip f se rse (Target.se tgt lqr))
            Node (d, h, w, ne0, nw0, sw0, se0)
        | Slice _ -> genZip f (QuadRope.Slicing.reallocate lqr) rqr tgt
        | _ -> QuadRope.genZip f lqr rqr tgt

/// Zip function that assumes that the internal structure of two ropes
/// matches. If not, it falls back to the slow, general case.
let rec private fastZip f lqr rqr tgt =
    match lqr, rqr with
        | Empty, Empty -> Empty
        | Leaf _, Leaf _ when QuadRope.shapesMatch lqr rqr ->
                QuadRope.fastZip f lqr rqr tgt
        | Node (d, h, w, lne, lnw, Empty, Empty), Node (_, _, _, rne, rnw, Empty, Empty)
            when QuadRope.subShapesMatch lqr rqr ->
                let ne, nw = par2 (fun () -> fastZip f lne rne (Target.ne tgt lqr))
                                  (fun () -> fastZip f lnw rnw tgt)
                Node (d, h, w, ne, nw, Empty, Empty)
        | Node (d, h, w, Empty, lnw, lsw, Empty), Node (_, _, _, Empty, rnw, rsw, Empty)
            when QuadRope.subShapesMatch lqr rqr ->
                let nw, sw = par2 (fun () -> fastZip f lnw rnw tgt)
                                  (fun () -> fastZip f lsw rsw (Target.sw tgt lqr))
                Node (d, h, w, Empty, nw, sw, Empty)
        | Node (d, h, w, lne, lnw, lsw, lse), Node (_, _, _, rne, rnw, rsw, rse)
            when QuadRope.subShapesMatch lqr rqr ->
                let ne, nw, sw, se = par4 (fun () -> fastZip f lne rne (Target.ne tgt lqr))
                                          (fun () -> fastZip f lnw rnw tgt)
                                          (fun () -> fastZip f lsw rsw (Target.sw tgt lqr))
                                          (fun () -> fastZip f lse rse (Target.se tgt lqr))
                Node (d, h, w, ne, nw, sw, se)
        // It may pay off to reallocate first if both reallocated quad
        // ropes have the same internal shape. This might be
        // over-fitted to matrix multiplication.
        | Slice _, Slice _ -> fastZip f (QuadRope.Slicing.reallocate lqr) (QuadRope.Slicing.reallocate rqr) tgt
        | Slice _, _ ->       fastZip f (QuadRope.Slicing.reallocate lqr) rqr tgt
        | Slice _, Slice _ -> fastZip f lqr (QuadRope.Slicing.reallocate rqr) tgt
        | _ -> genZip f lqr rqr tgt

/// Apply f to each (i, j) of lqr and rqr.
let zip f lqr rqr =
    if not (QuadRope.shapesMatch lqr rqr) then
        invalidArg "rope" "Must have the same shape as first argument."
    fastZip f lqr rqr (Target.make (rows lqr) (cols lqr))

/// Apply f to all values of the rope and reduce the resulting
/// values to a single scalar using g in parallel.
let rec mapreduce f g = function
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        let ne', nw' = par2 (fun () -> mapreduce f g ne) (fun () -> mapreduce f g nw)
        g nw' ne'
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        let nw', sw' = par2 (fun () -> mapreduce f g nw) (fun () -> mapreduce f g sw)
        g nw' sw'
    | Node (_, _, _, ne, nw, sw, se) ->
        let ne', nw', sw', se' = par4 (fun () -> mapreduce f g ne)
                                      (fun () -> mapreduce f g nw)
                                      (fun () -> mapreduce f g sw)
                                      (fun () -> mapreduce f g se)
        g (g nw' ne') (g sw' se')
    | Slice _ as qr -> mapreduce f g (QuadRope.Slicing.reallocate qr)
    | qr -> QuadRope.mapreduce f g qr

/// Reduce all values of the rope to a single scalar in parallel.
let inline reduce f qr = mapreduce id f qr

/// Horizontal mapreduce can be composed from init, mapreduce and slice.
let hmapreduce f g qr =
    init (rows qr) 1 (fun i _ -> mapreduce f g (QuadRope.slice i 0 1 (cols qr) qr))

/// Vertical mapreduce can be composed from init, mapreduce and slice.
let vmapreduce f g qr =
    init 1 (cols qr) (fun _ j -> mapreduce f g (QuadRope.slice 0 j (rows qr) 1 qr))

let inline hreduce f qr = hmapreduce id f qr
let inline vreduce f qr = vmapreduce id f qr

/// Remove all elements from rope for which p does not hold in
/// parallel. Input rope must be of height 1.
let rec hfilter p = function
    | Node (_, 1, _, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> hfilter p ne) (fun () -> hfilter p nw)
        QuadRope.flatNode nw0 ne0
    | Slice _ as qr -> hfilter p (QuadRope.Slicing.reallocate qr)
    | qr -> QuadRope.hfilter p qr

/// Remove all elements from rope for which p does not hold in
/// parallel. Input rope must be of width 1.
let rec vfilter p = function
    | Node (_, _, 1, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> vfilter p nw) (fun () -> vfilter p sw)
        QuadRope.thinNode nw0 sw0
    | Slice _ as qr -> vfilter p (QuadRope.Slicing.reallocate qr)
    | qr -> QuadRope.vfilter p qr

/// Reverse the quad rope horizontally in parallel.
let rec hrev = function
    | Node (d, h, w, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> hrev ne) (fun () -> hrev nw)
        Node (d, h, w, nw0, ne0, Empty, Empty)
    | Node (d, h, w, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> hrev nw) (fun () -> hrev sw)
        Node (d, h, w, Empty, nw0, sw0, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun() -> hrev ne)
                                      (fun() -> hrev nw)
                                      (fun() -> hrev sw)
                                      (fun() -> hrev se)
        Node (d, h, w, nw0, ne0, se0, sw0)
    | Slice _ as qr -> hrev (QuadRope.Slicing.reallocate qr)
    | qr -> QuadRope.hrev qr

/// Reverse the quad rope vertically in parallel.
let rec vrev = function
    | Node (d, h, w, ne, nw, Empty, Empty) ->
        let ne0, nw0 = par2 (fun () -> vrev ne) (fun () -> vrev nw)
        Node (d, h, w, ne0, nw0, Empty, Empty)
    | Node (d, h, w, Empty, nw, sw, Empty) ->
        let nw0, sw0 = par2 (fun () -> vrev nw) (fun () -> vrev sw)
        Node (d, h, w, Empty, sw0, nw0, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        let ne0, nw0, sw0, se0 = par4 (fun() -> vrev ne)
                                      (fun() -> vrev nw)
                                      (fun() -> vrev sw)
                                      (fun() -> vrev se)
        Node (d, h, w, se0, sw0, nw0, ne0)
    | Slice _ as qr -> vrev (QuadRope.Slicing.reallocate qr)
    | qr -> QuadRope.vrev qr


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
    | Slice _ as qr -> transpose (QuadRope.Slicing.reallocate qr)
    | qr -> QuadRope.transpose qr

/// Apply a function with side effects to all elements and their
/// corresponding index pair in parallel.
let iteri f qr =
    let rec iteri f i j = function
        | Empty -> ()
        | Leaf vs -> ArraySlice.iteri (fun i0 j0 v -> f (i + i0) (j + j0) v) vs
        | Node (_, _, _, ne, nw, sw, se) ->
            par4 (fun () -> iteri f i j nw)
                 (fun () -> iteri f i (j + cols nw) ne)
                 (fun () -> iteri f (i + rows nw) j sw)
                 (fun () -> iteri f (i + rows ne) (j + cols sw) se)
            |> ignore
        | Slice _ as qr -> iteri f i j (QuadRope.Slicing.reallocate qr)
    iteri f 0 0 qr

/// Conversion into 2D array.
let toArray2D qr =
    let arr = Array2D.zeroCreate (rows qr) (cols qr)
    // This avoids repeated calls to get; it is enough to traverse
    // the rope once.
    iteri (fun i j v -> arr.[i, j] <- v) qr
    arr

/// Initialize a rope from a native 2D-array in parallel.
let fromArray2D arr =
    let rec init h0 w0 h1 w1 arr =
        let h = h1 - h0
        let w = w1 - w0
        if h <= QuadRope.s_max && w <= QuadRope.s_max then
            QuadRope.leaf (ArraySlice.makeSlice h0 w0 h w arr)
        else if w <= QuadRope.s_max then
            let hpv = h0 + (h >>> 1)
            let n, s = par2 (fun () -> init h0 w0 hpv w1 arr) (fun () -> init hpv w0 h1 w1 arr)
            QuadRope.thinNode n s
        else if h <= QuadRope.s_max then
            let wpv = w0 + (w >>> 1)
            let w, e = par2 (fun () -> init h0 w0 h1 wpv arr) (fun () -> init h0 wpv h1 w1 arr)
            QuadRope.flatNode w e
        else
            let hpv = h0 + (h >>> 1)
            let wpv = w0 + (w >>> 1)
            let ne, nw, sw, se = par4 (fun () -> init h0 wpv hpv w1 arr)
                                      (fun () -> init h0 w0 hpv wpv arr)
                                      (fun () -> init hpv w0 h1 wpv arr)
                                      (fun () -> init hpv wpv h1 w1 arr)
            QuadRope.node ne nw sw se
    init 0 0 (Array2D.length1 arr) (Array2D.length2 arr) arr

/// Reallocate a rope form the ground up. Sometimes, this is the
/// only way to improve performance of a badly composed quad rope.
let inline reallocate qr =
    fromArray2D (toArray2D qr)

/// Initialize a rope in parallel where all elements are
/// <code>e</code>.
let initAll h w e =
    init h w (fun _ _ -> e)

/// Initialize a rope in parallel with all zeros.
let initZeros h w = initAll h w 0
