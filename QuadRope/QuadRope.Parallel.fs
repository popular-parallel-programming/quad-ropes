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


let private isSparse = QuadRope.isSparse
let private leaf = QuadRope.leaf
let private hnode = QuadRope.hnode
let private vnode = QuadRope.vnode


let private smax = QuadRope.smax


/// Generate a new quad rope in parallel.
let init h w (f : int -> int -> _) =
    /// Build nodes by splitting underlying slices horizontally.
    let rec hsplit slc =
        if ArraySlice.rows slc <= 0 || ArraySlice.cols slc <= 0 then
            Empty
        else if ArraySlice.cols slc <= smax && ArraySlice.rows slc <= smax then
            ArraySlice.init slc f // Write into shared array.
            leaf slc
        else if ArraySlice.cols slc <= smax then
            vsplit slc
        else
            par2 (fun () -> vsplit (ArraySlice.leftHalf slc)) (fun () -> vsplit (ArraySlice.rightHalf slc))
            ||> hnode
    /// Build nodes by splitting underlying slices vertically.
    and vsplit slc =
        if ArraySlice.rows slc <= 0 || ArraySlice.cols slc <= 0 then
            Empty
        else if ArraySlice.cols slc <= smax && ArraySlice.rows slc <= smax then
            ArraySlice.init slc f // Write into shared array.
            leaf slc
        else if ArraySlice.rows slc <= smax then
            hsplit slc
        else
            par2 (fun () -> hsplit (ArraySlice.upperHalf slc)) (fun () -> hsplit (ArraySlice.lowerHalf slc))
            ||> vnode

    hsplit (ArraySlice.zeroCreate h w)


/// Apply a function f to all scalars in parallel.
let map f qr =
    let rec map qr tgt =
        match qr with
            | Empty -> Empty

            // Initialize target as soon as quad rope is dense.
            | _ when not (isSparse qr) && Target.isEmpty tgt ->
                map qr (Target.make (rows qr) (cols qr))

            // Write into target and construct new leaf.
            | Leaf slc ->
                leaf (Target.map f slc tgt)

            // Parallel recursive cases, adjust target descriptor.
            | HCat (_, _, _, _, a, b) ->
                par2 (fun () -> map a tgt) (fun () -> map b (Target.incrementCol tgt (cols a))) ||> hnode

            | VCat (_, _, _, _, a, b) ->
                par2 (fun () -> map a tgt) (fun () -> map b (Target.incrementRow tgt (rows a))) ||> vnode

            // Materialize quad rope and then map.
            | Slice _ ->
                map (QuadRope.materialize qr) tgt

            // The target is empty, don't write.
            | Sparse (h, w, v) ->
                Sparse (h, w, f v)

    map qr Target.empty


/// Map a function over a quad rope's values and its indexes.
let mapi f qr =
    let rec mapi i j qr tgt =
        match qr with
            | Empty -> Empty
            | Leaf slc ->
                Leaf (Target.mapi (fun i' j' v -> f (i + i') (j + j') v)  slc tgt)
            | HCat (_, _, _, _, a, b) ->
                hnode (mapi i j a tgt) (mapi i (j + cols a) b (Target.incrementCol tgt (cols a)))
            | VCat (_, _, _, _, a, b) ->
                vnode (mapi i j a tgt) (mapi (i + rows a) j  b (Target.incrementRow tgt (rows a)))
            | Slice _ -> mapi i j (QuadRope.materialize qr) tgt
            // Materialize a sparse leaf and scan it.
            | Sparse (h, w, v) when h <= smax || w <= smax ->
                mapi i j (init h w (fun _ _ -> v)) tgt
            // If both dimensions are larger than smax, we can
            // actually do some work in parallel.
            | Sparse _ -> // when smax < h && smax < w
                let a, b, c, d = QuadRope.split4 qr
                mapi i j  (hnode (vnode a b) (vnode c d)) tgt

    mapi 0 0 qr (Target.make (rows qr) (cols qr))


/// Map a function f to each row of the quad rope.
let hmap f qr =
    init (rows qr) 1 (fun i _ -> QuadRope.slice i 0 1 (cols qr) qr |> f)


/// Map a function f to each column of the quad rope.
let vmap f qr =
    init 1 (cols qr) (fun _ j -> QuadRope.slice 0 j (rows qr) 1 qr |> f)


/// Zip implementation for the general case where we do not assume
/// that both ropes have the same internal structure.
let rec private genZip f lqr rqr tgt =
    match lqr with
        // Initialize target if any of the two quad ropes is dense.
        | _ when Target.isEmpty tgt && not (isSparse lqr) ->
            genZip f lqr rqr (Target.make (rows lqr) (cols lqr))

        | HCat (_, _, _, _, aa, ab) ->
            par2 (fun () ->
                  let ba = QuadRope.hslice 0 (cols aa) rqr
                  genZip f aa ba tgt)
                 (fun () ->
                  let bb = QuadRope.hslice (cols aa) (cols ab) rqr
                  genZip f ab bb (Target.incrementCol tgt (cols aa)))
            ||> hnode

        | VCat (_, _, _, _, aa, ab) ->
            par2 (fun () ->
                  let ba = QuadRope.vslice 0 (rows aa) rqr
                  genZip f aa ba tgt)
                 (fun () ->
                  let bb = QuadRope.vslice (rows aa) (rows ab) rqr
                  genZip f ab bb (Target.incrementRow tgt (rows aa)))
            ||> vnode

        | Slice _ -> genZip f (QuadRope.materialize lqr) rqr tgt
        | _ -> QuadRope.genZip f lqr rqr tgt


/// Zip function that assumes that the internal structure of two ropes
/// matches. If not, it falls back to the slow, general case.
let rec private fastZip f lqr rqr tgt =
    match lqr, rqr with
        | Empty, Empty -> Empty

        // Initialize target if any of the two quad ropes is dense.
        | _ when Target.isEmpty tgt && (not (isSparse lqr) || not (isSparse rqr)) ->
            fastZip f lqr rqr (Target.make (rows lqr) (cols lqr))

        | Leaf _, Leaf _ when QuadRope.shapesMatch lqr rqr ->
                QuadRope.fastZip f lqr rqr tgt

        | HCat (_, _, _, _, aa, ab), HCat (_, _, _, _, ba, bb)
            when QuadRope.shapesMatch aa ba && QuadRope.shapesMatch ab bb ->
                par2 (fun () -> fastZip f aa ba tgt)
                     (fun () -> fastZip f ab bb (Target.incrementCol tgt (cols aa)))
                ||> hnode

        | VCat (_, _, _, _, aa, ab), HCat (_, _, _, _, ba, bb)
             when QuadRope.shapesMatch aa ba && QuadRope.shapesMatch ab bb ->
                par2 (fun () -> fastZip f aa ab tgt)
                     (fun () -> fastZip f ba bb (Target.incrementRow tgt (rows aa)))
                ||> vnode

        // It may pay off to reallocate first if both reallocated quad
        // ropes have the same internal shape. This might be
        // over-fitted to matrix multiplication.
        | Slice _, Slice _ -> fastZip f (QuadRope.materialize lqr) (QuadRope.materialize rqr) tgt
        | Slice _, _ ->       fastZip f (QuadRope.materialize lqr) rqr tgt
        | Slice _, Slice _ -> fastZip f lqr (QuadRope.materialize rqr) tgt

        // Sparse branches can be reduced to either a single call to f
        // in case both arguments are sparse, or to a mapping f.
        | Sparse (h, w, v1), Sparse (_, _, v2) -> Sparse (h, w, f v1 v2)
        | Sparse (_, _, v), _ -> map (f v) rqr
        | _, Sparse (_, _, v) -> map (fun x -> f x v) lqr

        // Fall back to general case.
        | _ -> genZip f lqr rqr tgt


/// Apply f to each (i, j) of lqr and rqr.
let zip f lqr rqr =
    if not (QuadRope.shapesMatch lqr rqr) then
        invalidArg "rqr" "Must have the same shape as first argument."
    fastZip f lqr rqr (Target.make (rows lqr) (cols lqr))


/// Apply f to all values of the rope and reduce the resulting values
/// to a single scalar using g in parallel. Variable epsilon is the
/// neutral element for g. We assume that g epsilon x = g x epsilon =
/// x.
let rec mapreduce f g epsilon = function
    | HCat (_, _, _, _, a, b) ->
        par2 (fun () -> mapreduce f g epsilon a) (fun () -> mapreduce f g epsilon b) ||> g

    | VCat (_, _, _, _, a, b) ->
        par2 (fun () -> mapreduce f g epsilon a) (fun () -> mapreduce f g epsilon b) ||> g

    | Slice _ as qr -> mapreduce f g epsilon (QuadRope.materialize qr)
    | qr -> QuadRope.mapreduce f g epsilon qr


/// Reduce all values of the rope to a single scalar in parallel.
let reduce f epsilon qr = mapreduce id f epsilon qr


let hmapreduce f g epsilon qr = hmap (mapreduce f g epsilon) qr
let vmapreduce f g epsilon qr = vmap (mapreduce f g epsilon) qr


let hreduce f epsilon qr = hmapreduce id f epsilon qr
let vreduce f epsilon qr = vmapreduce id f epsilon qr


/// Apply predicate p to all elements of qr in parallel and reduce the
/// elements in both dimension using logical and.
let forall p qr = mapreduce p (&&) true qr


/// Apply predicate p to all elements of qr in parallel and reduce the
/// elements in both dimensions using logical or.
let exists p qr = mapreduce p (||) false qr


/// Remove all elements from rope for which p does not hold in
/// parallel. Input rope must be of height 1.
let rec hfilter p = function
    | HCat (_, _, 1, _, a, b) ->
        par2 (fun () -> hfilter p a) (fun () -> hfilter p b) ||> hnode
    | Slice _ as qr -> hfilter p (QuadRope.materialize qr)
    | qr -> QuadRope.hfilter p qr


/// Remove all elements from rope for which p does not hold in
/// parallel. Input rope must be of width 1.
let rec vfilter p = function
    | VCat (_, _, _, 1, a, b) ->
        par2 (fun () -> vfilter p a) (fun () -> vfilter p b) ||> vnode
    | Slice _ as qr -> vfilter p (QuadRope.materialize qr)
    | qr -> QuadRope.vfilter p qr


/// Reverse the quad rope horizontally in parallel.
let hrev qr =
    let rec hrev qr tgt =
        match qr with
            | Leaf slc ->
                leaf (Target.hrev slc tgt)
            | HCat (_, _, _, _, a, b) ->
                par2 (fun () -> hrev b (Target.incrementCol tgt (cols a))) (fun () -> hrev a tgt)
                ||> hnode
            | VCat (_, _, _, _, a, b) ->
                par2 (fun () -> hrev a tgt) (fun () -> hrev b (Target.incrementRow tgt (rows a)))
                ||> vnode
            | Slice _ ->
                hrev (QuadRope.materialize qr) tgt
            | _ -> qr
    hrev qr (Target.make (rows qr) (cols qr))


/// Reverse the quad rope vertically in parallel.
let vrev qr =
    let rec vrev qr tgt =
        match qr with
            | Leaf slc ->
                leaf (Target.vrev slc tgt)
            | HCat (_, _, _, _, a, b) ->
                par2 (fun () -> vrev a tgt) (fun () -> vrev b (Target.incrementCol tgt (cols a)))
                ||> hnode
            | VCat (_, _, _, _, a, b) ->
                par2 (fun () -> vrev b (Target.incrementRow tgt (rows a))) (fun () -> vrev a tgt)
                ||> vnode
            | Slice _ ->
                vrev (QuadRope.materialize qr) tgt
            | _ -> qr
    vrev qr (Target.make (rows qr) (cols qr))


/// Transpose the quad rope in parallel. This is equal to swapping
/// indices, such that get rope i j = get rope j i.
let transpose qr =
    let rec transpose qr tgt =
        match qr with
            | Empty -> Empty
            | Leaf slc ->
                leaf (Target.transpose slc tgt)
            | HCat (_, _, _, _, a, b) ->
                par2 (fun () -> transpose a tgt) (fun () -> transpose b (Target.incrementCol tgt (cols a)))
                ||> vnode
            | VCat (_, _, _, _, a, b) ->
                par2 (fun () -> transpose a tgt) (fun () -> transpose b (Target.incrementRow tgt (rows a)))
                ||> hnode
            | Slice _ ->
                transpose (QuadRope.materialize qr) tgt
            | Sparse (h, w, v) -> Sparse (w, h, v)
    transpose qr (Target.make (cols qr) (rows qr))


/// Apply a function with side effects to all elements and their
/// corresponding index pair in parallel.
let iteri f qr =
    let rec iteri f i j = function
        | Empty -> ()
        | Leaf vs -> ArraySlice.iteri (fun i0 j0 v -> f (i + i0) (j + j0) v) vs

        | HCat (_, _, _, _, a, b) ->
            par2 (fun () -> iteri f i j a) (fun () -> iteri f i (j + cols a) b)
            |> ignore

        | VCat (_, _, _, _, a, b) ->
            par2 (fun () -> iteri f i j a) (fun () -> iteri f (i + rows a) j b)
            |> ignore

        | Slice _ as qr -> iteri f i j (QuadRope.materialize qr)

        // Small sparse quad rope, process sequentially.
        | Sparse (h, w, v) when h <= smax && w <= smax ->
            for r in i .. i + h - 1 do
                for c in j .. j + w - 1 do
                    f r c v

        // Wide sparse quad rope, split horizontally.
        | Sparse (h, w, _) as qr when h <= smax ->
            let w' = w / 2
            par2 (fun () -> iteri f i j (QuadRope.hslice 0 w' qr))
                 (fun () -> iteri f i (j + w') (QuadRope.hslice w' w qr))
            |> ignore

        // Tall sparse quad rope, split vertically.
        | Sparse (h, w, _) as qr when w <= smax ->
            let h' = h / 2
            par2 (fun () -> iteri f i j (QuadRope.vslice 0 h' qr))
                 (fun () -> iteri f (i + h') j (QuadRope.vslice h' h qr))
            |> ignore

        | Sparse _ as qr ->
            let a, b, c, d = QuadRope.split4 qr
            iteri f i j (hnode (vnode a b) (vnode c d))

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
        if smax < h && smax < w then
            let hpv = h0 + (h >>> 1)
            let wpv = w0 + (w >>> 1)
            let ne, nw, sw, se = par4 (fun () -> init h0 wpv hpv w1 arr)
                                      (fun () -> init h0 w0 hpv wpv arr)
                                      (fun () -> init hpv w0 h1 wpv arr)
                                      (fun () -> init hpv wpv h1 w1 arr)
            hnode (vnode nw sw) (vnode ne se)
        else if w <= QuadRope.smax && QuadRope.smax < h then
            let hpv = h0 + (h >>> 1)
            let n, s = par2 (fun () -> init h0 w0 hpv w1 arr) (fun () -> init hpv w0 h1 w1 arr)
            vnode n s
        else if QuadRope.smax < w && h <= QuadRope.smax then
            let wpv = w0 + (w >>> 1)
            let w, e = par2 (fun () -> init h0 w0 h1 wpv arr) (fun () -> init h0 wpv h1 w1 arr)
            hnode w e
        else
            QuadRope.leaf (ArraySlice.makeSlice h0 w0 h w arr)
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


/// Compute the generalized summed area table in parallel, if
/// possible. All rows and columns are initialized with init. Function
/// f will be called like this:
/// f(I(i, j - 1), I(i - 1, j - 1), I(i - 1, j), I(i,j))
let scan f init qr =
    // Prefix is implicitly passed on through side effects when
    // writing into tgt. Therefore, we need to take several cases of
    // dependencies into account:
    //
    // - flat or thin node: no parallelism.
    // - rows ne <= rows nw && cols sw <= cols nw: scan ne and sw in parallel.
    // - cols nw < cols sw: scan ne before sw.
    // - rows nw < rows ne: scan sw before ne.
    let rec scan pre qr tgt =
        match qr with
            | Empty -> Empty
            | Leaf slc ->
                Target.scan f pre slc tgt
                Leaf (Target.toSlice tgt (ArraySlice.rows slc) (ArraySlice.cols slc))

            // Parallel cases: b and c depend only on a, d
            // depends on all other children. Scan b and c in
            // parallel.
            | HCat (_, _, _, _, VCat (_, _, _, _, a, b), VCat (_, _, _, _, c, d))
                when rows c <= rows a && cols b <= cols a ->
                    let a' = scan pre a tgt
                    let b', c' = par2 (fun () ->
                                       let tgt' = Target.incrementRow tgt (rows a)
                                       scan (Target.get tgt') b tgt')
                                      (fun () ->
                                       let tgt' = Target.incrementCol tgt (cols a)
                                       scan (Target.get tgt') c tgt')
                    let tgt' = Target.increment tgt (rows c) (cols b)
                    let d' = scan (Target.get tgt') d tgt'
                    hnode (vnode a' b') (vnode c' d')

            // We need a second case to re-construct nodes correctly.
            | VCat (_, _, _, _, HCat (_, _, _, _, a, c), HCat (_, _, _, _, b, d))
                when rows c <= rows a && cols b <= cols a ->
                    let a' = scan pre a tgt
                    let b', c' = par2 (fun () ->
                                       let tgt' = Target.incrementRow tgt (rows a)
                                       scan (Target.get tgt') b tgt')
                                      (fun () ->
                                       let tgt' = Target.incrementCol tgt (cols a)
                                       scan (Target.get tgt') c tgt')
                    let tgt' = Target.increment tgt (rows c) (cols b)
                    let d' = scan (Target.get tgt') d tgt'
                    vnode (hnode a' c') (hnode b' d')

            // Sequential cases.
            | HCat (_, _, _, _, a, b) ->
                let a' = scan pre a tgt
                let tgt' = Target.incrementCol tgt (cols a)
                let b' = scan (Target.get tgt') b tgt'
                hnode a' b'

            | VCat (_, _, _, _, a, b) ->
                let a' = scan pre a tgt
                let tgt' = Target.incrementRow tgt (rows a)
                let b' = scan (Target.get tgt') b tgt'
                vnode a' b'

            | Slice _ -> scan pre (QuadRope.materialize qr) tgt

            // Materialize a sparse leaf and scan it.
            | Sparse (h, w, v) when h <= smax || w <= smax ->
                // Fill the target imperatively.
                Target.fill tgt h w v
                // Get a slice over the target.
                let slc = Target.toSlice tgt h w
                // Compute prefix imperatively.
                Target.scan f pre slc tgt
                // Make a new quad rope from array slice.
                leaf slc

            // If both dimensions are larger than smax, we can
            // actually do some work in parallel.
            | Sparse _ -> // when smax < h && smax < w
                let a, b, c, d = QuadRope.split4 qr
                scan pre (hnode (vnode a b) (vnode c d)) tgt

    // Initial target and start of recursion.
    let tgt = Target.makeWithFringe (rows qr) (cols qr) init
    scan (Target.get tgt) qr tgt


/// Compare two quad ropes element wise and return true if they are
/// equal. False otherwise.
let equals qr0 qr1 =
    reduce (&&) true (zip (=) qr0 qr1)



module SparseDouble =

    /// We cannot optimize any further than parallelizing; reduce is
    /// already optimized for sparse quad ropes.
    let sum = reduce (+) 0.0


    /// Compute the product of all values in a quad rope in
    /// parallel. This function short-circuits the computation where
    /// possible, but it does not stop already running computations.
    let rec prod = function
        | HCat (_, _, _, _, Sparse (_, _, 0.0), _)
        | HCat (_, _, _, _, _, Sparse (_, _, 0.0))
        | VCat (_, _, _, _, Sparse (_, _, 0.0), _)
        | VCat (_, _, _, _, _, Sparse (_, _, 0.0))
        | Sparse (_, _, 0.0) -> 0.0
        | Sparse (_, _, 1.0) -> 1.0
        | HCat (_, _, _, _, VCat (_, _, _, _, a, b), VCat (_, _, _, _, c, d))
        | VCat (_, _, _, _, HCat (_, _, _, _, a, c), HCat (_, _, _, _, b, d)) ->
            let ab = par2 (fun () -> prod a) (fun () -> prod b) ||> (*)
            if ab = 0.0 then
                0.0
            else
                ab * (par2 (fun () -> prod c) (fun () -> prod d) ||> (*))
        | HCat (_, _, _, _, a, b) ->
            par2 (fun () -> prod a) (fun () -> prod b) ||> (*)
        | VCat (_, _, _, _, a, b) ->
            par2 (fun () -> prod a) (fun () -> prod b) ||> (*)
        | Slice _ as qr -> prod (QuadRope.materialize qr)
        | qr -> reduce (*) 1.0 qr


    /// Multiply two quad ropes of doubles point-wise.
    let pointwise lqr rqr =
        /// General case for quad ropes of different structure.
        let rec gen lqr rqr =
            match rqr with
                // Sparse quad ropes of zero result in zero.
                | Sparse (_, _, 0.0) -> rqr
                | Sparse (_, _, 1.0) -> lqr
                | _ ->
                    match lqr with
                        // Flat node.
                        | HCat (true, _, _, _, aa, ab) ->
                            par2 (fun () -> gen aa (QuadRope.hslice 0 (cols aa) rqr))
                                 (fun () -> gen ab (QuadRope.hslice (cols aa) (cols ab) rqr))
                            ||> hnode

                        // Thin node.
                        | VCat (true, _, _, _, aa, ab) ->
                            par2 (fun () -> gen aa (QuadRope.vslice 0 (rows aa) rqr))
                                 (fun () -> gen ab (QuadRope.vslice (rows aa) (rows ab) rqr))
                            ||> vnode

                        | Sparse (_, _, 0.0) -> lqr
                        | Sparse (_, _, 1.0) -> rqr

                        | _ -> zip (*) lqr rqr

        /// Fast case for quad ropes of equal structure.
        let rec fast lqr rqr =
            match lqr, rqr with
                // Flat node.
                | HCat (true, _, _, _, aa, ab), HCat (_,    _, _, _, ba, bb)
                | HCat (_,    _, _, _, aa, ab), HCat (true, _, _, _, ba, bb)
                    when QuadRope.shapesMatch aa ba && QuadRope.shapesMatch ab bb ->
                        par2 (fun () -> fast aa ba) (fun () -> fast ab bb) ||> hnode

                // Thin node.
                | VCat (true, _, _, _, aa, ab), HCat (_,    _, _, _, ba, bb)
                | VCat (_,    _, _, _, aa, ab), HCat (true, _, _, _, ba, bb)
                    when QuadRope.shapesMatch aa ba && QuadRope.shapesMatch ab bb ->
                        par2 (fun () -> fast aa ba) (fun () -> fast ab bb) ||> vnode

                // It may pay off to materialize if slices are sparse.
                | Slice _, _ when isSparse lqr -> fast (QuadRope.materialize lqr) rqr
                | _, Slice _ when isSparse rqr -> fast lqr (QuadRope.materialize rqr)

                // Sparse quad ropes of zero result in zero.
                | Sparse (h, w, 0.0), _
                | _, Sparse (h, w, 0.0) -> Sparse (h, w, 0.0)

                // Sparse quad ropes of one result in the other quad rope.
                | Sparse (_, _, 1.0), qr
                | qr, Sparse (_, _, 1.0) -> qr

                // Fall back to general case.
                | _ when isSparse lqr || isSparse rqr -> gen lqr rqr
                | _ -> zip (*) lqr rqr

        if not (QuadRope.shapesMatch lqr rqr) then
            invalidArg "rqr" "Quad ropes must be of equal shape."
        fast lqr rqr
