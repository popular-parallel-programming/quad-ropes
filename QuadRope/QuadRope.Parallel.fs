// Copyright (c) 2017 Florian Biermann, fbie@itu.dk

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
module QuadRope.Parallel.QuadRope

open QuadRope
open QuadRope.Types
open QuadRope.Utils
open QuadRope.Utils.Tasks

let private rows    = QuadRope.rows
let private cols    = QuadRope.cols
let private depth   = QuadRope.depth
let private isEmpty = QuadRope.isEmpty


let private isSparse = QuadRope.isSparse
let private leaf     = QuadRope.leaf
let private hnode    = QuadRope.hnode
let private vnode    = QuadRope.vnode


let private materialize = QuadRope.materialize
let private fastGet     = QuadRope.fastGet


let private smax = QuadRope.smax


/// Generate a new quad rope in parallel.
let init h w (f : int -> int -> _) =
    /// Build nodes by splitting underlying slices horizontally.
    let rec hsplit f slc =
        if ArraySlice.rows slc <= 0 || ArraySlice.cols slc <= 0 then
            Empty
        else if ArraySlice.cols slc <= smax && ArraySlice.rows slc <= smax then
            Future (ArraySlice.rows slc,
                  ArraySlice.cols slc,
                  Tasks.task (fun () -> ArraySlice.init slc f; // Write into shared array.
                                        slc))
        else if ArraySlice.cols slc <= smax then
            vsplit f slc
        else
            hnode (vsplit f (ArraySlice.leftHalf slc)) (vsplit f (ArraySlice.rightHalf slc))
    /// Build nodes by splitting underlying slices vertically.
    and vsplit f slc =
        if ArraySlice.rows slc <= 0 || ArraySlice.cols slc <= 0 then
            Empty
        else if ArraySlice.cols slc <= smax && ArraySlice.rows slc <= smax then
            Future (ArraySlice.rows slc,
                  ArraySlice.cols slc,
                  Tasks.task (fun () -> ArraySlice.init slc f; // Write into shared array.
                                        slc))
        else if ArraySlice.rows slc <= smax then
            hsplit f slc
        else
            vnode (hsplit f (ArraySlice.upperHalf slc)) (hsplit f (ArraySlice.lowerHalf slc))

    // Optimizing closures makes a factor two difference in
    // performance.
    let f' = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
    hsplit f' (ArraySlice.zeroCreate h w)


/// Apply a function f to all scalars in parallel.
let map f qr =
    let rec map qr tgt =
        match qr with
            | Empty -> Empty

            // Initialize target as soon as quad rope is dense.
            | _ when not (isSparse qr) && Target.isEmpty tgt ->
                map qr (Target.make (rows qr) (cols qr))

            // Write into target and construct new leaf.
            | Leaf vs ->
                leaf (Target.map f vs tgt)

            | Future (r, c, t) ->
                Future (r, c, Tasks.map (fun vs -> Target.map f vs tgt) t)

            // Parallel recursive cases, adjust target descriptor.
            | HCat (left = a; right = b) ->
                hnode (map a tgt)
                            (map b (Target.incrementCol tgt (cols a)))

            | VCat (left = a; right = b) ->
                vnode (map a tgt)
                            (map b (Target.incrementRow tgt (rows a)))

            // Materialize quad rope and then map.
            | Slice _ ->
                map (QuadRope.materialize qr) tgt

            // The target is empty, don't write.
            | Sparse (h, w, v) ->
                Sparse (h, w, f v)

    map qr Target.empty


/// Map a function over a quad rope's values and its indexes.
let mapi f qr =
    let f' = Functions.adapt3 f
    let rec mapi i j qr tgt =
        match qr with
            | Empty -> Empty
            | Leaf vs ->
                Leaf (Target.mapi (fun i' j' v -> Functions.invoke3 f' (i + i') (j + j') v) vs tgt)

            | Future (r, c, t) ->
                Future (r, c, Tasks.map (fun vs -> Target.mapi (fun i' j' v -> Functions.invoke3 f' (i + i') (j + j') v) vs tgt) t)

            | HCat (left = a; right = b) ->
                hnode (mapi i j a tgt)
                      (mapi i (j + cols a) b (Target.incrementCol tgt (cols a)))

            | VCat (left = a; right = b) ->
                vnode (mapi i j a tgt)
                      (mapi (i + rows a) j  b (Target.incrementRow tgt (rows a)))

            | Slice _ -> mapi i j (QuadRope.materialize qr) tgt
            // Materialize a sparse leaf and scan it.
            | Sparse (h, w, v) when h <= smax || w <= smax ->
                mapi i j (init h w (fun _ _ -> v)) tgt
            // If both dimensions are larger than smax, we can
            // actually do some work in parallel.
            | Sparse _ -> // when smax < h && smax < w
                let a, b, c, d = QuadRope.split4 qr
                mapi i j (hnode (vnode a b) (vnode c d)) tgt

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
    match rqr with
        | Sparse (_, _, v) ->
            let f' = Functions.adapt2 f
            map (fun x -> Functions.invoke2 f' x v) lqr
        | _ ->
            match lqr with
                // Initialize target if any of the two quad ropes is dense.
                | _ when Target.isEmpty tgt && not (isSparse lqr) ->
                    genZip f lqr rqr (Target.make (rows lqr) (cols lqr))

                | Future (r, c, t) ->
                    let f' = Functions.adapt2 f
                    let zip vs =
                        match materialize rqr with
                            | Sparse (_, _, v') ->
                                Target.map (fun v -> Functions.invoke2 f' v v') vs tgt
                            | rqr ->
                                Target.mapi (fun i j v -> Functions.invoke2 f' v (fastGet rqr i j)) vs tgt
                    Future (r, c, Tasks.map zip t)

                | HCat (_, _, _, _, aa, ab) ->
                    let ba, bb = QuadRope.hsplit2 rqr (cols aa)
                    hnode (genZip f aa ba tgt)
                          (genZip f ab bb (Target.incrementCol tgt (cols aa)))

                | VCat (_, _, _, _, aa, ab) ->
                    let ba, bb = QuadRope.vsplit2 rqr (rows aa)
                    vnode (genZip f aa ba tgt)
                          (genZip f ab bb (Target.incrementRow tgt (rows aa)))

                | Slice _ -> genZip f (materialize lqr) rqr tgt
                | _ -> QuadRope.genZip f lqr rqr tgt


/// Zip function that assumes that the internal structure of two ropes
/// matches. If not, it falls back to the slow, general case.
let rec private fastZip f lqr rqr tgt =
    match lqr, rqr with
        | Empty, Empty -> Empty

        // Initialize target if any of the two quad ropes is dense.
        | _ when Target.isEmpty tgt && not (isSparse lqr) ->
            fastZip f lqr rqr (Target.make (rows lqr) (cols lqr))

        | Leaf _, Leaf _ when QuadRope.shapesMatch lqr rqr ->
            QuadRope.fastZip f lqr rqr tgt

        | Future (r, c, t), Leaf vs' ->
            Future (r, c, Tasks.map (fun vs  -> Target.map2 f vs vs' tgt) t)

        | Leaf vs, Future (r, c, t) ->
            Future (r, c, Tasks.map (fun vs' -> Target.map2 f vs vs' tgt) t)

        | HCat (_, _, _, _, aa, ab), HCat (_, _, _, _, ba, bb)
             when QuadRope.shapesMatch aa ba && QuadRope.shapesMatch ab bb ->
                 hnode (fastZip f aa ba tgt)
                             (fastZip f ab bb (Target.incrementCol tgt (cols aa)))

        | VCat (_, _, _, _, aa, ab), VCat (_, _, _, _, ba, bb)
             when QuadRope.shapesMatch aa ba && QuadRope.shapesMatch ab bb ->
                 vnode (fastZip f aa ba tgt)
                             (fastZip f ab bb (Target.incrementRow tgt (rows aa)))

        // It may pay off to reallocate first if both reallocated quad
        // ropes have the same internal shape. This might be
        // over-fitted to matrix multiplication.
        | Slice _, Slice _ -> fastZip f (QuadRope.materialize lqr) (QuadRope.materialize rqr) tgt
        | Slice _, _       -> fastZip f (QuadRope.materialize lqr) rqr tgt
        | _, Slice _       -> fastZip f lqr (QuadRope.materialize rqr) tgt

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
    fastZip f lqr rqr Target.empty


/// Apply f to all values of the rope and reduce the resulting values
/// to a single scalar using g in parallel. Variable epsilon is the
/// neutral element for g. We assume that g epsilon x = g x epsilon =
/// x.
let rec mapreduce f g epsilon = function
    | HCat (left = a; right = b) | VCat (left = a; right = b) ->
        par2AndThen (fun () -> mapreduce f g epsilon a)
                    (fun () -> mapreduce f g epsilon b)
                    g

    | Slice _ as qr ->
        mapreduce f g epsilon (QuadRope.materialize qr)

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


/// Reverse the quad rope horizontally in parallel.
let hrev qr =
    let rec hrev qr tgt =
        match qr with
            | Leaf vs -> leaf (Target.hrev vs tgt)
            | Future (r, c, t) -> Future (r, c, Tasks.map (fun vs -> Target.hrev vs tgt) t)

            | HCat (left = a; right = b) ->
                hnode (hrev b (Target.incrementCol tgt (cols a)))
                            (hrev a tgt)

            | VCat (left = a; right = b) ->
                vnode (hrev a tgt)
                            (hrev b (Target.incrementRow tgt (rows a)))

            | Slice _ ->
                hrev (QuadRope.materialize qr) tgt
            | _ -> qr
    hrev qr (Target.make (rows qr) (cols qr))


/// Reverse the quad rope vertically in parallel.
let vrev qr =
    let rec vrev qr tgt =
        match qr with
            | Leaf vs -> leaf (Target.vrev vs tgt)
            | Future (r, c, t) -> Future (r, c, Tasks.map (fun vs -> Target.vrev vs tgt) t)

            | HCat (left = a; right = b) ->
                hnode (vrev a tgt)
                            (vrev b (Target.incrementCol tgt (cols a)))

            | VCat (left = a; right = b) ->
                vnode (vrev b (Target.incrementRow tgt (rows a)))
                            (vrev a tgt)

            | Slice _ ->
                vrev (QuadRope.materialize qr) tgt
            | _ -> qr
    vrev qr (Target.make (rows qr) (cols qr))


/// Transpose the quad rope in parallel. This is equal to swapping
/// indices, such that get rope i j = get rope j i.
let transpose qr =
    let rec transpose qr tgt =
        match qr with
            | Empty          -> Empty
            | Leaf vs        -> leaf (Target.transpose vs tgt)
            | Future (r, c, t) -> Future (r, c, Tasks.map (fun vs -> Target.transpose vs tgt) t)

            | HCat (left = a; right = b) ->
                vnode (transpose a tgt)
                            (transpose b (Target.incrementCol tgt (cols a)))

            | VCat (left = a; right = b) ->
                hnode (transpose a tgt)
                            (transpose b (Target.incrementRow tgt (rows a)))

            | Slice _ ->
                transpose (QuadRope.materialize qr) tgt
            | Sparse (h, w, v) -> Sparse (w, h, v)
    transpose qr (Target.make (cols qr) (rows qr))


/// Apply a function with side effects to all elements and their
/// corresponding index pair in parallel.
let iteri f qr =
    let rec iteri f i j = function
        | Empty          -> ()
        | Leaf vs        -> ArraySlice.iteri (fun i0 j0 v -> f (i + i0) (j + j0) v) vs
        | Future (_, _, t) -> ArraySlice.iteri (fun i0 j0 v -> f (i + i0) (j + j0) v) (Tasks.result t)

        | HCat (left = a; right = b) ->
            par2AndThen (fun () -> iteri f i j a) (fun () -> iteri f i (j + cols a) b) (fun _ _ -> ())

        | VCat (left = a; right = b) ->
            par2AndThen (fun () -> iteri f i j a) (fun () -> iteri f (i + rows a) j b) (fun _ _ -> ())

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
            let ne = init h0 wpv hpv w1 arr
            let nw = init h0 w0 hpv wpv arr
            let sw = init hpv w0 h1 wpv arr
            let se = init hpv wpv h1 w1 arr
            hnode (vnode nw sw) (vnode ne se)
        else if w <= QuadRope.smax && QuadRope.smax < h then
            let hpv = h0 + (h >>> 1)
            vnode (init h0 w0 hpv w1 arr) (init hpv w0 h1 w1 arr)
        else if QuadRope.smax < w && h <= QuadRope.smax then
            let wpv = w0 + (w >>> 1)
            hnode (init h0 w0 h1 wpv arr) (init h0 wpv h1 w1 arr)
        else
            Future (h, w, Tasks.task (fun () -> ArraySlice.makeSlice h0 w0 h w arr))
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

    let inline parCond a b c _ =
        rows c <= rows a && cols b <= cols a

    /// A quad rope with branches a b d c, where b and c can be
    /// processed in parallel; i.e., we assume rows c <= rows a and
    /// cols b <= cols a.
    let rec parScan f a b c d tgt =
        let a' = scan f a tgt
        let b', c' = par2 (fun () -> scan f b (Target.incrementRow tgt (rows a)))
                          (fun () -> scan f c (Target.incrementCol tgt (cols a)))

        let tgt' = Target.increment tgt (rows c) (cols b)
        a', b', c', scan f d tgt'

    // Prefix is implicitly passed on through side effects when
    // writing into tgt. Therefore, we need to take several cases of
    // dependencies into account:
    //
    // - flat or thin node: no parallelism.
    // - rows ne <= rows nw && cols sw <= cols nw: scan ne and sw in parallel.
    // - cols nw < cols sw: scan ne before sw.
    // - rows nw < rows ne: scan sw before ne.
    and scan f qr tgt =
        match qr with
            | Empty -> Empty
            | Leaf slc ->
                Target.scan f slc tgt
                Leaf (Target.toSlice tgt (ArraySlice.rows slc) (ArraySlice.cols slc))

            // Memory writing effects must occur before continuing.
            | Future (_, _, t) -> scan f (Tasks.result >> leaf <| t) tgt

            // Parallel cases: b and c depend only on a, d
            // depends on all other children. Scan b and c in
            // parallel.
            | HCat (_, _, _, _, VCat (left = a; right = b), VCat (_, _, _, _, c, d))
                when parCond a b c d ->
                    let a', b', c', d' = parScan f a b c d tgt
                    hnode (vnode a' b') (vnode c' d')

            // We need a second case to re-construct nodes correctly.
            | VCat (_, _, _, _, HCat (_, _, _, _, a, c), HCat (_, _, _, _, b, d))
                when parCond a b c d ->
                    let a', b', c', d' = parScan f a b c d tgt
                    vnode (hnode a' c') (hnode b' d')

            // Sequential cases.
            | HCat (left = a; right = b) ->
                let a' = scan f a tgt
                let tgt' = Target.incrementCol tgt (cols a)
                let b' = scan f b tgt'
                hnode a' b'

            | VCat (left = a; right = b) ->
                let a' = scan f a tgt
                let tgt' = Target.incrementRow tgt (rows a)
                let b' = scan f b tgt'
                vnode a' b'

            | Slice _ -> scan f (QuadRope.materialize qr) tgt

            // Materialize a sparse leaf and scan it.
            | Sparse (h, w, v) when h <= smax || w <= smax ->
                // Fill the target imperatively.
                Target.fill tgt h w v
                // Get a slice over the target.
                let slc = Target.toSlice tgt h w
                // Compute prefix imperatively.
                Target.scan f slc tgt
                // Make a new quad rope from array slice.
                leaf slc

            // If both dimensions are larger than smax, we can
            // actually do some work in parallel.
            | Sparse _ -> // when smax < h && smax < w
                let a, b, c, d = QuadRope.split4 qr
                scan f (hnode (vnode a b) (vnode c d)) tgt

    // Initial target and start of recursion.
    let tgt = Target.makeWithFringe (rows qr) (cols qr) init
    scan f qr tgt


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
        | HCat (left = Sparse (_, _, 0.0))
        | VCat (left = Sparse (_, _, 0.0))
        | HCat (right = Sparse (_, _, 0.0))
        | VCat (right = Sparse (_, _, 0.0))
        | Sparse (_, _, 0.0) -> 0.0
        | Sparse (_, _, 1.0) -> 1.0
        | HCat (left = VCat (left = a; right = b); right = VCat (left = c; right = d))
        | VCat (left = HCat (left = a; right = c); right = HCat (left = b; right = d)) ->
            let ab = par2AndThen (fun () -> prod a) (fun () -> prod b) (*)
            if ab = 0.0 then
                0.0
            else
                ab * (par2AndThen (fun () -> prod c) (fun () -> prod d) (*))
        | HCat (left = a; right = b) ->
            par2AndThen (fun () -> prod a) (fun () -> prod b) (*)
        | VCat (left = a; right = b) ->
            par2AndThen (fun () -> prod a) (fun () -> prod b) (*)
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
                        | HCat (sparse = true; left = aa; right = ab) ->
                            hnode (gen aa (QuadRope.hslice 0 (cols aa) rqr))
                                  (gen ab (QuadRope.hslice (cols aa) (cols ab) rqr))

                        // Thin node.
                        | VCat (sparse = true; left = aa; right = ab) ->
                            vnode (gen aa (QuadRope.vslice 0 (rows aa) rqr))
                                  (gen ab (QuadRope.vslice (rows aa) (rows ab) rqr))

                        | Sparse (_, _, 0.0) -> lqr
                        | Sparse (_, _, 1.0) -> rqr

                        | _ -> zip (*) lqr rqr

        /// Fast case for quad ropes of equal structure.
        let rec fast lqr rqr =
            match lqr, rqr with
                // Flat node.
                | HCat (sparse = true; left = aa; right = ab), HCat (left = ba; right = bb)
                | HCat (left = aa; right = ab), HCat (sparse = true; left = ba; right = bb)
                    when QuadRope.shapesMatch aa ba && QuadRope.shapesMatch ab bb ->
                        hnode (fast aa ba) (fast ab bb)

                // Thin node.
                | VCat (sparse = true; left = aa; right = ab), VCat (left = ba; right = bb)
                | VCat (left = aa; right = ab), VCat (sparse = true; left = ba; right = bb)
                    when QuadRope.shapesMatch aa ba && QuadRope.shapesMatch ab bb ->
                        vnode (fast aa ba) (fast ab bb)

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
        // Since multiplication is commutative, move the shallower
        // quad rope left to recurse on its structure instead of the
        // deeper one.
        if depth rqr < depth lqr then
            fast rqr lqr
        else
            fast lqr rqr
