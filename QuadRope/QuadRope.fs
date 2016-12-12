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
module RadTrees.QuadRope

open Types
open Utils

(* The maximal size of a leaf array in any direction. *)
#if DEBUG
let s_max = 4
#else
let s_max = 32
#endif

// Aliases for more concise code.
let inline rows qr = Types.rows qr
let inline cols qr = Types.cols qr
let inline depth qr = Types.depth qr
let inline isEmpty qr = Types.isEmpty qr

/// True if the quad rope contains a sparse sub-tree.
let rec isSparse = function
    | Sparse _ -> true
    | Node (true, _, _, _, _, _, _, _) -> true
    | Slice ( _, _, _, _, qr) -> isSparse qr
    | _ -> false

/// Construct a Leaf if slc is non-empty. Otherwise, return Empty.
let leaf slc =
    if ArraySlice.length1 slc = 0 || ArraySlice.length2 slc = 0 then
        Empty
    else
        Leaf slc

/// Pseudo-constructor for generating a new rope out of some
/// existing nodes. This function maintains node construction invariants.
let rec node ne nw sw se =
    match ne, nw, sw, se with
        | _,     Empty, Empty, Empty -> ne
        | Empty, _,     Empty, Empty -> nw
        | Empty, Empty, _,     Empty -> sw
        | Empty, Empty, Empty, _     -> se
        | Empty, Empty, _,     _     -> node se sw Empty Empty
        | _,     Empty, Empty, _     -> node Empty ne se Empty
        | _,     Empty, _,     Empty -> node ne sw Empty Empty
        | Empty, _    , Empty, _     -> node se nw Empty Empty
        | _ ->
            let d = max (max (depth ne) (depth nw)) (max (depth sw) (depth se)) + 1
            let h = rows nw + rows sw
            let w = cols nw + cols ne
            Node (isSparse ne || isSparse nw || isSparse sw || isSparse se, d, h, w, ne, nw, sw, se)

let inline flatNode w e =
    node e w Empty Empty (* NB: Arguments switched. *)

let inline thinNode n s =
    node Empty n s Empty

let inline private withinRange qr i j =
    0 <= i && i < rows qr && 0 <= j && j < cols qr

let inline private checkBounds qr i j =
    if rows qr <= i then
        invalidArg "i" (sprintf "First index must be within bounds of quad rope: %d" i)
    if cols qr <= j then
        invalidArg "j" (sprintf "Second index must be within bounds of quad rope: %d" j)

/// Get the value of a location in the tree. This function does not
/// check whether i, j are within bounds.
let rec internal fastGet qr i j =
    match qr with
        | Empty -> invalidArg "qr" "Empty quad rope contains no values."
        | Leaf vs -> ArraySlice.get vs i j
        | Node (s, _, _, _, ne, nw, sw, se) ->
            if withinRange nw i j then
                fastGet nw i j
            else if withinRange ne i (j - cols nw) then
                fastGet ne i (j - cols nw)
            else if withinRange sw (i - rows nw) j then
                fastGet sw (i - rows nw) j
            else
                fastGet se (i - rows ne) (j - cols sw)
        | Slice (x, y, _, _, qr) -> fastGet qr (i + x) (j + y)
        | Sparse (_, _, v) -> v

/// Get the value of a location in the tree.
let get root i j =
    checkBounds root i j
    fastGet root i j

/// Update a tree location without modifying the original tree.
let set root i j v =
    let rec set qr i j v =
        match qr with
            | Empty ->
                invalidArg "qr" "Empty quad rope cannot be set."
            | Leaf vs ->
                Leaf (ArraySlice.set vs i j v)
            | Node (s, d, h, w, ne, nw, sw, se) ->
                if withinRange nw i j then
                    Node (s, d, h, w, ne, set nw i j v, sw, se)
                else if withinRange ne i (j - cols nw) then
                    Node (s, d, h, w, set ne i (j - cols nw) v, nw, sw, se)
                else if withinRange sw (i - rows nw) j then
                    Node (s, d, h, w, ne, nw, set sw (i - rows nw) j v, se)
                else
                    Node (s, d, h, w, ne, nw, sw, set se (i - rows ne) (j - cols sw) v)
            // Set the underlying quad rope with offset.
            | Slice (x, y, h, w, qr) ->
                Slice (x, y, h, w, set qr (i + x) (j + y) v)
            // Initialize
            | Sparse (h, w, v') ->
                let vals = Array2D.create h w v'
                vals.[i, j] <- v
                leaf (ArraySlice.make vals)
    checkBounds root i j
    set root i j v

let private isBalanced d s =
    d < 45 && Fibonacci.fib (d + 2) <= s

/// True if rope is balanced horizontally. False otherwise.
let isBalancedH = function
    | Node (s, d, _, w, _, _, _, _) -> isBalanced d w
    | _ -> true

/// True if rope is balanced vertically. False otherwise.
let isBalancedV = function
    | Node (s, d, h, _, _, _, _, _) -> isBalanced d h
    | _ -> true

/// Calls <code>f</code> recursively on a list of quad ropes until the
/// list is reduced to a single quad rope.
let rec private reduceList f = function
    | []  -> Empty
    | [n] -> n
    | ns  -> reduceList f (f ns)

/// Rebuild one level of the original quad rope using
/// <code>merge</code> to merge adjacent quad ropes.
let rec private rebuild merge nodes =
    match nodes with
        | [] -> []
        | [x] -> [x]
        | x :: y :: nodes -> merge x y :: (rebuild merge nodes)

/// Balance rope horizontally.
let hbalance qr =
    let rec hbalance qr =
        if isBalancedH qr then
            qr
        else
            let rs = collect qr []
            reduceList (rebuild flatNode) rs
    and collect qr rs  =
        match qr with
            | Empty -> rs
            | Node (_, _, _, _, ne, nw, Empty, Empty) -> collect nw (collect ne rs)
            | Node (_, _, _, _, ne, nw, sw, se) ->
                node (hbalance ne) (hbalance nw) (hbalance sw) (hbalance se) :: rs
            | _ -> qr :: rs
    hbalance qr

/// Balance rope vertically.
let vbalance qr =
    let rec vbalance qr =
        if isBalancedV qr then
            qr
        else
            let rs = collect qr []
            reduceList (rebuild thinNode) rs
    and collect qr rs  =
        match qr with
            | Empty -> rs
            | Node (_, _, _, _, Empty, nw, sw, Empty) -> collect nw (collect sw rs)
            | Node (_, _, _, _, ne, nw, sw, se) ->
                node (vbalance ne) (vbalance nw) (vbalance sw) (vbalance se) :: rs
            | _ -> qr :: rs
    vbalance qr

/// Balancing after Boehm et al. It turns out that this is slightly
/// slower than reduce-rebuild balancing and the resulting tree is of
/// greater height.
module Boehm =
    let hbalance qr =
        let rec insert qr n = function
            | r0 :: r1 :: qrs when depth r1 <= n -> insert qr n (flatNode r1 r0 :: qrs)
            | r0 :: qrs when depth r0 <= n -> (flatNode r0 qr) :: qrs
            | qrs -> qr :: qrs
        let rec hbalance qr qrs =
            match qr with
                | Empty -> qrs
                | Node (_, _, _, _, ne, nw, Empty, Empty) when not (isBalancedH qr) ->
                    hbalance ne (hbalance nw qrs)
                | _ ->
                    insert qr (Fibonacci.nth (cols qr)) qrs
        if isBalancedH qr then
            qr
        else
            List.reduce (fun ne nw -> flatNode nw ne) (hbalance qr [])

    let vbalance qr =
        let rec insert qr n = function
            | r0 :: r1 :: qrs when depth r1 <= n -> insert qr n (thinNode r1 r0 :: qrs)
            | r0 :: qrs when depth r0 <= n -> (thinNode r0 qr) :: qrs
            | qrs -> qr :: qrs
        let rec vbalance qr qrs =
            match qr with
                | Empty -> qrs
                | Node (_, _, _, _, Empty, nw, sw, Empty) when not (isBalancedH qr) ->
                    vbalance sw (vbalance nw qrs)
                | _ ->
                    insert qr (Fibonacci.nth (rows qr)) qrs
        if isBalancedV qr then
            qr
        else
            List.reduce (fun sw nw -> thinNode nw sw) (vbalance qr [])

/// Compute the "subrope" starting from indexes i, j taking h and w
/// elements in vertical and horizontal direction.
let slice i j h w qr =
    // Negative indices are not allowed, otherwise clients could
    // extend the slice arbitrarily.
    let i0 = max 0 i
    let j0 = max 0 j
    let h0 = min (rows qr - i0) (max 0 h)
    let w0 = min (cols qr - j0) (max 0 w)
    match qr with
        // Slicing on Empty has no effect.
        | Empty -> Empty
        // Index domain of quad rope is not inside bounds.
        | _ when rows qr = i0 || cols qr = j0 || h0 <= 0 || w0 <= 0 ->
            Empty
        // Index domain of quad rope is entirely inside bounds.
        | _ when i0 = 0 && rows qr <= h0 && j0 = 0 && cols qr <= w0 ->
            qr
        // Leaves are sliced on ArraySlice level, saves one level of
        // indirection.
        | Leaf vals ->
            leaf (ArraySlice.slice i0 j0 h0 w0 vals)
        // Avoid directly nested slices, unpack the original slice.
        | Slice (i1, j1, _, _, qr) ->
            Slice (i0 + i1, j0 + j1, h0, w0, qr)
        // Just re-size the sparse quad rope.
        | Sparse (_, _, v) ->
            Sparse (h0, w0, v)
        // Just initialize a new slice.
        | _ ->
            Slice (i0, j0, h0, w0, qr)

/// Split rope vertically from row i, taking h rows.
let inline vsplit i h qr =
    slice i 0 h (cols qr) qr

/// Split rope horizontally from column j, taking w columns.
let inline hsplit j w qr =
    slice 0 j (rows qr) w qr

/// Split rope in two at row i.
let inline vsplit2 qr i =
    vsplit 0 i qr, vsplit i (rows qr) qr

/// Split rope in two at column j.
let inline hsplit2 qr j =
    hsplit 0 j qr, hsplit j (cols qr) qr

// Get a single row or column
let row qr i = slice i 0 1 (cols qr) qr
let col qr j = slice 0 j (rows qr) 1 qr

// Get a sequence of rows or columns
let toRows qr = Seq.init (rows qr) (row qr)
let toCols qr = Seq.init (cols qr) (col qr)

/// Materialize a quad rope slice, i.e. traverse the slice and
/// allocate new quad rope nodes and new slices on arrays. Does not
/// allocate new arrays.
let materialize qr =
    let rec materialize i j h w qr =
        match qr with
            | _ when i <= 0 && j <= 0 && rows qr <= h && cols qr <= w ->
                qr
            | _ when rows qr <= i || cols qr <= j || h <= 0 || w <= 0 ->
                Empty
            | Empty ->
                Empty
            | Leaf vs ->
                leaf (ArraySlice.slice i j h w vs)
            | Node (_, _, _, _, ne, nw, sw, se) ->
                let nw0 = materialize i j h w nw
                let ne0 = materialize i (j - cols nw) h (w - cols nw0) ne
                let sw0 = materialize (i - rows nw) j (h - rows nw0) w sw
                let se0 = materialize (i - rows ne) (j - cols sw) (h - rows ne0) (w - cols sw0) se
                node ne0 nw0 sw0 se0
            | Slice (x, y, r, c, qr) ->
                materialize (i + x) (j + y) (min r h) (min c w) qr
            | Sparse (h', w', v) ->
                Sparse (min h h', min w w', v)
    match qr with
        | Slice (i, j, h, w, qr) -> materialize i j h w qr
        | qr -> qr

/// Concatenate two trees vertically. For the sake of leave size, this
/// may result in actually keeping parts of a large area in memory
/// twice. TODO: Consider other options.
let vcat upper lower =
    let canMerge us ls =
        ArraySlice.length2 us = ArraySlice.length2 ls &&
        ArraySlice.length1 us + ArraySlice.length1 ls <= s_max
    let rec vcat upper lower =
        match upper, lower with
            // Concatenation with Empty yields argument.
            | Empty, _ -> lower
            | _, Empty -> upper

            // Merge single leaves.
            | Leaf us, Leaf ls when canMerge us ls ->
                leaf (ArraySlice.cat1 us ls)

            // Merge sub-leaves.
            | Node (s, _, _, _, Empty, nwu, Leaf swus, Empty), Leaf ls when canMerge swus ls->
                thinNode nwu (leaf (ArraySlice.cat1 swus ls))
            | Leaf us, Node (_, _, _, _, Empty, Leaf nwls, swl, Empty) when canMerge us nwls ->
                thinNode (leaf (ArraySlice.cat1 us nwls)) swl

            // Merge upper southern and lower northern children.
            | (Node (_, _, _, _, neu, nwu, Leaf swus, Leaf seus),
               Node (_, _, _, _, Leaf nels, Leaf nwls, Empty, Empty))
                when canMerge swus nwls && canMerge seus nels ->
                    let sw = leaf (ArraySlice.cat1 swus nwls)
                    let se = leaf (ArraySlice.cat1 seus nels)
                    node neu nwu sw se
            | (Node (_, _, _, _, Leaf neus, Leaf nwus, Empty, Empty),
               Node (_, _, _, _, Leaf nels, Leaf nwls, swl, sel))
                when canMerge nwus nwls && canMerge neus nels ->
                    let nw = leaf (ArraySlice.cat1 nwus nwls)
                    let ne = leaf (ArraySlice.cat1 neus nels)
                    node ne nw swl sel

            // Merge flat nodes.
            | (Node (_, _, _, _, neu, nwu, Empty, Empty),
               Node (_, _, _, _, nel, nwl, Empty, Empty)) ->
                node neu nwu nwl nel

            // Merge sparse nodes of same element.
            | Sparse (h1, w, v1), Sparse (h2, _, v2) when v1 = v2 ->
                Sparse (h1 + h2, w, v1)

            // Create a new node pointing to arguments.
            | _ -> thinNode upper lower

    if (not ((isEmpty upper) || (isEmpty lower))) && cols upper <> cols lower then
        invalidArg "lower" "Lower quad rope must be of same width as upper quad rope."
    else
        vbalance (vcat upper lower)

/// Concatenate two trees horizontally. For the sake of leave size,
/// this may result in actually keeping parts of a large area in
/// memory twice. TODO: Consider other options.
let hcat left right =
    let canMerge ls rs =
        ArraySlice.rows ls = ArraySlice.rows rs && ArraySlice.cols ls + ArraySlice.cols rs <= s_max
    let rec hcat left right =
        match left, right with
            // Concatenation with Empty yields argument.
            | Empty, _ -> right
            | _, Empty -> left

            // Merge single leaves.
            | Leaf ls, Leaf rs when canMerge ls rs ->
                leaf (ArraySlice.cat2 ls rs)

            // Merge sub-leaves.
            | Node (s, _, _, _, Leaf lnes, lnw, Empty, Empty), Leaf rs when canMerge lnes rs->
                flatNode lnw (leaf (ArraySlice.cat2 lnes rs))
            | Leaf ls, Node (_, _, _, _, rne, Leaf rnws, Empty, Empty) when canMerge ls rnws ->
                flatNode (leaf (ArraySlice.cat2 ls rnws)) rne

            // Merge left western and right eastern children.
            | (Node (_, _, _, _, Leaf lnes, lnw, lsw, Leaf lses),
               Node (_, _, _, _, Empty, Leaf rnws, Leaf rsws, Empty))
                when canMerge lnes rnws && canMerge lses rsws ->
                    let ne = leaf (ArraySlice.cat2 lnes rnws)
                    let se = leaf (ArraySlice.cat2 lses rsws)
                    node ne lnw lsw se
            | (Node (_, _, _, _, Empty, Leaf lnws, Leaf lsws, Empty),
               Node (_, _, _, _, rne, Leaf rnws, Leaf rsws, rse))
                when canMerge lnws rnws && canMerge lsws rsws ->
                    let nw = leaf (ArraySlice.cat2 lnws rnws)
                    let sw = leaf (ArraySlice.cat2 lsws rsws)
                    node rne nw sw rse

            // Merge thin nodes.
            | (Node (_, _, _, _, Empty, lnw, lsw, Empty),
               Node (_, _, _, _, Empty, rnw, rsw, Empty)) ->
                node rnw lnw lsw rsw

            // Merge sparse nodes of same element.
            | Sparse (h, w1, v1), Sparse (_, w2, v2) when v1 = v2 ->
                Sparse (h, w1 + w2, v1)

            // Create a new node pointing to arguments.
            | _ -> flatNode left right

    if (not ((isEmpty left) || (isEmpty right))) && rows left <> rows right then
        invalidArg "right" "Right quad rope must be of same height as left quad rope."
    else
        hbalance (hcat left right)

/// Reverse rope horizontally.
let hrev qr =
    let rec hrev qr tgt =
        match qr with
            | Leaf slc ->
                leaf (Target.hrev slc tgt)
            | Node (s, d, h, w, Empty, nw, sw, Empty) ->
                Node (s, d, h, w,
                      Empty,
                      hrev nw tgt,
                      hrev sw (Target.sw tgt qr),
                      Empty)
            | Node (s, d, h, w, ne, nw, sw, se) ->
                Node (s, d, h, w,
                      hrev nw tgt,
                      hrev ne (Target.ne tgt qr),
                      hrev se (Target.se tgt qr),
                      hrev sw (Target.sw tgt qr))
            | Slice _ ->
                hrev (materialize qr) tgt
            | _ -> qr
    hrev qr (Target.make (rows qr) (cols qr))

/// Reverse rope vertically.
let vrev qr =
    let rec vrev qr tgt =
        match qr with
            | Leaf slc ->
                leaf (Target.vrev slc tgt)
            | Node (s, d, h, w, ne, nw, Empty, Empty) ->
                Node (s, d, h, w,
                      vrev ne (Target.ne tgt qr),
                      vrev nw tgt,
                      Empty,
                      Empty)
            | Node (s, d, h, w, ne, nw, sw, se) ->
                Node (s, d, h, w,
                      vrev se (Target.se tgt qr),
                      vrev sw (Target.sw tgt qr),
                      vrev nw tgt,
                      vrev ne (Target.ne tgt qr))
            | Slice _ ->
                vrev (materialize qr) tgt
            | _ -> qr
    vrev qr (Target.make (rows qr) (cols qr))

/// Create a quad rope from an array slice instance.
let rec internal fromArraySlice slc =
    if ArraySlice.rows slc <= 0 || ArraySlice.cols slc <= 0 then
        Empty
    else if ArraySlice.rows slc <= s_max && ArraySlice.cols slc <= s_max then
        leaf slc
    else if ArraySlice.cols slc <= s_max then
        let n, s = ArraySlice.vsplit2 slc
        thinNode (fromArraySlice n) (fromArraySlice s)
    else if ArraySlice.rows slc <= s_max then
        let w, e = ArraySlice.hsplit2 slc
        flatNode (fromArraySlice w) (fromArraySlice e)
    else
        let ne, nw, sw, se = ArraySlice.split4 slc
        node (fromArraySlice ne) (fromArraySlice nw) (fromArraySlice sw) (fromArraySlice se)

/// Initialize a rope from a native 2D-array.
let fromArray2D arr =
    fromArraySlice (ArraySlice.make arr)

/// Generate a new tree without any intermediate values.
let inline init h w f =
    fromArray2D (Array2D.init h w f)

/// Initialize a rope where all elements are <code>e</code>.
let inline create h w v =
    Sparse (h, w, v)

/// Generate a singleton quad rope.
let inline singleton v =
    create 1 1 v

/// Initialize a rope with all zeros.
let inline createZeros h w = create h w 0

/// True if rope is a singleton, false otherwise.
let isSingleton qr =
    rows qr = 1 && cols qr = 1

/// Initialize a rope from a native array for a given width.
let fromArray vs w =
    if Array.length vs % w <> 0 then
        invalidArg "w" "Must be evenly divisible by array length."
    let h = Array.length vs / w
    init h w (fun i j -> vs.[i * w + j])

/// Apply a function with side effects to all elements of the rope.
let rec iter f = function
    | Empty -> ()
    | Leaf vs -> ArraySlice.iter f vs
    | Node (s, _, _, _, ne, nw, sw, se) ->
        iter f ne
        iter f nw
        iter f sw
        iter f se
    | Slice _ as qr -> iter f (materialize qr)
    | Sparse (h, w, v) ->
        for i in 1 .. h * w - 1 do
            f v

/// Apply a function with side effects to all elements and their
/// corresponding index pair.
let iteri f qr =
    let rec iteri f i j = function
        | Empty -> ()
        | Leaf vs -> ArraySlice.iteri (fun i0 j0 v -> f (i + i0) (j + j0) v) vs
        | Node (s, _, _, _, ne, nw, sw, se) ->
            iteri f i j nw
            iteri f i (j + cols nw) ne
            iteri f (i + rows nw) j sw
            iteri f (i + rows ne) (j + cols sw) se
        | Slice _ as qr -> iteri f i j (materialize qr)
        | Sparse (h, w, v) ->
            for r in i .. i + h - 1 do
                for c in j .. j + w - 1 do
                    f r c v
    iteri f 0 0 qr

/// Conversion into 1D array.
let toArray qr =
    let arr = Array.zeroCreate (rows qr * cols qr)
    // This avoids repeated calls to get; it is enough to traverse
    // the rope once.
    iteri (fun i j v -> arr.[i * cols qr + j] <- v) qr
    arr

let toRowsArray qr =
    toRows qr |> Seq.map toArray |> Seq.toArray

let toColsArray qr =
    toCols qr |> Seq.map toArray |> Seq.toArray

/// Conversion into 2D array.
let toArray2D qr =
    let arr = Array2D.zeroCreate (rows qr) (cols qr)
    // This avoids repeated calls to get; it is enough to traverse
    // the rope once.
    iteri (fun i j v -> arr.[i, j] <- v) qr
    arr

/// Reallocate a rope form the ground up. Sometimes, this is the
/// only way to improve performance of a badly composed quad rope.
let inline reallocate qr =
    fromArray2D (toArray2D qr)

/// Apply a function to every element in the tree.
let map f qr =
    let rec map qr tgt =
        match qr with
            | Empty ->
                Empty
            // Initialize target as soon as quad rope is dense.
            | _ when not (isSparse qr) && Target.isEmpty tgt ->
                map qr (Target.make (rows qr) (cols qr))
            // Write into target and construct new leaf.
            | Leaf slc ->
                Leaf (Target.map f slc tgt)
            // Node recursive case, adjust target descriptor.
            | Node (s, d, h, w, ne, nw, sw, se) ->
                Node (s, d, h, w,
                      map ne (Target.ne tgt qr),
                      map nw tgt, // No need to adjust target.
                      map sw (Target.sw tgt qr),
                      map se (Target.se tgt qr))
            // Materialize quad rope and then map.
            | Slice _ ->
                map (materialize qr) tgt
            // The target is empty, don't write.
            | Sparse (h, w, v) ->
                Sparse (h, w, f v)
    map qr Target.empty

/// Map a function f to each row of the quad rope.
let hmap f qr =
    init (rows qr) 1 (fun i _ -> slice i 0 1 (cols qr) qr |> f)

/// Map a function f to each column of the quad rope.
let vmap f qr =
    init 1 (cols qr) (fun _ j -> slice 0 j (rows qr) 1 qr |> f)

/// Slice b as to match the sub-shapes of a. If a is empty, the
/// resulting slices will all be empty. If a is not a Node, all slices
/// except for the NW slice will be empty.
let internal sliceToMatch a b =
    match a with
        | Empty -> Empty, Empty, Empty, Empty
        | Node (_, _, _, _, ne, nw, sw, se) ->
            slice 0         (cols nw) (rows ne) (cols ne) b,
            slice 0          0        (rows nw) (cols nw) b,
            slice (rows nw)  0        (rows sw) (cols sw) b,
            slice (rows ne) (cols sw) (rows se) (cols se) b
        | _ -> Empty, slice 0 0 (rows a) (cols b) b, Empty, Empty

/// Zip implementation for the general case where we do not assume
/// that both ropes have the same internal structure.
let rec internal genZip f lqr rqr tgt =
    match lqr with
        | Empty -> Empty
        // Initialize target if any of the two quad ropes is dense.
        | _ when Target.isEmpty tgt && not (isSparse lqr) ->
            genZip f lqr rqr (Target.make (rows lqr) (cols lqr))
        | Leaf slc ->
            let rqr = materialize rqr
            leaf (Target.mapi (fun i j v -> f v (fastGet rqr i j)) slc tgt)
        | Node (s, d, h, w, ne, nw, sw, se) ->
            let rne, rnw, rsw, rse = sliceToMatch lqr rqr
            let nw0 = genZip f nw rnw tgt
            let ne0 = genZip f ne rne (Target.ne tgt lqr)
            let sw0 = genZip f sw rsw (Target.sw tgt lqr)
            let se0 = genZip f se rse (Target.se tgt lqr)
            Node (s, d, h, w, ne0, nw0, sw0, se0)
        | Slice _ -> genZip f (materialize lqr) rqr tgt
        | Sparse (_, _, v) -> map (f v) rqr

/// True if the shape of two ropes match.
let internal shapesMatch a b =
    rows a = rows b && cols a = cols b

/// True if a and b are nodes and the shapes of all their sub-ropes in
/// the same positions match. Not recursive, hence O(1) complexity.
let internal subShapesMatch a b =
    match a, b with
        | Node (_, _, _, _, ane, anw, asw, ase),
          Node (_, _, _, _, bne, bnw, bsw, bse) ->
            shapesMatch ane bne
            && shapesMatch anw bnw
            && shapesMatch asw bsw
            && shapesMatch ase bse
        | _ -> false

/// Zip function that assumes that the internal structure of two ropes
/// matches. If not, it falls back to the slow, general case.
let rec internal fastZip f lqr rqr tgt =
    match lqr, rqr with
        | Empty, Empty -> Empty
        // Initialize target if any of the two quad ropes is dense.
        | _ when Target.isEmpty tgt && (not (isSparse lqr) || not (isSparse rqr)) ->
            fastZip f lqr rqr (Target.make (rows lqr) (cols lqr))
        | Leaf ls, Leaf rs when shapesMatch lqr rqr ->
            leaf (Target.map2 f ls rs tgt)
        | Node (_, _, _, _, lne, lnw, lsw, lse), Node (_, _, _, _, rne, rnw, rsw, rse)
            when subShapesMatch lqr rqr ->
                node (fastZip f lne rne (Target.ne tgt lqr))
                     (fastZip f lnw rnw tgt)
                     (fastZip f lsw rsw (Target.sw tgt lqr))
                     (fastZip f lse rse (Target.se tgt lqr))
        // It may pay off to reallocate first if both reallocated quad
        // ropes have the same internal shape. This might be
        // over-fitted to matrix multiplication.
        | Slice _, Slice _ -> fastZip f (materialize lqr) (materialize rqr) tgt
        | Slice _, _ ->       fastZip f (materialize lqr) rqr tgt
        | Slice _, Slice _ -> fastZip f lqr (materialize rqr) tgt
        // Sparse branches can be reduced to either a single call to f
        // in case both arguments are sparse, or to a mapping f.
        | Sparse (h, w, v1), Sparse (_, _, v2) -> Sparse (h, w, f v1 v2)
        | Sparse (_, _, v), _ -> map (f v) rqr
        | _, Sparse (_, _, v) -> map (fun x -> f x v) lqr
        // Fall back to general case.
        | _ -> genZip f lqr rqr tgt

/// Apply f to each (i, j) of lqr and rope.
let zip f lqr rqr =
    if not (shapesMatch lqr rqr) then
        failwith "Quad ropes must have the same shape."
    fastZip f lqr rqr (Target.make (rows lqr) (cols lqr))

/// Apply f to all values of the rope and reduce the resulting values
/// to a single scalar using g. Variable epsilon is the neutral
/// element for g. We assume that g epsilon x = g x epsilon = x.
let rec mapreduce f g epsilon = function
    | Empty -> epsilon
    | Leaf slc ->
        ArraySlice.mapreduce f g slc
    | Node (_, _, _, _, ne, nw, Empty, Empty) ->
        g (mapreduce f g epsilon nw) (mapreduce f g epsilon ne)
    | Node (_, _, _, _, Empty, nw, sw, Empty) ->
        g (mapreduce f g epsilon nw) (mapreduce f g epsilon sw)
    | Node (_, _, _, _, ne, nw, sw, se) ->
        g (g (mapreduce f g epsilon nw) (mapreduce f g epsilon ne))
          (g (mapreduce f g epsilon sw) (mapreduce f g epsilon se))
    | Slice _ as qr ->
        mapreduce f g epsilon (materialize qr)
    | Sparse (h, w, v) ->
        let fv = f v
        if fv = epsilon then
            epsilon
        else
            let mutable acc = f v
            for i in 2 .. h * w do
                acc <- g acc fv
            acc

/// Reduce all values of the rope to a single scalar.
let reduce f epsilon qr = mapreduce id f epsilon qr

let hmapreduce f g epsilon qr = hmap (mapreduce f g epsilon) qr
let vmapreduce f g epsilon qr = vmap (mapreduce f g epsilon) qr

let hreduce f epsilon qr = hmapreduce id f epsilon qr
let vreduce f epsilon qr = vmapreduce id f epsilon qr

let inline private offset f x =
    ((+) x) >> f

/// Compute the row-wise prefix sum of the rope for f starting with
/// states.
let rec hscan f states = function
    | Empty -> Empty
    | Leaf vs -> Leaf (ArraySlice.scan2 f states vs)
    | Node (_, _, _, _, ne, nw, sw, se) ->
        let nw' = hscan f states nw
        let sw' = hscan f (offset states (rows nw')) sw
        (* NW and SW might differ in height and width, we cannot join them to a thin node. *)
        let estate i =
            if i < rows nw' then fastGet nw' i (cols nw' - 1) else fastGet sw' (i - rows nw') (cols sw' - 1)
        let ne' = hscan f estate ne
        let se' = hscan f (offset estate (rows ne')) se
        node ne' nw' sw' se'
    | Slice _ as qr -> hscan f states (materialize qr)

/// Compute the column-wise prefix sum of the rope for f starting
/// with states.
let rec vscan f states = function
    | Empty -> Empty
    | Leaf vs -> Leaf (ArraySlice.scan1 f states vs)
    | Node (_, _, _, _, ne, nw, sw, se) ->
        let nw' = vscan f states nw
        let ne' = vscan f (offset states (cols nw')) ne
        (* NW and NE might differ in height and width, we cannot join them to a flat node. *)
        let sstate j =
            if j < cols nw' then fastGet nw' (rows nw' - 1) j else fastGet ne' (rows ne' - 1) (j - cols nw')
        let sw' = vscan f sstate sw
        let se' = vscan f (offset sstate (cols sw')) se
        node ne' nw' sw' se'
    | Slice _ as qr -> vscan f states (materialize qr)

/// Compute the generalized summed area table for functions plus and
/// minus; all rows and columns are initialized with init.
let scan plus minus init qr =
    // Prefix is implicitly passed on through side effects when
    // writing into tgt.
    let rec scan pre qr tgt =
        match qr with
            | Empty -> Empty
            | Leaf slc ->
                Target.scan plus minus pre slc tgt
                Leaf (Target.toSlice tgt (ArraySlice.rows slc) (ArraySlice.cols slc))

            // Flat node.
            | Node (s, d, h, w, ne, nw, Empty, Empty) ->
                let nw' = scan pre nw tgt
                let tgt_ne = Target.ne tgt qr
                let ne' = scan (Target.get tgt_ne) ne tgt_ne
                Node (s, d, h, w, ne', nw', Empty, Empty)

            // Thin nodes.
            | Node (s, d, h, w, Empty, nw, sw, Empty) ->
                let nw' = scan pre nw tgt
                let tgt_sw = Target.sw tgt qr
                let sw' = scan (Target.get tgt_sw) sw tgt_sw
                Node (s, d, h, w, Empty, nw', sw', Empty)

            // SW depends on NE.
            | Node (s, d, h, w, ne, nw, sw, se) when cols nw < cols sw ->
                let nw' = scan pre nw tgt
                let tgt_ne = Target.ne tgt qr
                let ne' = scan (Target.get tgt_ne) ne tgt_ne
                let tgt_sw = Target.sw tgt qr
                let sw' = scan (Target.get tgt_sw) sw tgt_sw
                let tgt_se = Target.se tgt qr
                let se' = scan (Target.get tgt_se) se tgt_se
                Node (s, d, h, w, ne', nw', sw', se')

            // NE depends on SW.
            | Node (s, d, h, w, ne, nw, sw, se) -> // when rows nw < rows ne
                let nw' = scan pre nw tgt
                let tgt_sw = Target.sw tgt qr
                let sw' = scan (Target.get tgt_sw) sw tgt_sw
                let tgt_ne = Target.ne tgt qr
                let ne' = scan (Target.get tgt_ne) ne tgt_ne
                let tgt_se = Target.se tgt qr
                let se' = scan (Target.get tgt_se) se tgt_se
                Node (s, d, h, w, ne', nw', sw', se')

            | Slice _ -> scan pre (materialize qr) tgt

            | Sparse (h, w, v) ->
                // Fill the target imperatively.
                Target.fill tgt h w v
                // Get a slice over the target.
                let slc = Target.toSlice tgt h w
                // Compute prefix imperatively.
                Target.scan plus minus pre slc tgt
                // Make a new quad rope from array slice.
                fromArraySlice slc

    let tgt = Target.makeWithFringe (rows qr) (cols qr) init
    scan (Target.get tgt) qr tgt

/// Checks that some relation p holds between each two adjacent
/// elements in each row. This is slow and should not really be
/// used.
let forallRows p = function
    | Empty -> true
    | qr ->
        hmapreduce List.singleton List.append [] qr
        |> map List.pairwise
        |> mapreduce (List.forall (fun (a, b) -> p a b)) (&&) true

/// Checks that some relation p holds between each two adjacent
/// elements in each column. This is slow and should not really be
/// used.
let forallCols p = function
    | Empty -> true
    | qr ->
        vmapreduce List.singleton List.append [] qr
        |> map List.pairwise
        |> mapreduce (List.forall (fun (a, b) -> p a b)) (&&) true

/// Apply predicate p to all elements of qr and reduce the
/// elements in both dimension using logical and.
let forall p qr = mapreduce p (&&) true qr

/// Apply predicate p to all elements of qr and reduce the
/// elements in both dimensions using logical or.
let exists p qr = mapreduce p (||) false qr

/// Remove all elements from rope for which p does not hold. Input
/// rope must be of height 1.
let rec hfilter p = function
    | Empty -> Empty
    | Leaf vs -> leaf (ArraySlice.filter2 p vs)
    | Node (_, _, 1, _, ne, nw, Empty, Empty) ->
        flatNode (hfilter p nw) (hfilter p ne)
    | Sparse (1, _, v) as qr when p v -> qr
    | Sparse (1, _, _) -> Empty
    | _ -> failwith "Quad rope height must be exactly one."

/// Remove all elements from rope for which p does not hold. Input
/// rope must be of width 1.
let rec vfilter p = function
    | Empty -> Empty
    | Leaf vs -> leaf (ArraySlice.filter1 p vs)
    | Node (_, _, _, 1, Empty, nw, sw, Empty) ->
        thinNode (vfilter p nw) (vfilter p sw)
    | Sparse (_, 1, v) as qr when p v -> qr
    | Sparse (_, 1, _) -> Empty
    | _ -> failwith "Quad rope width must be exactly one."

/// Transpose the quad rope. This is equal to swapping indices,
/// such that get qr i j = get (transpose qr) j i.
let transpose qr =
    let rec transpose qr tgt =
        match qr with
            | Empty -> Empty
            | Leaf slc ->
                leaf (Target.transpose slc tgt)
            | Node (_, _, _, _, ne, nw, sw, se) ->
                node (transpose sw (Target.sw tgt qr))
                     (transpose nw tgt)
                     (transpose ne (Target.ne tgt qr))
                     (transpose se (Target.se tgt qr))
            | Slice _ ->
                transpose (materialize qr) tgt
            | Sparse (h, w, v) -> Sparse (w, h, v)
    transpose qr (Target.make (cols qr) (rows qr))

/// Produce a string with the tikz code for printing the rope as a
/// box diagram. This is useful for illustrating algorithms on
/// quad ropes.
let tikzify h w qr =
    let line i0 j0 i1 j1 =
        sprintf "\\draw (%f, %f) -- (%f, %f);" j0 i0 j1 i1
    let thinLine i0 j0 i1 j1 =
        sprintf "\\draw [very thin] (%f, %f) -- (%f, %f);" j0 i0 j1 i1
    let rect i j h w =
        sprintf "\\draw (%f, %f) rectangle (%f, %f);" j i (j + w) (i + h)
    let box i j h w =
        sprintf "\\fill[gray!20!white] (%f, %f) rectangle (%f, %f);" j i (j + w) (i + h)
    let rec tikz i j h w = function
        | Empty -> seq { yield box i j h w; yield thinLine i j (h + i) (w + j) }
        | Leaf _ -> Seq.empty
        | Node (s, _, _, _, ne, nw, sw, se) ->
            let h0 = h / 2.0
            let w0 = w / 2.0
            seq { yield! tikz i j h0 w0 sw
                  yield! tikz (i + h0) j h0 w0 nw
                  yield! tikz i (j + w0) h0 w0 se
                  yield! tikz (i + h0) (j + w0) h0 w0 ne
                  yield line i (j + w0) (i + h) (j + w0)
                  yield line (i + h0) j (i + h0) (j + w) }
        | Slice _ as qr -> tikz i j h w (materialize qr)
    let cmds = List.ofSeq (seq { yield! tikz 0.0 0.0 h w qr; yield rect 0.0 0.0 h w });
    printfn "%s" (String.concat "\n" cmds)
