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

/// Construct a Leaf if slc is non-empty. Otherwise, return Empty.
let leaf slc =
    if ArraySlice.length1 slc = 0 || ArraySlice.length2 slc = 0 then
        Empty
    else
        Leaf slc

/// Pseudo-constructor for generating a new rope out of some
/// existing nodes.
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
            Node (d, h, w, ne, nw, sw, se)

let inline flatNode w e =
    node e w Empty Empty (* NB: Arguments switched. *)

let inline thinNode n s =
    node Empty n s Empty

let inline private withinRange root i j =
    0 <= i && i < rows root && 0 <= j && j < cols root

let inline private checkBounds qr i j =
    if rows qr <= i then
        invalidArg "i" (sprintf "First index must be within bounds of quad rope: %d" i)
    if cols qr <= j then
        invalidArg "j" (sprintf "Second index must be within bounds of quad rope: %d" j)

/// Get the value of a location in the tree. This function does not
/// check whether i, j are within bounds.
let rec internal fastGet qr i j =
    match qr with
        | Empty -> invalidArg "qr" "Empty tree cannot contain values."
        | Leaf vs -> ArraySlice.get vs i j
        | Node (_, _, _, ne, nw, sw, se) ->
            if withinRange nw i j then
                fastGet nw i j
            else if withinRange ne i (j - cols nw) then
                fastGet ne i (j - cols nw)
            else if withinRange sw (i - rows nw) j then
                fastGet sw (i - rows nw) j
            else
                fastGet se (i - rows ne) (j - cols sw)
        | Slice (x, y, _, _, qr) -> fastGet qr (i + x) (j + y)

/// Get the value of a location in the tree.
let get root i j =
    checkBounds root i j
    fastGet root i j

/// Update a tree location without modifying the original tree.
let set root i j v =
    let rec set qr i j v =
        match qr with
            | Empty -> invalidArg "qr" "Empty tree cannot contain values."
            | Leaf vs -> Leaf (ArraySlice.set vs i j v)
            | Node (d, h, w, ne, nw, sw, se) ->
                if withinRange nw i j then
                    Node (d, h, w, ne, set nw i j v, sw, se)
                else if withinRange ne i (j - cols nw) then
                    Node (d, h, w, set ne i (j - cols nw) v, nw, sw, se)
                else if withinRange sw (i - rows nw) j then
                    Node (d, h, w, ne, nw, set sw (i - rows nw) j v, se)
                else
                    Node (d, h, w, ne, nw, sw, set se (i - rows ne) (j - cols sw) v)
            | Slice (x, y, h, w, qr) -> Slice (x, y, h, w, set qr (i + x) (j + y) v)
    checkBounds root i j
    set root i j v

/// Write to a tree location destructively.
let write root i j v =
    let rec write qr i j v =
        match qr with
            | Empty -> invalidArg "qr" "Empty tree cannot contain values."
            | Leaf vs -> Leaf (ArraySlice.set vs i j v)
            | Node (_, _, _, ne, nw, sw, se) ->
                if withinRange nw i j then
                    write nw i j v
                else if withinRange ne i (j - cols nw) then
                    write ne i (j - cols nw) v
                else if withinRange sw (i - rows nw) j then
                    write sw (i - rows nw) j v
                else
                    write se (i - rows ne) (j - cols sw) v
            | Slice (x, y, _, _, qr) -> write qr (x + i) (y + j) v
    checkBounds root i j
    write root i j v

let private isBalanced d s =
    d < 45 && Fibonacci.fib (d + 2) <= s

/// True if rope is balanced horizontally. False otherwise.
let isBalancedH = function
    | Node (d, _, w, _, _, _, _) -> isBalanced d w
    | _ -> true

/// True if rope is balanced vertically. False otherwise.
let isBalancedV = function
    | Node (d, h, _, _, _, _, _) -> isBalanced d h
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
            | Node (_, _, _, ne, nw, Empty, Empty) -> collect nw (collect ne rs)
            | Node (_, _, _, ne, nw, sw, se) ->
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
            | Node (_, _, _, Empty, nw, sw, Empty) -> collect nw (collect sw rs)
            | Node (_, _, _, ne, nw, sw, se) ->
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
                | Node (_, _, _, ne, nw, Empty, Empty) when not (isBalancedH qr) ->
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
                | Node (_, _, _, Empty, nw, sw, Empty) when not (isBalancedH qr) ->
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

let row qr i = slice i 0 1 (cols qr) qr
let col qr j = slice 0 j (rows qr) 1 qr

let toRows qr =
    Seq.init (rows qr) (row qr)

let toCols qr =
    Seq.init (cols qr) (col qr)

module internal Slicing =

    /// Auxiliary function to recursively slice a tree structure.
    let rec private reallocate0 i j h w qr =
        match qr with
            | _ when i <= 0 && j <= 0 && rows qr <= h && cols qr <= w ->
                qr
            | _ when rows qr <= i || cols qr <= j || h <= 0 || w <= 0 ->
                Empty
            | Empty ->
                Empty
            | Leaf vs ->
                leaf (ArraySlice.slice i j h w vs)
            | Node (_, _, _, ne, nw, sw, se) ->
                let nw0 = reallocate0 i j h w nw
                let ne0 = reallocate0 i (j - cols nw) h (w - cols nw0) ne
                let sw0 = reallocate0 (i - rows nw) j (h - rows nw0) w sw
                let se0 = reallocate0 (i - rows ne) (j - cols sw) (h - rows ne0) (w - cols sw0) se
                node ne0 nw0 sw0 se0
            | Slice (x, y, r, c, qr) ->
                reallocate0 (i + x) (j + y) (min r h) (min c w) qr

    /// Actually compute a slice.
    let reallocate = function
        | Slice (i, j, h, w, qr) -> reallocate0 i j h w qr
        | qr -> qr

    /// Compute a slice and map in the same traversal.
    let map f qr (arr : _ [,]) =
        let rec map i j h w qr =
            match qr with
                | _ when rows qr <= i || cols qr <= j || h <= 0 || w <= 0 -> Empty
                | Empty -> Empty
                | Leaf vs ->
                    ArraySlice.iteri (fun x y e -> arr.[x + i, y + j] <- f e) (ArraySlice.slice i j h w vs)
                    leaf (ArraySlice.makeSlice i j h w arr)
                | Node (_, _, _, ne, nw, sw, se) ->
                    let nw0 = map i j h w nw
                    let ne0 = map i (j - cols nw) h (w - cols nw0) ne
                    let sw0 = map (i - rows nw) j (h - rows nw0) w sw
                    let se0 = map (i - rows ne) (j - cols sw) (h - rows ne0) (w - cols sw0) se
                    node ne0 nw0 sw0 se0
                | Slice (x, y, r, c, qr) -> map (x + i) (y + j) (min h r) (min w c) qr
        map 0 0 (rows qr) (cols qr) qr

/// Concatenate two trees vertically.
let vcat upper lower =
    let canMerge us ls =
        ArraySlice.length2 us = ArraySlice.length2 ls &&
        ArraySlice.length1 us + ArraySlice.length1 ls <= s_max
    let rec vcat upper lower =
        match upper, lower with
            // Concatenation with Empty yields argument.
            | Empty, _ -> lower
            | _, Empty -> upper

            // Slices must be reallocated. TODO: Reconsider.
            | Slice _, _ -> vcat (Slicing.reallocate upper) lower
            | _, Slice _ -> vcat upper (Slicing.reallocate lower)

            // Merge single leaves.
            | Leaf us, Leaf ls when canMerge us ls ->
                leaf (ArraySlice.cat1 us ls)

            // Merge sub-leaves.
            | Node (_, _, _, Empty, nwu, Leaf swus, Empty), Leaf ls when canMerge swus ls->
                thinNode nwu (leaf (ArraySlice.cat1 swus ls))
            | Leaf us, Node (_, _, _, Empty, Leaf nwls, swl, Empty) when canMerge us nwls ->
                thinNode (leaf (ArraySlice.cat1 us nwls)) swl

            // Merge upper southern and lower northern children.
            | (Node (_, _, _, neu, nwu, Leaf swus, Leaf seus),
               Node (_, _, _, Leaf nels, Leaf nwls, Empty, Empty))
                when canMerge swus nwls && canMerge seus nels ->
                    let sw = leaf (ArraySlice.cat1 swus nwls)
                    let se = leaf (ArraySlice.cat1 seus nels)
                    node neu nwu sw se
            | (Node (_, _, _, Leaf neus, Leaf nwus, Empty, Empty),
               Node (_, _, _, Leaf nels, Leaf nwls, swl, sel))
                when canMerge nwus nwls && canMerge neus nels ->
                    let nw = leaf (ArraySlice.cat1 nwus nwls)
                    let ne = leaf (ArraySlice.cat1 neus nels)
                    node ne nw swl sel

            // Merge flat nodes.
            | (Node (_, _, _, neu, nwu, Empty, Empty),
               Node (_, _, _, nel, nwl, Empty, Empty)) ->
                node neu nwu nwl nel

            // Create a new node pointing to arguments.
            | _ -> thinNode upper lower

    if (not ((isEmpty upper) || (isEmpty lower))) && cols upper <> cols lower then
        invalidArg "lower" "Lower quad rope must be of same width as upper quad rope."
    else
        vbalance (vcat upper lower)

/// Concatenate two trees horizontally.
let hcat left right =
    let canMerge ls rs =
        ArraySlice.length1 ls = ArraySlice.length1 rs &&
        ArraySlice.length2 ls + ArraySlice.length2 rs <= s_max
    let rec hcat left right =
        match left, right with
            // Concatenation with Empty yields argument.
            | Empty, _ -> right
            | _, Empty -> left

            // Slices must be reallocated. TODO: Reconsider.
            | Slice _, _ -> hcat (Slicing.reallocate left) right
            | _, Slice _ -> hcat left (Slicing.reallocate right)

            // Merge single leaves.
            | Leaf ls, Leaf rs when canMerge ls rs ->
                leaf (ArraySlice.cat2 ls rs)

            // Merge sub-leaves.
            | Node (_, _, _, Leaf lnes, lnw, Empty, Empty), Leaf rs when canMerge lnes rs->
                flatNode lnw (leaf (ArraySlice.cat2 lnes rs))
            | Leaf ls, Node (_, _, _, rne, Leaf rnws, Empty, Empty) when canMerge ls rnws ->
                flatNode (leaf (ArraySlice.cat2 ls rnws)) rne

            // Merge left western and right eastern children.
            | (Node (_, _, _, Leaf lnes, lnw, lsw, Leaf lses),
               Node (_, _, _, Empty, Leaf rnws, Leaf rsws, Empty))
                when canMerge lnes rnws && canMerge lses rsws ->
                    let ne = leaf (ArraySlice.cat2 lnes rnws)
                    let se = leaf (ArraySlice.cat2 lses rsws)
                    node ne lnw lsw se
            | (Node (_, _, _, Empty, Leaf lnws, Leaf lsws, Empty),
               Node (_, _, _, rne, Leaf rnws, Leaf rsws, rse))
                when canMerge lnws rnws && canMerge lsws rsws ->
                    let nw = leaf (ArraySlice.cat2 lnws rnws)
                    let sw = leaf (ArraySlice.cat2 lsws rsws)
                    node rne nw sw rse

            // Merge thin nodes.
            | (Node (_, _, _, Empty, lnw, lsw, Empty),
               Node (_, _, _, Empty, rnw, rsw, Empty)) ->
                node rnw lnw lsw rsw

            // Create a new node pointing to arguments.
            | _ -> flatNode left right

    if (not ((isEmpty left) || (isEmpty right))) && rows left <> rows right then
        invalidArg "right" "Right quad rope must be of same width as left quad rope."
    else
        hbalance (hcat left right)

/// Reverse rope horizontally.
let rec hrev = function
    | Empty -> Empty
    | Leaf vs -> Leaf (ArraySlice.rev2 vs)
    | Node (d, h, w, Empty, nw, sw, Empty) ->
        Node (d, h, w, Empty, hrev nw, hrev sw, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        Node (d, h, w, hrev nw, hrev ne, hrev se, hrev sw)
    | Slice (i, j, h, w, qr) -> Slice (i, j, h, w, hrev qr)

/// Reverse rope vertically.
let rec vrev = function
    | Empty -> Empty
    | Leaf vs -> Leaf (ArraySlice.rev1 vs)
    | Node (d, h, w, ne, nw, Empty, Empty) ->
        Node (d, h, w, vrev ne, vrev nw, Empty, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        Node (d, h, w, vrev se, vrev sw, vrev nw, vrev ne)
    | Slice (i, j, h, w, qr) -> Slice (i, j, h, w, vrev qr)

/// Initialize a rope from a native 2D-array.
let fromArray2D arr =
    let rec init h0 w0 h1 w1 arr =
        let h = h1 - h0
        let w = w1 - w0
        if h <= 0 || w <= 0 then
            Empty
        else if h <= s_max && w <= s_max then
            leaf (ArraySlice.makeSlice h0 w0 h w arr)
        else if w <= s_max then
            let hpv = h0 + (h >>> 1)
            thinNode (init h0 w0 hpv w1 arr) (init hpv w0 h1 w1 arr)
        else if h <= s_max then
            let wpv = w0 + (w >>> 1)
            flatNode (init h0 w0 h1 wpv arr) (init h0 wpv h1 w1 arr)
        else
            let hpv = h0 + (h >>> 1)
            let wpv = w0 + (w >>> 1)
            node (init h0 wpv hpv w1 arr)
                 (init h0 w0 hpv wpv arr)
                 (init hpv w0 h1 wpv arr)
                 (init hpv wpv h1 w1 arr)
    init 0 0 (Array2D.length1 arr) (Array2D.length2 arr) arr

/// Generate a new tree without any intermediate values.
let inline init h w f =
    fromArray2D (Array2D.init h w f)

/// Initialize a rope where all elements are <code>e</code>.
let inline create h w e =
    init h w (fun _ _ -> e)

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
    | Node (_, _, _, ne, nw, sw, se) ->
        iter f ne
        iter f nw
        iter f sw
        iter f se
    | Slice _ as qr -> iter f (Slicing.reallocate qr)

/// Apply a function with side effects to all elements and their
/// corresponding index pair.
let iteri f qr =
    let rec iteri f i j = function
        | Empty -> ()
        | Leaf vs -> ArraySlice.iteri (fun i0 j0 v -> f (i + i0) (j + j0) v) vs
        | Node (_, _, _, ne, nw, sw, se) ->
            iteri f i j nw
            iteri f i (j + cols nw) ne
            iteri f (i + rows nw) j sw
            iteri f (i + rows ne) (j + cols sw) se
        | Slice _ as qr -> iteri f i j (Slicing.reallocate qr)
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
            | Leaf vals ->
                Leaf (Target.map f vals tgt)
            | Node (d, h, w, ne, nw, sw, se) ->
                Node (d, h, w,
                      map ne (Target.ne tgt qr),
                      map nw tgt, // No need to adjust target.
                      map sw (Target.sw tgt qr),
                      map se (Target.se tgt qr))
            | Slice _ ->
                map (Slicing.reallocate qr) tgt
    map qr (Target.make (rows qr) (cols qr))

/// Fold each row of rope with f, starting with the according
/// state in states.
let hfold f states qr =
    if rows states <> rows qr then
        invalidArg "states" "Must have the same height as qr."
    let rec fold1 states = function
        | Empty -> states
        | Leaf vs -> leaf (ArraySlice.fold2 f (fun i -> fastGet states i 0) vs)
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw sw) ne se
        | Slice _ as qr -> fold1 states (Slicing.reallocate qr)
    and fold2 states n s =
        let nstates, sstates = vsplit2 states (rows n)
        thinNode (fold1 nstates n) (fold1 sstates s)
    fold1 states qr

/// Fold each column of rope with f, starting with the according
/// state in states.
let vfold f states qr =
    if cols states <> cols qr then
        invalidArg "states" "Must have the same width as qr."
    let rec fold1 states = function
        | Empty -> states
        | Leaf vs -> leaf (ArraySlice.fold1 f (fastGet states 0) vs)
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw ne) sw se
        | Slice _ as qr -> fold1 states (Slicing.reallocate qr)
    and fold2 states w e =
        let wstates, estates = hsplit2 states (cols w)
        flatNode (fold1 wstates w) (fold1 estates e)
    fold1 states qr

/// Slice b as to match the sub-shapes of a. If a is empty, the
/// resulting slices will all be empty. If a is not a Node, all slices
/// except for the NW slice will be empty.
let internal sliceToMatch a b =
    match a with
        | Empty -> Empty, Empty, Empty, Empty
        | Node (_, _, _, ne, nw, sw, se) ->
            slice 0         (cols nw) (rows ne) (cols ne) b,
            slice 0          0        (rows nw) (cols nw) b,
            slice (rows nw)  0        (rows sw) (cols sw) b,
            slice (rows ne) (cols sw) (rows se) (cols se) b
        | _ -> Empty, slice 0 0 (rows a) (cols b) b, Empty, Empty

/// Zip implementation for the general case where we do not assume
/// that both ropes have the same internal structure.
let rec internal genZip f i j lqr rqr (arr : _ [,]) =
    match lqr with
        | Empty -> Empty
        | Leaf vs ->
            let rqr = Slicing.reallocate rqr
            ArraySlice.iteri (fun x y e1 -> arr.[x + i, y + j] <- f e1 (get rqr x y)) vs
            leaf (ArraySlice.makeSlice i j (rows lqr) (cols lqr) arr)
        | Node (d, h, w, ne, nw, sw, se) ->
            let rne, rnw, rsw, rse = sliceToMatch lqr rqr
            let nw0 = genZip f i              j            nw rnw arr
            let ne0 = genZip f i             (j + cols nw) ne rne arr
            let sw0 = genZip f (i + rows nw)  j            sw rsw arr
            let se0 = genZip f (i + rows ne) (j + cols sw) se rse arr
            Node (d, h, w, ne0, nw0, sw0, se0)
        | Slice _ -> genZip f i j (Slicing.reallocate lqr) rqr arr

/// True if the shape of two ropes match.
let internal shapesMatch a b =
    rows a = rows b && cols a = cols b

/// True if a and b are nodes and the shapes of all their sub-ropes in
/// the same positions match.
let internal subShapesMatch a b =
    match a, b with
        | Node (_, _, _, ane, anw, asw, ase),
          Node (_, _, _, bne, bnw, bsw, bse) ->
            shapesMatch ane bne
            && shapesMatch anw bnw
            && shapesMatch asw bsw
            && shapesMatch ase bse
        | _ -> false

/// Zip function that assumes that the internal structure of two ropes
/// matches. If not, it falls back to the slow, general case.
let rec internal fastZip f i j lqr rqr (arr : _ [,]) =
    match lqr, rqr with
        | Empty, Empty -> Empty
        | Leaf ls, Leaf rs when shapesMatch lqr rqr ->
            // Map and write into target array.
            ArraySlice.iteri2 (fun x y e1 e2 -> arr.[x + i, y + j] <- f e1 e2) ls rs
            leaf (ArraySlice.makeSlice i j (rows lqr) (cols lqr) arr)
        | Node (_, _, _, lne, lnw, lsw, lse), Node (_, _, _, rne, rnw, rsw, rse)
            when subShapesMatch lqr rqr ->
                node (fastZip f i              (j + cols lnw) lne rne arr)
                     (fastZip f i              j              lnw rnw arr)
                     (fastZip f (i + rows lnw) j              lsw rsw arr)
                     (fastZip f (i + rows lne) (j + cols lsw) lse rse arr)
        // It may pay off to reallocate first if both reallocated quad
        // ropes have the same internal shape. This might be
        // over-fitted to matrix multiplication.
        | Slice _, Slice _ -> fastZip f i j (Slicing.reallocate lqr) (Slicing.reallocate rqr) arr
        | Slice _, _ ->       fastZip f i j (Slicing.reallocate lqr) rqr arr
        | Slice _, Slice _ -> fastZip f i j lqr (Slicing.reallocate rqr) arr
        | _ -> genZip f i j lqr rqr arr

/// Apply f to each (i, j) of lqr and rope.
let zip f lqr rqr =
    if not (shapesMatch lqr rqr) then
        failwith "ropes must have the same shape"
    let arr = Array2D.zeroCreate (rows lqr) (cols lqr)
    fastZip f 0 0 lqr rqr arr

/// Apply f to all values of the rope and reduce the resulting
/// values to a single scalar using g.
let rec mapreduce f g = function
    | Empty -> failwith "Impossible to reduce an empty rope"
    | Leaf vs -> ArraySlice.mapReduce f g vs
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        g (mapreduce f g nw) (mapreduce f g ne)
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        g (mapreduce f g nw) (mapreduce f g sw)
    | Node (_, _, _, ne, nw, sw, se) ->
        g (g (mapreduce f g nw) (mapreduce f g ne)) (g (mapreduce f g sw) (mapreduce f g se))
    | Slice _ as qr -> mapreduce f g (Slicing.reallocate qr)

/// Reduce all values of the rope to a single scalar.
let reduce f qr = mapreduce id f qr

/// Reduce row n.
let rowmapreduce f g qr n = mapreduce f g (slice n 0 1 (cols qr) qr)

/// Reduce column n.
let colmapreduce f g qr n = mapreduce f g (slice 0 n (rows qr) 1 qr)

let inline rowreduce f qr n = rowmapreduce id f qr n
let inline colreduce f qr n = colmapreduce id f qr n

/// Horizontal mapreduce can be composed from init, mapreduce and slice.
let hmapreduce f g qr =
    let red = rowmapreduce f g qr
    init (rows qr) 1 (fun i _ -> red i)

/// Vertical mapreduce can be composed from init, mapreduce and slice.
let vmapreduce f g qr =
    let red = colmapreduce f g qr
    init 1 (cols qr) (fun _ j -> red j)

let inline hreduce f qr = hmapreduce id f qr
let inline vreduce f qr = vmapreduce id f qr

let inline private offset f x =
    ((+) x) >> f

/// Compute the row-wise prefix sum of the rope for f starting with
/// states.
let rec hscan f states = function
    | Empty -> Empty
    | Leaf vs -> Leaf (ArraySlice.scan2 f states vs)
    | Node (_, _, _, ne, nw, sw, se) ->
        let nw' = hscan f states nw
        let sw' = hscan f (offset states (rows nw')) sw
        (* NW and SW might differ in height and width, we cannot join them to a thin node. *)
        let estate i =
            if i < rows nw' then fastGet nw' i (cols nw' - 1) else fastGet sw' (i - rows nw') (cols sw' - 1)
        let ne' = hscan f estate ne
        let se' = hscan f (offset estate (rows ne')) se
        node ne' nw' sw' se'
    | Slice _ as qr -> hscan f states (Slicing.reallocate qr)

/// Compute the column-wise prefix sum of the rope for f starting
/// with states.
let rec vscan f states = function
    | Empty -> Empty
    | Leaf vs -> Leaf (ArraySlice.scan1 f states vs)
    | Node (_, _, _, ne, nw, sw, se) ->
        let nw' = vscan f states nw
        let ne' = vscan f (offset states (cols nw')) ne
        (* NW and NE might differ in height and width, we cannot join them to a flat node. *)
        let sstate j =
            if j < cols nw' then fastGet nw' (rows nw' - 1) j else fastGet ne' (rows ne' - 1) (j - cols nw')
        let sw' = vscan f sstate sw
        let se' = vscan f (offset sstate (cols sw')) se
        node ne' nw' sw' se'
    | Slice _ as qr -> vscan f states (Slicing.reallocate qr)

/// Checks that some relation p holds between each two adjacent
/// elements in each row. This is slow and should not really be
/// used.
let forallRows p = function
    | Empty -> true
    | qr ->
        let xs = hfold (fun xs x -> x :: xs) (create (rows qr) 1 []) qr
        get (vmapreduce (List.rev >> List.pairwise >> List.forall (fun (x, y) -> p x y)) (&&) xs) 0 0

/// Checks that some relation p holds between each two adjacent
/// elements in each column. This is slow and should not really be
/// used.
let forallCols p = function
    | Empty -> true
    | qr ->
        let xs = vfold (fun xs x -> x :: xs) (create 1 (cols qr) []) qr
        get (hmapreduce (List.rev >> List.pairwise >> List.forall (fun (x, y) -> p x y)) (&&) xs) 0 0

/// Apply predicate p to all elements of rope and reduce the
/// elements in both dimension using logical and.
let forall p = function
    | Empty -> true
    | qr -> mapreduce p (&&) qr

/// Apply predicate p to all elements of rope and reduce the
/// elements in both dimensions using logical or.
let exists p = function
    | Empty -> false
    | qr -> mapreduce p (||) qr

/// Remove all elements from rope for which p does not hold. Input
/// rope must be of height 1.
let rec hfilter p = function
    | Empty -> Empty
    | Leaf vs -> leaf (ArraySlice.filter2 p vs)
    | Node (_, 1, _, ne, nw, Empty, Empty) ->
        flatNode (hfilter p nw) (hfilter p ne)
    | _ -> failwith "hight must be exactly 1"

/// Remove all elements from rope for which p does not hold. Input
/// rope must be of width 1.
let rec vfilter p = function
    | Empty -> Empty
    | Leaf vs -> leaf (ArraySlice.filter1 p vs)
    | Node (_, _, 1, Empty, nw, sw, Empty) ->
        thinNode (vfilter p nw) (vfilter p sw)
    | _ -> failwith "width must be exactly 1"

/// Transpose the quad rope. This is equal to swapping indices,
/// such that get rope i j = get (reverse rope) j i.
let rec transpose = function
    | Empty -> Empty
    | Leaf vs -> leaf (ArraySlice.transpose vs)
    | Node (_, _, _, ne, nw, sw, se) ->
        node (transpose sw) (transpose nw) (transpose ne) (transpose se)
    | Slice _ as qr -> transpose (Slicing.reallocate qr)

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
        | Node (_, _, _, ne, nw, sw, se) ->
            let h0 = h / 2.0
            let w0 = w / 2.0
            seq { yield! tikz i j h0 w0 sw
                  yield! tikz (i + h0) j h0 w0 nw
                  yield! tikz i (j + w0) h0 w0 se
                  yield! tikz (i + h0) (j + w0) h0 w0 ne
                  yield line i (j + w0) (i + h) (j + w0)
                  yield line (i + h0) j (i + h0) (j + w) }
        | Slice _ as qr -> tikz i j h w (Slicing.reallocate qr)
    let cmds = List.ofSeq (seq { yield! tikz 0.0 0.0 h w qr; yield rect 0.0 0.0 h w });
    printfn "%s" (String.concat "\n" cmds)
