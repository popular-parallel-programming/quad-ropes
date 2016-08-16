[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module RadTrees.QuadRope

open Types
open Utils

(* The maximal size of a leaf array in any direction. *)
#if DEBUG
let s_max = 4
#else
let s_max = 16
#endif

/// Number of rows in a rectangular tree.
let rows = function
    | Empty -> 0
    | Leaf vs -> ArraySlice.length1 vs
    | Node (_, h, _, _, _, _, _) -> h
    | Slice (_, _, h, _, _) -> h

/// Number of columns in a rectangular tree.
let cols = function
    | Empty -> 0
    | Leaf vs -> ArraySlice.length2 vs
    | Node (_, _, w, _, _, _, _) -> w
    | Slice (_, _, _, w, _) -> w

/// Depth of a rectangular tree.
let rec depth = function
    | Empty -> 0
    | Leaf _ -> 0
    | Node (d, _, _, _, _, _, _) -> d
    | Slice (_, _, _, _, rope) -> depth rope

let isEmpty = function
    | Empty -> true
    | _ -> false

let leaf vs =
    if ArraySlice.length1 vs = 0 || ArraySlice.length2 vs = 0 then
        Empty
    else
        Leaf vs

/// Pseudo-constructor for generating a new rope out of some
/// existing nodes.
let rec node ne nw sw se =
    match ne, nw, sw, se with
        | _, Empty, Empty, Empty -> ne
        | Empty, _, Empty, Empty -> nw
        | Empty, Empty, _, Empty -> sw
        | Empty, Empty, Empty, _ -> se
        | Empty, Empty, _, _ -> node se sw Empty Empty
        | _, Empty, Empty, _ -> node Empty ne se Empty
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

let inline private checkBounds rope i j =
    if rows rope <= i then
        invalidArg "i" "First index must be within bounds of quad rope."
    else if cols rope <= j then
        invalidArg "j" "Second index must be within bounds of quad rope."

/// Get the value of a location in the tree.
let get root i j =
    let rec get rope i j =
        match rope with
            | Empty -> invalidArg "rope" "Empty tree cannot contain values."
            | Leaf vs -> ArraySlice.get vs i j
            | Node (_, _, _, ne, nw, sw, se) ->
                if withinRange nw i j then
                    get nw i j
                else if withinRange ne i (j - cols nw) then
                    get ne i (j - cols nw)
                else if withinRange sw (i - rows nw) j then
                    get sw (i - rows nw) j
                else
                    get se (i - rows ne) (j - cols sw)
            | Slice (x, y, _, _, rope) -> get rope (i + x) (j + y)
    checkBounds root i j
    get root i j

/// Update a tree location without modifying the original tree.
let set root i j v =
    let rec set rope i j v =
        match rope with
            | Empty -> invalidArg "rope" "Empty tree cannot contain values."
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
            | Slice (x, y, h, w, rope) -> Slice (x, y, h, w, set rope (i + x) (j + y) v)
    checkBounds root i j
    set root i j v

/// Write to a tree location destructively.
let write root i j v =
    let rec write rope i j v =
        match rope with
            | Empty -> invalidArg "rope" "Empty tree cannot contain values."
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
            | Slice (x, y, _, _, rope) -> write rope (x + i) (y + j) v
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
let hbalance rope =
    let rec hbalance rope =
        if isBalancedH rope then
            rope
        else
            let rs = collect rope []
            reduceList (rebuild flatNode) rs
    and collect rope rs  =
        match rope with
            | Empty -> rs
            | Node (_, _, _, ne, nw, Empty, Empty) -> collect nw (collect ne rs)
            | Node (_, _, _, ne, nw, sw, se) ->
                node (hbalance ne) (hbalance nw) (hbalance sw) (hbalance se) :: rs
            | _ -> rope :: rs
    hbalance rope

/// Balance rope vertically.
let vbalance rope =
    let rec vbalance rope =
        if isBalancedV rope then
            rope
        else
            let rs = collect rope []
            reduceList (rebuild thinNode) rs
    and collect rope rs  =
        match rope with
            | Empty -> rs
            | Node (_, _, _, Empty, nw, sw, Empty) -> collect nw (collect sw rs)
            | Node (_, _, _, ne, nw, sw, se) ->
                node (vbalance ne) (vbalance nw) (vbalance sw) (vbalance se) :: rs
            | _ -> rope :: rs
    vbalance rope

/// Balancing after Boehm et al. It turns out that this is slightly
/// slower than reduce-rebuild balancing and the resulting tree is of
/// greater height.
module Boehm =
    let hbalance rope =
        let rec insert rope n = function
            | r0 :: r1 :: ropes when cols r1 <= n -> insert rope n (flatNode r1 r0 :: ropes)
            | r0 :: ropes when cols r0 <= n -> (flatNode r0 rope) :: ropes
            | ropes -> rope :: ropes
        let rec hbalance rope ropes =
            match rope with
                | Empty -> ropes
                | Node (_, _, _, ne, nw, Empty, Empty) when not (isBalancedH rope) ->
                    hbalance ne (hbalance nw ropes)
                | _ ->
                    insert rope (cols rope) ropes
        if isBalancedH rope then
            rope
        else
            List.reduce (fun ne nw -> flatNode nw ne) (hbalance rope [])

    let vbalance rope =
        let rec insert rope n = function
            | r0 :: r1 :: ropes when rows r1 <= n -> insert rope n (thinNode r1 r0 :: ropes)
            | r0 :: ropes when rows r0 <= n -> (thinNode r0 rope) :: ropes
            | ropes -> rope :: ropes
        let rec vbalance rope ropes =
            match rope with
                | Empty -> ropes
                | Node (_, _, _, Empty, nw, sw, Empty) when not (isBalancedH rope) ->
                    vbalance sw (vbalance nw ropes)
                | _ ->
                    insert rope (rows rope) ropes
        if isBalancedV rope then
            rope
        else
            List.reduce (fun sw nw -> thinNode nw sw) (vbalance rope [])

/// Compute the "subrope" starting from indexes i, j taking h and w
/// elements in vertical and horizontal direction.
let slice i j h w root =
    if rows root <= i || cols root <= j || h <= 0 || w <= 0 then
        Empty
    else if i <= 0 && rows root <= h && j <= 0 && cols root <= w then
        root
    else
        let i0 = max 0 i
        let j0 = max 0 j
        match root with
            | Slice (x, y, h0, w0, rope) ->
                Slice (x + i0, y + j0, min (h0 - i0) h, min (w0 - j0) w, rope)
            | _ ->
                Slice (i0, j0, min (rows root - i0) h, min (cols root - j0) w, root)

/// Split rope vertically from row i, taking h rows.
let inline vsplit i h rope =
    slice i 0 h (cols rope) rope

/// Split rope horizontally from column j, taking w columns.
let inline hsplit j w rope =
    slice 0 j (rows rope) w rope

/// Split rope in two at row i.
let inline vsplit2 rope i =
    vsplit 0 i rope, vsplit i (rows rope) rope

/// Split rope in two at column j.
let inline hsplit2 rope j =
    hsplit 0 j rope, hsplit j (cols rope) rope

module internal Slicing =

    /// Auxiliary function to recursively slice a tree structure.
    let rec private reallocate0 rope i j h w =
        match rope with
            | _ when i <= 0 && j <= 0 && rows rope <= h && cols rope <= w -> rope
            | _ when rows rope <= i || cols rope <= j || h <= 0 || w <= 0 -> Empty
            | Empty -> Empty
            | Leaf vs -> leaf (ArraySlice.slice vs i j h w)
            | Node (_, _, _, ne, nw, sw, se) ->
                let nw0 = reallocate0 nw i j h w
                let ne0 = reallocate0 ne i (j - cols nw) h (w - cols nw0)
                let sw0 = reallocate0 sw (i - rows nw) j (h - rows nw0) w
                let se0 = reallocate0 se (i - rows ne) (j - cols sw) (h - rows ne0) (w - cols sw0)
                node ne0 nw0 sw0 se0
            | Slice (x, y, h, w, rope) -> reallocate0 rope (x + i) (y + j) h w

    /// Actually compute a slice.
    let reallocate = function
        | Slice (i, j, h, w, rope) -> reallocate0 rope i j h w
        | rope -> rope

    /// Check whether i j h w forms a rectangle that is inside the
    /// domain of rope.
    let inline private dom rope i j h w =
        0 <= i && i <= rows rope && i + h <= rows rope &&
        0 <= j && j <= cols rope && j + w <= cols rope

    /// Find the smallest sub-rope that includes the indices of a
    /// slice. This is similar to slicing but does not perform new
    /// node allocations.
    let minimize rope =
        let rec minimize rope i j h w =
            match rope with
                | _ when i = 0 && j = 0 && h = rows rope && w = cols rope -> rope
                | Empty -> Empty
                | Leaf vs -> leaf (ArraySlice.slice vs i j h w) // Just return a view on arrays.
                | Node (_, _, _, ne, nw, sw, se) ->
                    if dom nw i j h w then
                        minimize nw i j h w
                    else if dom ne i (j - cols nw) h w then
                        minimize ne i (j - cols nw) h w
                    else if dom sw (i - rows nw) j h w then
                        minimize sw (i - rows nw) j h w
                    else if dom se (i - rows ne) (j - cols sw) h w then
                        minimize se (i - rows ne) (j - cols sw) h w
                    else // If the slice spans across sub-ropes, stop and make a new slice.
                        slice i j h w rope
                    | Slice (i, j, h, w, rope) -> minimize rope i j h w
        match rope with
            | Slice (i, j, h, w, rope) -> minimize rope i j h w
            | rope -> rope

    /// Compute a slice and map in the same traversal.
    let map f rope =
        let rec map rope i j h w =
            match rope with
                | _ when rows rope <= i || cols rope <= j || h <= 0 || w <= 0 -> Empty
                | Empty -> Empty
                | Leaf vs -> leaf (ArraySlice.map f (ArraySlice.slice vs i j h w))
                | Node (_, _, _, ne, nw, sw, se) ->
                    let nw0 = map nw i j h w
                    let ne0 = map ne i (j - cols nw) h (w - cols nw0)
                    let sw0 = map sw (i - rows nw) j (h - rows nw0) w
                    let se0 = map se (i - rows ne) (j - cols sw) (h - rows ne0) (w - cols sw0)
                    node ne0 nw0 sw0 se0
                | Slice (x, y, h, w, rope) -> map rope (x + i) (y + j) h w
        map rope 0 0 (rows rope) (cols rope)

/// Concatenate two trees vertically.
let vcat upper lower =
    let canMerge us ls =
        ArraySlice.length2 us = ArraySlice.length2 ls &&
        ArraySlice.length1 us + ArraySlice.length1 ls <= s_max
    let rec vcat upper lower =
        match upper, lower with
            | Empty, _ -> lower
            | _, Empty -> upper
            | Slice _, _ -> vcat (Slicing.reallocate upper) lower
            | _, Slice _ -> vcat upper (Slicing.reallocate lower)

            | Leaf us, Leaf ls when canMerge us ls ->
                leaf (ArraySlice.cat1 us ls)

            | Node (_, _, _, Empty, nwu, Leaf swus, Empty), Leaf ls when canMerge swus ls->
                thinNode nwu (leaf (ArraySlice.cat1 swus ls))

            | Leaf us, Node (_, _, _, Empty, Leaf nwls, swl, Empty) when canMerge us nwls ->
                thinNode (leaf (ArraySlice.cat1 us nwls)) swl

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

            | (Node (_, _, _, neu, nwu, Empty, Empty),
               Node (_, _, _, nel, nwl, Empty, Empty)) ->
                node neu nwu nwl nel

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
            | Empty, _ -> right
            | _, Empty -> left
            | Slice _, _ -> hcat (Slicing.reallocate left) right
            | _, Slice _ -> hcat left (Slicing.reallocate right)

            | Leaf ls, Leaf rs when canMerge ls rs ->
                leaf (ArraySlice.cat2 ls rs)

            | Node (_, _, _, Leaf lnes, lnw, Empty, Empty), Leaf rs when canMerge lnes rs->
                flatNode lnw (leaf (ArraySlice.cat2 lnes rs))

            | Leaf ls, Node (_, _, _, rne, Leaf rnws, Empty, Empty) when canMerge ls rnws ->
                flatNode (leaf (ArraySlice.cat2 ls rnws)) rne

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

            | (Node (_, _, _, Empty, lnw, lsw, Empty),
               Node (_, _, _, Empty, rnw, rsw, Empty)) ->
                node rnw lnw lsw rsw

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
    | Slice (i, j, h, w, rope) -> Slice (i, j, h, w, hrev rope)

/// Reverse rope vertically.
let rec vrev = function
    | Empty -> Empty
    | Leaf vs -> Leaf (ArraySlice.rev1 vs)
    | Node (d, h, w, ne, nw, Empty, Empty) ->
        Node (d, h, w, vrev ne, vrev nw, Empty, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        Node (d, h, w, vrev se, vrev sw, vrev nw, vrev ne)
    | Slice (i, j, h, w, rope) -> Slice (i, j, h, w, vrev rope)

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
let inline initAll h w e =
    init h w (fun _ _ -> e)

/// Generate a singleton quad rope.
let inline singleton v =
    initAll 1 1 v

/// Initialize a rope with all zeros.
let inline initZeros h w = initAll h w 0

/// True if rope is a singleton, false otherwise.
let isSingleton rope =
    rows rope = 1 && cols rope = 1

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
    | Slice _ as rope -> iter f (Slicing.reallocate rope)

/// Apply a function with side effects to all elements and their
/// corresponding index pair.
let iteri f rope =
    let rec iteri f i j = function
        | Empty -> ()
        | Leaf vs -> ArraySlice.iteri (fun i0 j0 v -> f (i + i0) (j + j0) v) vs
        | Node (_, _, _, ne, nw, sw, se) ->
            iteri f i j nw
            iteri f i (j + cols nw) ne
            iteri f (i + rows nw) j sw
            iteri f (i + rows ne) (j + cols sw) se
        | Slice _ as rope -> iteri f i j (Slicing.reallocate rope)
    iteri f 0 0 rope

/// Conversion into 1D array.
let toArray rope =
    let arr = Array.zeroCreate ((rows rope) * (cols rope))
    // This avoids repeated calls to get; it is enough to traverse
    // the rope once.
    iteri (fun i j v -> arr.[i * cols rope + j] <- v) rope
    arr

/// Conversion into 2D array.
let toArray2D rope =
    let arr = Array2D.zeroCreate (rows rope) (cols rope)
    // This avoids repeated calls to get; it is enough to traverse
    // the rope once.
    iteri (fun i j v -> arr.[i, j] <- v) rope
    arr

/// Reallocate a rope form the ground up. Sometimes, this is the
/// only way to improve performance of a badly composed quad rope.
let inline reallocate rope =
    fromArray2D (toArray2D rope)

/// Apply a function to every element in the tree and preserves the
/// tree structure.
let rec map f root =
    match root with
        | Empty -> Empty
        | Leaf vs -> leaf (ArraySlice.map f vs)
        | Node (d, h, w, ne, nw, sw, se) ->
            Node (d, h, w,
                  map f ne,
                  map f nw,
                  map f sw,
                  map f se)
        | Slice _ as rope -> Slicing.map f rope

let toCols = function
    | Empty -> Seq.empty
    | rope ->
        seq { for j in 0 .. cols rope - 1 ->
              seq { for i in 0 .. rows rope - 1 -> get rope i j }}

let toColsArray rope = (toCols >> Seq.concat >> Array.ofSeq) rope

let toRows = function
    | Empty -> Seq.empty
    | rope ->
        seq { for i in 0 .. rows rope - 1 ->
              seq { for j in 0 .. cols rope - 1 -> get rope i j }}

let toRowsArray rope = (toRows >> Seq.concat >> Array.ofSeq) rope

/// Fold each row of rope with f, starting with the according
/// state in states.
let hfold f states rope =
    if rows states <> rows rope then
        invalidArg "states" "Must have the same height as rope."
    let rec fold1 states = function
        | Empty -> states
        | Leaf vs -> leaf (ArraySlice.fold2 f (fun i -> get states i 0) vs)
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw sw) ne se
        | Slice _ as rope -> fold1 states (Slicing.reallocate rope)
    and fold2 states n s =
        let nstates, sstates = vsplit2 states (rows n)
        thinNode (fold1 nstates n) (fold1 sstates s)
    fold1 states rope

/// Fold each column of rope with f, starting with the according
/// state in states.
let vfold f states rope =
    if cols states <> cols rope then
        invalidArg "states" "Must have the same width as rope."
    let rec fold1 states = function
        | Empty -> states
        | Leaf vs -> leaf (ArraySlice.fold1 f (get states 0) vs)
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw ne) sw se
        | Slice _ as rope -> fold1 states (Slicing.reallocate rope)
    and fold2 states w e =
        let wstates, estates = hsplit2 states (cols w)
        flatNode (fold1 wstates w) (fold1 estates e)
    fold1 states rope

/// Zip implementation for the general case where we do not assume
/// that both ropes have the same internal structure.
let rec private genZip f lope rope =
    match lope with
        | Empty -> Empty
        | Leaf vs ->
            let rope = Slicing.minimize rope
            leaf (ArraySlice.mapi (fun i j e -> f e (get rope i j)) vs)
        | Node (d, h, w, ne, nw, sw, se) ->
            let nw0 = genZip f nw (slice 0 0 (rows nw) (cols nw) rope)
            let ne0 = genZip f ne (slice 0 (cols nw) (rows ne) (cols ne) rope)
            let sw0 = genZip f sw (slice (rows nw) 0 (rows sw) (cols sw) rope)
            let se0 = genZip f se (slice (rows ne) (cols sw) (rows se) (cols se) rope)
            Node (d, h, w, ne0, nw0, sw0, se0)
        | Slice _ -> genZip f (Slicing.reallocate lope) rope

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
let rec private fastZip f lope rope =
    match lope, rope with
        | Empty, Empty -> Empty
        | Leaf ls, Leaf rs when shapesMatch lope rope -> leaf (ArraySlice.map2 f ls rs)
        | Node (_, _, _, lne, lnw, lsw, lse), Node (_, _, _, rne, rnw, rsw, rse)
            when subShapesMatch lope rope ->
                node (fastZip f lne rne) (fastZip f lnw rnw) (fastZip f lsw rsw) (fastZip f lse rse)
        // It may pay off to reallocate first if both reallocated quad
        // ropes have the same internal shape. This might be
        // over-fitted to matrix multiplication.
        | Slice _, Slice _ -> fastZip f (Slicing.reallocate lope) (Slicing.reallocate rope)
        | Slice _, _ ->       fastZip f (Slicing.reallocate lope) rope
        | Slice _, Slice _ -> fastZip f lope (Slicing.reallocate rope)
        | _ -> genZip f lope rope

/// Apply f to each (i, j) of lope and rope.
let zip f lope rope =
    if cols lope <> cols rope || rows lope <> rows rope then
        failwith "ropes must have the same shape"
    fastZip f lope rope

/// Map f to every element of the rope and reduce rows with g.
let rec hmapreduce f g = function
    | Empty -> Empty
    | Leaf vs -> leaf (ArraySlice.mapReduce2 f g vs)
    | Node (_, _, _, ne, nw, sw, se) ->
        (* Both, w and e, are of width 1. *)
        let w = thinNode (hmapreduce f g nw) (hmapreduce f g sw)
        let e = thinNode (hmapreduce f g ne) (hmapreduce f g se)
        match e with
            | Empty -> w
            | _ -> zip g w e
    | Slice _ as rope -> hmapreduce f g (Slicing.reallocate rope)

/// Map f to every element of the rope and reduce columns with g.
let rec vmapreduce f g = function
    | Empty -> Empty
    | Leaf vs -> leaf (ArraySlice.mapReduce1 f g vs)
    | Node (_, _, _, ne, nw, sw, se) ->
        (* Both, n and s, are of height 1. *)
        let n = flatNode (vmapreduce f g nw) (vmapreduce f g ne)
        let s = flatNode (vmapreduce f g sw) (vmapreduce f g se)
        match s with
            | Empty -> n
            | _ -> zip g n s
    | Slice _ as rope -> vmapreduce f g (Slicing.reallocate rope)

/// Reduce all rows of rope with f.
let inline hreduce f rope = hmapreduce id f rope

/// Reduce all columns of rope with f.
let inline vreduce f rope = vmapreduce id f rope

/// Apply f to all values of the rope and reduce the resulting
/// values to a single scalar using g.
let rec mapreduce f g = function
    | Empty -> failwith "impossible to reduce an empty rope"
    | Leaf vs -> ArraySlice.mapReduce f g vs
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        g (mapreduce f g nw) (mapreduce f g ne)
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        g (mapreduce f g nw) (mapreduce f g sw)
    | Node (_, _, _, ne, nw, sw, se) ->
        g (g (mapreduce f g nw) (mapreduce f g ne)) (g (mapreduce f g sw) (mapreduce f g se))
    | Slice _ as rope -> mapreduce f g (Slicing.reallocate rope)

/// Reduce all values of the rope to a single scalar.
let reduce f rope = mapreduce id f rope

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
            if i < rows nw' then get nw' i (cols nw' - 1) else get sw' (i - rows nw') (cols sw' - 1)
        let ne' = hscan f estate ne
        let se' = hscan f (offset estate (rows ne')) se
        node ne' nw' sw' se'
    | Slice _ as rope -> hscan f states (Slicing.reallocate rope)

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
            if j < cols nw' then get nw' (rows nw' - 1) j else get ne' (rows ne' - 1) (j - cols nw')
        let sw' = vscan f sstate sw
        let se' = vscan f (offset sstate (cols sw')) se
        node ne' nw' sw' se'
    | Slice _ as rope -> vscan f states (Slicing.reallocate rope)

/// Checks that some relation p holds between each two adjacent
/// elements in each row. This is slow and should not really be
/// used.
let forallRows p = function
    | Empty -> true
    | rope ->
        let xs = hfold (fun xs x -> x :: xs) (initAll (rows rope) 1 []) rope
        get (vmapreduce (List.rev >> List.pairwise >> List.forall (fun (x, y) -> p x y)) (&&) xs) 0 0

/// Checks that some relation p holds between each two adjacent
/// elements in each column. This is slow and should not really be
/// used.
let forallCols p = function
    | Empty -> true
    | rope ->
        let xs = vfold (fun xs x -> x :: xs) (initAll 1 (cols rope) []) rope
        get (hmapreduce (List.rev >> List.pairwise >> List.forall (fun (x, y) -> p x y)) (&&) xs) 0 0

/// Apply predicate p to all elements of rope and reduce the
/// elements in both dimension using logical and.
let forall p = function
    | Empty -> true
    | rope -> mapreduce p (&&) rope

/// Apply predicate p to all elements of rope and reduce the
/// elements in both dimensions using logical or.
let exists p = function
    | Empty -> false
    | rope -> mapreduce p (||) rope

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
    | Slice _ as rope -> transpose (Slicing.reallocate rope)

/// Produce a string with the tikz code for printing the rope as a
/// box diagram. This is useful for illustrating algorithms on
/// quad ropes.
let tikzify h w rope =
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
        | Slice _ as rope -> tikz i j h w (Slicing.reallocate rope)
    let cmds = List.ofSeq (seq { yield! tikz 0.0 0.0 h w rope; yield rect 0.0 0.0 h w });
    printfn "%s" (String.concat "\n" cmds)
