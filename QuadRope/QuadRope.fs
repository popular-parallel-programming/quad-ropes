[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module RadTrees.QuadRope

open Types
open Utils

(* The maximal size of a leaf array in any direction. *)
#if DEBUG
let h_max = 4
let w_max = 4
#else
let h_max = 32
let w_max = 16
#endif
let d_max = 32

(* Initialize Fibonacci numbers at module load time. *)
ignore (Fibonacci.fib d_max)

/// Number of rows in a rectangular tree.
let rows = function
    | Empty -> 0
    | Leaf vs -> Array2DView.length1 vs
    | Node (_, h, _, _, _, _, _) -> h
    | Slice (_, _, h, _, _) -> h

/// Number of columns in a rectangular tree.
let cols = function
    | Empty -> 0
    | Leaf vs -> Array2DView.length2 vs
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
    if Array2DView.length1 vs = 0 || Array2DView.length2 vs = 0 then
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
    if rows rope <= i || cols rope <= j then
        failwith (sprintf "Index out of bounds: (%d, %d) exceeds  %A" i j rope)

/// Get the value of a location in the tree.
let get root i j =
    let rec get0 rope i j =
        match rope with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Array2DView.get vs i j
            | Node (_, _, _, ne, nw, sw, se) ->
                if withinRange nw i j then
                    get0 nw i j
                else if withinRange ne i (j - cols nw) then
                    get0 ne i (j - cols nw)
                else if withinRange sw (i - rows nw) j then
                    get0 sw (i - rows nw) j
                else
                    get0 se (i - rows ne) (j - cols sw)
            | Slice (x, y, _, _, rope) -> get0 rope (i + x) (j + y)
    checkBounds root i j
    get0 root i j

/// Update a tree location without modifying the original tree.
let set root i j v =
    let rec set0 rope i j v =
        match rope with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Leaf (RadTrees.Array2DView.set vs i j v)
            | Node (d, h, w, ne, nw, sw, se) ->
                if withinRange nw i j then
                    Node (d, h, w, ne, set0 nw i j v, sw, se)
                else if withinRange ne i (j - cols nw) then
                    Node (d, h, w, set0 ne i (j - cols nw) v, nw, sw, se)
                else if withinRange sw (i - rows nw) j then
                    Node (d, h, w, ne, nw, set0 sw (i - rows nw) j v, se)
                else
                    Node (d, h, w, ne, nw, sw, set0 se (i - rows ne) (j - cols sw) v)
            | Slice (x, y, h, w, rope) -> Slice (x, y, h, w, set0 rope (i + x) (j + y) v)
    checkBounds root i j
    set0 root i j v

/// Write to a tree location destructively.
let write root i j v =
    let rec write0 rope i j v =
        match rope with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Array2DView.write vs i j v
            | Node (_, _, _, ne, nw, sw, se) ->
                if withinRange nw i j then
                    write0 nw i j v
                else if withinRange ne i (j - cols nw) then
                    write0 ne i (j - cols nw) v
                else if withinRange sw (i - rows nw) j then
                    write0 sw (i - rows nw) j v
                else
                    write0 se (i - rows ne) (j - cols sw) v
            | Slice (x, y, _, _, rope) -> write0 rope (x + i) (y + j) v
    checkBounds root i j
    write0 root i j v

let private isBalanced d s =
    d <= 1 || d <= d_max && Fibonacci.fib (d + 1) <= s

/// True if rope is balanced horizontally. False otherwise.
let isBalancedH = function
    | Node (d, h, _, _, _, _, _) -> isBalanced d h
    | _ -> true

/// True if rope is balanced vertically. False otherwise.
let isBalancedV = function
    | Node (d, _, w, _, _, _, _) -> isBalanced d w
    | _ -> true

let rec private reduceList f = function
    | []  -> Empty
    | [n] -> n
    | ns  -> reduceList f (f ns)

let rec private rebuild merge = function
    | [] -> []
    | [x] -> [x]
    | [x ; y] -> [merge x y]
    | xs ->
        let lxs, rxs = List.splitAt ((List.length xs) / 2) xs
        rebuild merge lxs @ rebuild merge rxs

/// Balance rope horizontally.
let hbalance rope =
    let rec hbalance0 rope =
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
                node (hbalance0 ne) (hbalance0 nw) (hbalance0 sw) (hbalance0 se) :: rs
            | _ -> rope :: rs
    hbalance0 rope

/// Balance rope vertically.
let vbalance rope =
    let rec vbalance0 rope =
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
                node (vbalance0 ne) (vbalance0 nw) (vbalance0 sw) (vbalance0 se) :: rs
            | _ -> rope :: rs
    vbalance0 rope

/// Compute the "subrope" starting from indexes i, j taking h and w
/// elements in vertical and horizontal direction.
let slice root i j h w =
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
let inline vsplit rope i h =
    slice rope i 0 h (cols rope)

/// Split rope horizontally from column j, taking w columns.
let inline hsplit rope j w =
    slice rope 0 j (rows rope) w

/// Split rope in two at row i.
let inline vsplit2 rope i =
    vsplit rope 0 i, vsplit rope i (rows rope)

/// Split rope in two at column j.
let inline hsplit2 rope j =
    hsplit rope 0 j, hsplit rope j (cols rope)

module internal Slicing =

    /// Auxiliary function to recursively slice a tree structure.
    let rec private reallocate0 rope i j h w =
        if i <= 0 && j <= 0 && rows rope <= h && cols rope <= w then
            rope
        else if rows rope <= i || cols rope <= j || h <= 0 || w <= 0 then
            Empty
        else
            match rope with
                | Empty -> Empty
                | Leaf vs -> leaf (Array2DView.slice i j h w vs)
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

    /// Auxiliary function to find the minimal sub-rope that contains
    /// the indexes in [i, h][j, w].
    let rec private minimize0 rope i j h w =
        if i = 0 && j = 0 && h = rows rope && w = cols rope then
            rope
        else
            match rope with
                | Empty -> Empty
                | Leaf vs -> leaf (Array2DView.slice i j h w vs) // Just return a view on arrays.
                | Node (_, _, _, ne, nw, sw, se) ->
                    if dom nw i j h w then
                        minimize0 nw i j h w
                    else if dom ne i (j - cols nw) h w then
                        minimize0 ne i (j - cols nw) h w
                    else if dom sw (i - rows nw) j h w then
                        minimize0 sw (i - rows nw) j h w
                    else if dom se (i - rows ne) (j - cols sw) h w then
                        minimize0 se (i - rows ne) (j - cols sw) h w
                    else
                        slice rope i j h w // If the slice spans across sub-ropes, stop and make a new slice.
                | Slice (i, j, h, w, rope) -> minimize0 rope i j h w

    /// Find the smallest sub-rope that includes the indices of a
    /// slice. This is similar to slicing but does not perform new
    /// node allocations.
    let minimize = function
        | Slice (i, j, h, w, rope) -> minimize0 rope i j h w
        | rope -> rope

    /// Compute a slice and map in the same traversal.
    let rec map0 f rope i j h w =
        match rope with
            | Empty -> Empty
            | Leaf vs -> leaf (Array2DView.map f (Array2DView.slice i j h w vs))
            | Node (_, _, _, ne, nw, sw, se) ->
                let nw0 = map0 f nw i j h w
                let ne0 = map0 f ne i (j - cols nw) h (w - cols nw0)
                let sw0 = map0 f sw (i - rows nw) j (h - rows nw0) w
                let se0 = map0 f se (i - rows ne) (j - cols sw) (h - rows ne0) (w - cols sw0)
                node ne0 nw0 sw0 se0
            | Slice (x, y, h, w, rope) -> map0 f rope (x + i) (y + j) h w

    /// Map over a slice while computing it.
    let inline map f rope = map0 f rope 0 0 (rows rope) (cols rope)

/// Concatenate two trees vertically.
let rec vcat upper lower =
    let canCopy us ls =
        Array2DView.length2 us = Array2DView.length2 ls
        && Array2DView.length1 us + Array2DView.length1 ls <= h_max
    if (not ((isEmpty upper) || (isEmpty lower))) && cols upper <> cols lower then
        failwith (sprintf "Trees must be of same width! u = %A\nl = %A" upper lower)
    match upper, lower with
        | Empty, _ -> lower
        | _, Empty -> upper
        | Slice _, _ -> vcat (Slicing.reallocate upper) lower
        | _, Slice _ -> vcat upper (Slicing.reallocate lower)

        | Leaf us, Leaf ls when canCopy us ls ->
            leaf (Array2DView.cat1 us ls)

        | Node (_, _, _, Empty, nwu, Leaf swus, Empty), Leaf ls when canCopy swus ls->
            thinNode nwu (leaf (Array2DView.cat1 swus ls))

        | Leaf us, Node (_, _, _, Empty, Leaf nwls, swl, Empty) when canCopy us nwls ->
            thinNode (leaf (Array2DView.cat1 us nwls)) swl

        | (Node (_, _, _, neu, nwu, Leaf swus, Leaf seus),
           Node (_, _, _, Leaf nels, Leaf nwls, Empty, Empty))
            when canCopy swus nwls && canCopy seus nels ->
                let sw = leaf (Array2DView.cat1 swus nwls)
                let se = leaf (Array2DView.cat1 seus nels)
                node neu nwu sw se

        | (Node (_, _, _, Leaf neus, Leaf nwus, Empty, Empty),
           Node (_, _, _, Leaf nels, Leaf nwls, swl, sel))
            when canCopy nwus nwls && canCopy neus nels ->
                let nw = leaf (Array2DView.cat1 nwus nwls)
                let ne = leaf (Array2DView.cat1 neus nels)
                node ne nw swl sel

        | (Node (_, _, _, neu, nwu, Empty, Empty),
           Node (_, _, _, nel, nwl, Empty, Empty)) ->
            node neu nwu nwl nel

        | _ -> thinNode upper lower

/// Concatenate two trees horizontally.
let rec hcat left right =
    let canCopy ls rs =
        Array2DView.length1 ls = Array2DView.length1 rs
        && Array2DView.length2 ls + Array2DView.length2 rs <= w_max
    if (not ((isEmpty left) || (isEmpty right))) && rows left <> rows right then
        failwith (sprintf "Trees must be of same height! l = %A\nr = %A" left right)
    match left, right with
        | Empty, _ -> right
        | _, Empty -> left
        | Slice _, _ -> hcat (Slicing.reallocate left) right
        | _, Slice _ -> hcat left (Slicing.reallocate right)

        | Leaf ls, Leaf rs when canCopy ls rs ->
            leaf (Array2DView.cat2 ls rs)

        | Node (_, _, _, Leaf lnes, lnw, Empty, Empty), Leaf rs when canCopy lnes rs->
            flatNode lnw (leaf (Array2DView.cat2 lnes rs))

        | Leaf ls, Node (_, _, _, rne, Leaf rnws, Empty, Empty) when canCopy ls rnws ->
            flatNode (leaf (Array2DView.cat2 ls rnws)) rne

        | (Node (_, _, _, Leaf lnes, lnw, lsw, Leaf lses),
           Node (_, _, _, Empty, Leaf rnws, Leaf rsws, Empty))
            when canCopy lnes rnws && canCopy lses rsws ->
                let ne = leaf (Array2DView.cat2 lnes rnws)
                let se = leaf (Array2DView.cat2 lses rsws)
                node ne lnw lsw se

        | (Node (_, _, _, Empty, Leaf lnws, Leaf lsws, Empty),
           Node (_, _, _, rne, Leaf rnws, Leaf rsws, rse))
            when canCopy lnws rnws && canCopy lsws rsws ->
                let nw = leaf (Array2DView.cat2 lnws rnws)
                let sw = leaf (Array2DView.cat2 lsws rsws)
                node rne nw sw rse

        | (Node (_, _, _, Empty, lnw, lsw, Empty),
           Node (_, _, _, Empty, rnw, rsw, Empty)) ->
            node rnw lnw lsw rsw

        | _ -> flatNode left right

/// Reverse rope horizontally.
let rec hrev = function
    | Empty -> Empty
    | Leaf vs -> Leaf (Array2DView.rev2 vs)
    | Node (d, h, w, Empty, nw, sw, Empty) ->
        Node (d, h, w, Empty, hrev nw, hrev sw, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        Node (d, h, w, hrev nw, hrev ne, hrev se, hrev sw)
    | Slice (i, j, h, w, rope) -> Slice (i, j, h, w, hrev rope)

/// Reverse rope vertically.
let rec vrev = function
    | Empty -> Empty
    | Leaf vs -> Leaf (Array2DView.rev1 vs)
    | Node (d, h, w, ne, nw, Empty, Empty) ->
        Node (d, h, w, vrev ne, vrev nw, Empty, Empty)
    | Node (d, h, w, ne, nw, sw, se) ->
        Node (d, h, w, vrev se, vrev sw, vrev nw, vrev ne)
    | Slice (i, j, h, w, rope) -> Slice (i, j, h, w, vrev rope)

/// Generate a new tree without any intermediate values.
let init h w f =
    let rec init0 h0 w0 h1 w1 =
        let h = h1 - h0
        let w = w1 - w0
        if h <= 0 || w <= 0 then
            Empty
        else if h <= h_max && w <= w_max then
            leaf (Array2DView.init h w (fun i j -> f (h0 + i) (w0 + j)))
        else if w <= w_max then
            let hpv = h0 + h / 2
            thinNode (init0 h0 w0 hpv w1) (init0 hpv w0 h1 w1)
        else if h <= h_max then
            let wpv = w0 + w / 2
            flatNode (init0 h0 w0 h1 wpv) (init0 h0 wpv h1 w1)
        else
            let hpv = h0 + h / 2
            let wpv = w0 + w / 2
            node (init0 h0 wpv hpv w1)
                 (init0 h0 w0 hpv wpv)
                 (init0 hpv w0 h1 wpv)
                 (init0 hpv wpv h1 w1)
    init0 0 0 h w

/// Reallocate a rope form the ground up. Sometimes, this is the
/// only way to improve performance of a badly composed quad rope.
let inline reallocate rope =
    init (rows rope) (cols rope) (get rope)

/// Initialize a rope where all elements are <code>e</code>.
let inline initAll h w e =
    init h w (fun _ _ -> e)

/// Initialize a rope with all zeros.
let inline initZeros h w = initAll h w 0

/// Initialize a rope from a native 2D-array.
let fromArray vss =
    init (Array2D.length1 vss) (Array2D.length2 vss) (Array2D.get vss)

/// Apply a function to every element in the tree and preserves the
/// tree structure.
let rec map f root =
    match root with
        | Empty -> Empty
        | Leaf vs -> leaf (Array2DView.map f vs)
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
        failwith "states and rope must have same height"
    let rec fold1 states = function
        | Empty -> states
        | Leaf vs -> leaf (Array2DView.fold2 f (fun i -> get states i 0) vs)
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
        failwith "states and rope must have same width"
    let rec fold1 states = function
        | Empty -> states
        | Leaf vs -> leaf (Array2DView.fold1 f (get states 0) vs)
        | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw ne) sw se
        | Slice _ as rope -> fold1 states (Slicing.reallocate rope)
    and fold2 states w e =
        let wstates, estates = hsplit2 states (cols w)
        flatNode (fold1 wstates w) (fold1 estates e)
    fold1 states rope

/// Apply f to each (i, j) of lope and rope.
let zip f lope rope =
    let rec zip0 f lope rope =
         match lope with
             | Empty -> Empty
             | Leaf vs ->
                 let rope = Slicing.minimize rope
                 leaf (Array2DView.mapi (fun i j e -> f e (get rope i j)) vs)
             | Node (d, h, w, ne, nw, sw, se) ->
                 let nw0 = zip0 f nw (slice rope 0 0 (rows nw) (cols nw))
                 let ne0 = zip0 f ne (slice rope 0 (cols nw) (rows ne) (cols ne))
                 let sw0 = zip0 f sw (slice rope (rows nw) 0 (rows sw) (cols sw))
                 let se0 = zip0 f se (slice rope (rows ne) (cols sw) (rows se) (cols se))
                 Node (d, h, w, ne0, nw0, sw0, se0)
             | Slice _ -> zip0 f (Slicing.reallocate lope) rope
    if cols lope <> cols rope || rows lope <> rows rope then
        failwith "ropes must have the same shape"
    zip0 f lope rope

/// Map f to every element of the rope and reduce rows with g.
let rec hmapreduce f g = function
    | Empty -> Empty
    | Leaf vs -> leaf (Array2DView.mapreduce2 f g vs)
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
    | Leaf vs -> leaf (Array2DView.mapreduce1 f g vs)
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
    | Leaf vs -> Array2DView.mapreduce f g vs
    | Node (_, _, _, ne, nw, Empty, Empty) ->
        g (mapreduce f g ne) (mapreduce f g nw)
    | Node (_, _, _, Empty, nw, sw, Empty) ->
        g (mapreduce f g nw) (mapreduce f g sw)
    | Node (_, _, _, ne, nw, sw, se) ->
        g (g (mapreduce f g ne) (mapreduce f g nw)) (g (mapreduce f g sw) (mapreduce f g se))
    | Slice _ as rope -> mapreduce f g (Slicing.reallocate rope)

/// Reduce all values of the rope to a single scalar.
let inline reduce f rope = mapreduce id f rope

let inline private offset f x =
    ((+) x) >> f

/// Compute the row-wise prefix sum of the rope for f starting with
/// states.
let rec hscan f states = function
    | Empty -> Empty
    | Leaf vs -> Leaf (Array2DView.scan2 f states vs)
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
    | Leaf vs -> Leaf (Array2DView.scan1 f states vs)
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
        get (vmapreduce (List.rev >> List.pairwise >> List.forall p) (&&) xs) 0 0

/// Checks that some relation p holds between each two adjacent
/// elements in each column. This is slow and should not really be
/// used.
let forallCols p = function
    | Empty -> true
    | rope ->
        let xs = vfold (fun xs x -> x :: xs) (initAll 1 (cols rope) []) rope
        get (hmapreduce (List.rev >> List.pairwise >> List.forall p) (&&) xs) 0 0

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
    | Leaf vs -> leaf (Array2DView.filter2 p vs)
    | Node (_, 1, _, ne, nw, Empty, Empty) ->
        flatNode (hfilter p nw) (hfilter p ne)
    | _ -> failwith "hight must be exactly 1"

/// Remove all elements from rope for which p does not hold. Input
/// rope must be of width 1.
let rec vfilter p = function
    | Empty -> Empty
    | Leaf vs -> leaf (Array2DView.filter1 p vs)
    | Node (_, _, 1, Empty, nw, sw, Empty) ->
        thinNode (vfilter p nw) (vfilter p sw)
    | _ -> failwith "width must be exactly 1"

/// Transpose the quad rope. This is equal to swapping indices,
/// such that get rope i j = get (reverse rope) j i.
let rec transpose = function
    | Empty -> Empty
    | Leaf vs -> leaf (Array2DView.transpose vs)
    | Node (_, _, _, ne, nw, sw, se) ->
        node (transpose sw) (transpose nw) (transpose ne) (transpose se)
    | Slice _ as rope -> transpose (Slicing.reallocate rope)

/// Straightforward conversion into a 2D array.
let toArray rope =
    Array2D.init (rows rope) (cols rope) (get rope)

/// Apply a function with side effects to all elements of the rope.
let rec apply f = function
    | Empty -> ()
    | Leaf vs ->
        for i in 0 .. (Array2DView.length1 vs) - 1 do
            for j in 0 .. (Array2DView.length2 vs) - 1 do
                f (Array2DView.get vs i j)
    | Node (_, _, _, ne, nw, sw, se) ->
        apply f ne
        apply f nw
        apply f sw
        apply f se
    | Slice _ as rope -> apply f (Slicing.reallocate rope)

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
