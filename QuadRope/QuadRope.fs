namespace RadTrees

open Microsoft.FSharp.Core

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a QuadRope =
    | Empty
    | Leaf of 'a ViewArray2D
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module QuadRope =

    (* The maximal size of a leaf array in any direction. *)
    let h_max = 6
    let w_max = 5
    let d_max = 4

    (* Initialize Fibonacci numbers at module load time. *)
    ignore (Fibonacci.fib d_max)

    /// Number of rows in a rectangular tree.
    let rows = function
        | Empty -> 0
        | Leaf vs -> ViewArray2D.length1 vs
        | Node (_, h, _, _, _, _, _) -> h

    /// Number of columns in a rectangular tree.
    let cols = function
        | Empty -> 0
        | Leaf vs -> ViewArray2D.length2 vs
        | Node (_, _, w, _, _, _, _) -> w

    /// Depth of a rectangular tree.
    let depth = function
        | Empty -> 0
        | Leaf _ -> 0
        | Node (d, _, _, _, _, _, _) -> d

    let isEmpty = function
        | Empty -> true
        | _ -> false

    let makeLeaf vs =
        if ViewArray2D.length1 vs = 0 || ViewArray2D.length2 vs = 0 then
            Empty
        else
            Leaf vs

    /// Pseudo-constructor for generating a new rope out of some
    /// existing nodes.
    let rec makeNode ne nw sw se =
        match ne, nw, sw, se with
            | _, Empty, Empty, Empty -> ne
            | Empty, _, Empty, Empty -> nw
            | Empty, Empty, _, Empty -> sw
            | Empty, Empty, Empty, _ -> se
            | Empty, Empty, _, _ -> makeNode se sw Empty Empty
            | _, Empty, Empty, _ -> makeNode Empty ne se Empty
            | _ ->
                let d = max (max (depth ne) (depth nw)) (max (depth sw) (depth se)) + 1
                let h = rows nw + rows sw
                let w = cols nw + cols ne
                Node (d, h, w, ne, nw, sw, se)

    let inline makeFlatNode w e =
        makeNode e w Empty Empty (* NB: Arguments switched. *)

    let inline makeThinNode n s =
        makeNode Empty n s Empty

    let inline private withinRange root i j =
        0 <= i && i < rows root && 0 <= j && j < cols root

    /// Get the value of a location in the tree.
    let rec get root i j =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> ViewArray2D.get vs i j
            | Node (_, _, _, ne, nw, sw, se) ->
                if withinRange nw i j then
                    get nw i j
                else
                    let j0 = j - cols nw
                    if withinRange ne i j0 then
                        get ne i j0
                    else
                        let i0 = i - rows nw
                        if withinRange sw i0 j then
                            get sw i0 j
                        else
                            get se (i - rows ne) (j - cols sw) (* Either contains or ends in out-of-bounds. *)

    /// Update a tree location wihtout modifying the original tree.
    let rec set root i j v =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Leaf (RadTrees.ViewArray2D.set vs i j v)
            | Node (d, h, w, ne, nw, sw, se) ->
                if withinRange nw i j then
                    Node (d, h, w, ne, set nw i j v, sw, se)
                else
                    let j0 = j - (cols nw)
                    if withinRange ne i j0 then
                        Node (d, h, w, set ne i j0 v, nw, sw, se)
                    else
                        let i0 = i - (rows nw)
                        if withinRange sw i0 j then
                            Node (d, h, w, ne, nw, set sw i0 j v, se)
                        else
                            Node (d, h, w, ne, nw, sw, set se (i - rows ne) (j - cols sw) v)

    /// Write to a tree location destructively.
    let rec write root i j v =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> ViewArray2D.write vs i j v
            | Node (_, _, _, ne, nw, sw, se) ->
                if withinRange nw i j then
                    write nw i j v
                else
                    let j0 = j - (cols nw)
                    if withinRange ne i j0 then
                        write ne i j0 v
                    else
                        let i0 = i - (rows nw)
                        if withinRange sw i0 j then
                            write sw i0 j v
                        else
                            write se (i - rows ne) (j - cols sw) v

    let private isBalanced d s =
        d <= 1 || d <= d_max && Fibonacci.fib (d + 1) <= s

    /// True if rope is balanced horizontally. False otherwise.
    let isBalancedH = function
        | Empty
        | Leaf _ -> true
        | Node (d, h, _, _, _, _, _) -> isBalanced d h

    /// True if rope is balanced vertically. False otherwise.
    let isBalancedV = function
        | Empty
        | Leaf _ -> true
        | Node (d, _, w, _, _, _, _) -> isBalanced d w

    let rec private reduce f = function
        | [] -> Empty
        | n :: [] -> n
        | ns -> reduce f (f ns)

    let rec private rebuild merge = function
        | [] -> []
        | x  :: [] -> x :: []
        | x :: y :: [] -> merge x y :: []
        | xs ->
            let lxs, rxs = List.splitAt ((List.length xs) / 2) xs
            rebuild merge lxs @ rebuild merge rxs

    /// Balance rope horizontally.
    let hbalance rope =
        let hreduce = reduce (rebuild makeFlatNode)
        let rec hbalance0 rope =
            let rs = collect rope []
            hreduce rs
        and collect rope rs  =
            match rope with
                | Empty -> rs
                | Node (_, _, _, ne, nw, Empty, Empty) -> collect nw (collect ne rs)
                | Node (_, _, _, ne, nw, sw, se) ->
                    makeNode (hbalance0 ne) (hbalance0 nw) (hbalance0 sw) (hbalance0 se) :: rs
                | _ -> rope :: rs
        hbalance0 rope

    /// Balance rope vertically.
    let vbalance rope =
        let vreduce = reduce (rebuild makeThinNode)
        let rec vbalance0 rope =
            let rs = collect rope []
            vreduce rs
        and collect rope rs  =
            match rope with
                | Empty -> rs
                | Node (_, _, _, Empty, nw, sw, Empty) -> collect nw (collect sw rs)
                | Node (_, _, _, ne, nw, sw, se) ->
                    makeNode (vbalance0 ne) (vbalance0 nw) (vbalance0 sw) (vbalance0 se) :: rs
                | _ -> rope :: rs
        vbalance0 rope

    /// Concatenate two trees vertically.
    let vcat upper lower =
        let canCopy us ls =
            ViewArray2D.length2 us = ViewArray2D.length2 ls && ViewArray2D.length1 us + ViewArray2D.length1 ls <= h_max
        if cols upper <> cols lower then
            failwith (sprintf "Trees must be of same width! u = %A\nl = %A" upper lower)
        match upper, lower with
            | Empty, _ -> lower
            | _, Empty -> upper
            | Leaf us, Leaf ls when canCopy us ls ->
                makeLeaf (ViewArray2D.cat1 us ls)

            | Node (_, _, _, Empty, nwu, Leaf swus, Empty), Leaf ls when canCopy swus ls->
                makeThinNode nwu (makeLeaf (ViewArray2D.cat1 swus ls))

            | Leaf us, Node (_, _, _, Empty, Leaf nwls, swl, Empty) when canCopy us nwls ->
                makeThinNode (makeLeaf (ViewArray2D.cat1 us nwls)) swl

            | (Node (_, _, _, neu, nwu, Leaf swus, Leaf seus),
               Node (_, _, _, Leaf nels, Leaf nwls, Empty, Empty))
                when canCopy swus nwls && canCopy seus nels ->
                    let sw = makeLeaf (ViewArray2D.cat1 swus nwls)
                    let se = makeLeaf (ViewArray2D.cat1 seus nels)
                    makeNode neu nwu sw se

            | (Node (_, _, _, Leaf neus, Leaf nwus, Empty, Empty),
               Node (_, _, _, Leaf nels, Leaf nwls, swl, sel))
                when canCopy nwus nwls && canCopy neus nels ->
                    let nw = makeLeaf (ViewArray2D.cat1 nwus nwls)
                    let ne = makeLeaf (ViewArray2D.cat1 neus nels)
                    makeNode ne nw swl sel

            | (Node (_, _, _, neu, nwu, Empty, Empty),
               Node (_, _, _, nel, nwl, Empty, Empty)) ->
                makeNode neu nwu nwl nel

            | _ -> makeThinNode upper lower

    /// Concatenate two trees horizontally.
    let hcat left right =
        let canCopy ls rs =
            ViewArray2D.length1 ls = ViewArray2D.length1 rs && ViewArray2D.length2 ls + ViewArray2D.length2 rs <= w_max
        if rows left <> rows right then
            failwith (sprintf "Trees must be of same height! l = %A\nr = %A" left right)
        match left, right with
            | Empty, _ -> right
            | _, Empty -> left
            | Leaf ls, Leaf rs when canCopy ls rs ->
                makeLeaf (ViewArray2D.cat2 ls rs)

            | Node (_, _, _, Leaf lnes, lnw, Empty, Empty), Leaf rs when canCopy lnes rs->
                makeFlatNode lnw (makeLeaf (ViewArray2D.cat2 lnes rs))

            | Leaf ls, Node (_, _, _, rne, Leaf rnws, Empty, Empty) when canCopy ls rnws ->
                makeFlatNode (makeLeaf (ViewArray2D.cat2 ls rnws)) rne

            | (Node (_, _, _, Leaf lnes, lnw, lsw, Leaf lses),
               Node (_, _, _, Empty, Leaf rnws, Leaf rsws, Empty))
                when canCopy lnes rnws && canCopy lses rsws ->
                    let ne = makeLeaf (ViewArray2D.cat2 lnes rnws)
                    let se = makeLeaf (ViewArray2D.cat2 lses rsws)
                    makeNode ne lnw lsw se

            | (Node (_, _, _, Empty, Leaf lnws, Leaf lsws, Empty),
               Node (_, _, _, rne, Leaf rnws, Leaf rsws, rse))
                when canCopy lnws rnws && canCopy lsws rsws ->
                    let nw = makeLeaf (ViewArray2D.cat2 lnws rnws)
                    let sw = makeLeaf (ViewArray2D.cat2 lsws rsws)
                    makeNode rne nw sw rse

            | (Node (_, _, _, Empty, lnw, lsw, Empty),
               Node (_, _, _, Empty, rnw, rsw, Empty)) ->
                makeNode rnw lnw lsw rsw

            | _ -> makeFlatNode left right

    /// Compute the "subrope" starting from indexes i, j taking h and w
    /// elements in vertical and horizontal direction.
    let rec split root i j h w =
        if rows root <= i || h <= 0 || cols root <= j || w <= 0 then
            Empty
        else if i <= 0 && rows root <= h && j <= 0 && cols root <= w then
            root
        else
            match root with
                | Empty -> Empty
                | Leaf vs -> makeLeaf (ViewArray2D.subArr i j h w vs)
                | Node (_, _, _, ne, nw, sw, se) ->
                    let nw0 = split nw i j h w
                    let ne0 = split ne i (j - cols nw) h (w - cols nw0)
                    let sw0 = split sw (i - rows nw) j (h - rows nw0) w
                    let se0 = split se (i - rows ne) (j - cols sw) (h - rows ne0) (w - cols sw0)
                    makeNode ne0 nw0 sw0 se0

    /// Split rope vertically from row i, taking h rows.
    let inline vsplit rope i h =
        split rope i 0 h (cols rope)

    /// Split rope horizontally from column j, taking w columns.
    let inline hsplit rope j w =
        split rope 0 j (rows rope) w

    /// Split rope in two at row i.
    let inline vsplit2 rope i =
        vsplit rope 0 i, vsplit rope i (rows rope)

    /// Split rope in two at column j.
    let inline hsplit2 rope j =
        hsplit rope 0 j, hsplit rope j (cols rope)

    /// Reverse rope horizontally.
    let rec hrev = function
        | Empty -> Empty
        | Leaf vs -> Leaf (ViewArray2D.rev2 vs)
        | Node (d, h, w, Empty, nw, sw, Empty) ->
            Node (d, h, w, Empty, hrev nw, hrev sw, Empty)
        | Node (d, h, w, ne, nw, sw, se) ->
            Node (d, h, w, hrev nw, hrev ne, hrev se, hrev sw)

    /// Reverse rope vertically.
    let rec vrev = function
        | Empty -> Empty
        | Leaf vs -> Leaf (ViewArray2D.rev1 vs)
        | Node (d, h, w, ne, nw, Empty, Empty) ->
            Node (d, h, w, vrev ne, vrev nw, Empty, Empty)
        | Node (d, h, w, ne, nw, sw, se) ->
            Node (d, h, w, vrev se, vrev sw, vrev nw, vrev ne)

    /// Generate a new tree without any intermediate values.
    let init h w f =
        let rec init0 h0 w0 h1 w1 =
            let h = h1 - h0
            let w = w1 - w0
            if h <= 0 || w <= 0 then
                Empty
            else if h <= h_max && w <= w_max then
                makeLeaf (ViewArray2D.init h w (fun i j -> f (h0 + i) (w0 + j)))
            else if w <= w_max then
                let hpv = h0 + h / 2
                makeThinNode (init0 h0 w0 hpv w1) (init0 hpv w0 h1 w1)
            else if h <= h_max then
                let wpv = w0 + w / 2
                makeFlatNode (init0 h0 w0 h1 wpv) (init0 h0 wpv h1 w1)
            else
                let hpv = h0 + h / 2
                let wpv = w0 + w / 2
                makeNode (init0 h0 wpv hpv w1) (* NE *)
                         (init0 h0 w0 hpv wpv) (* NW *)
                         (init0 hpv w0 h1 wpv) (* SW *)
                         (init0 hpv wpv h1 w1) (* SE *)
        init0 0 0 h w

    /// Reallocate a rope form the ground up. Sometimes, this is the
    /// only way to improve performance of a badly composed quad rope.
    let reallocate rope =
        init (rows rope) (cols rope) (get rope)

    /// Initialize a rope with all zeros.
    let initZeros h w =
        init h w (fun _ _ -> 0)

    let fromArray vss =
        init (ViewArray2D.length1 vss) (ViewArray2D.length2 vss) (ViewArray2D.get vss)

    /// Apply a function to every element in the tree and preserves the
    /// tree structure.
    let rec map f root =
        match root with
            | Empty -> Empty
            | Leaf vs -> makeLeaf (ViewArray2D.map f vs)
            | Node (d, h, w, ne, nw, sw, se) ->
                Node (d, h, w,
                      map f ne,
                      map f nw,
                      map f sw,
                      map f se)

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
        let rec fold1 states = function
            | Empty -> Empty
            | Leaf vs -> makeLeaf (ViewArray2D.fold2 f (fun i -> get states i 0) vs)
            | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw sw) ne se
        and fold2 states n s =
            let nstates, sstates = vsplit2 states (rows n)
            makeThinNode (fold1 nstates n) (fold1 sstates s)
        fold1 states rope

    /// Fold each column of rope with f, starting with the according
    /// state in states.
    let vfold f states rope =
        let rec fold1 states = function
            | Empty -> Empty
            | Leaf vs -> makeLeaf (ViewArray2D.fold1 f (get states 0) vs)
            | Node (_, _, _, ne, nw, sw, se) -> fold2 (fold2 states nw ne) sw se
        and fold2 states w e =
            let wstates, estates = hsplit2 states (cols w)
            makeFlatNode (fold1 wstates w) (fold1 estates e)
        fold1 states rope

    /// Apply f to each (i, j) of lope and rope.
    let zip f lope rope =
        if rows lope <> rows rope || cols lope <> cols rope then
            failwith "QuadRopes must have same shape."
        init (rows lope) (cols lope) (fun i j -> f (get lope i j) (get rope i j))

    // Map f to every element of the rope and reduce column-wise with g.
    let rec mapHreduce f g = function
        | Empty -> Empty
        | Leaf vs -> makeLeaf (ViewArray2D.mapreduce2 f g vs)
        | Node (_, _, _, ne, nw, sw, se) ->
            let w = makeThinNode (mapHreduce f g nw) (mapHreduce f g sw)
            let e = makeThinNode (mapHreduce f g ne) (mapHreduce f g se)
            match e with
                | Empty -> w
                | _ -> zip g w e

    // Map f to every element of the rope and reduce row-wise with g.
    let rec mapVreduce f g = function
        | Empty -> Empty
        | Leaf vs -> makeLeaf (ViewArray2D.mapreduce1 f g vs)
        | Node (_, _, _, ne, nw, sw, se) ->
            let n = makeFlatNode (mapVreduce f g nw) (mapVreduce f g ne)
            let s = makeFlatNode (mapVreduce f g sw) (mapVreduce f g se)
            match s with
                | Empty -> n
                | _ -> zip g n s

    /// Reduce all rows of rope with f.
    let rec hreduce f rope = mapHreduce id f rope

    /// Reduce all columns of rope with f.
    let rec vreduce f rope = mapVreduce id f rope

    let forall p rope =
        get (vreduce (&&) (mapHreduce p (&&) rope)) 0 0

    let rec hfilter p = function
        | Empty -> Empty
        | Leaf vs -> Leaf (ViewArray2D.filter2 p vs)
        | Node (_, 1, _, ne, nw, Empty, Empty) ->
            makeFlatNode (hfilter p nw) (hfilter p ne)
        | _ -> failwith "hight must be exactly 1"

    let rec vfilter p = function
        | Empty -> Empty
        | Leaf vs -> Leaf (ViewArray2D.filter1 p vs)
        | Node (_, _, 1, Empty, nw, sw, Empty) ->
            makeThinNode (vfilter p nw) (vfilter p sw)
        | _ -> failwith "width must be exactly 1"
