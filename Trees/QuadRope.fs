namespace RadTrees

type 'a QuadRope =
    | Empty
    | Leaf of 'a [,]
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope

module QuadRope =

    (* The maximal size of a leaf array in any direction. *)
    let maxHeight = 6
    let maxWidth = 5
    let maxDepth = 4

    (* Initialize Fibonacci numbers at module load time. *)
    ignore (Fibonacci.fib maxDepth)

    (* Number of rows in a rectangular tree. *)
    let rows = function
        | Empty -> 0
        | Leaf vs -> Array2D.length1 vs
        | Node (_, h, _, _, _, _, _) -> h

    (* Number of columns in a rectangular tree. *)
    let cols = function
        | Empty -> 0
        | Leaf vs -> Array2D.length2 vs
        | Node (_, _, w, _, _, _, _) -> w

    (* Depth of a rectangular tree. *)
    let depth = function
        | Empty -> 0
        | Leaf _ -> 1
        | Node (d, _, _, _, _, _, _) -> d

    let makeLeaf vs =
        if Array2D.length1 vs = 0 || Array2D.length2 vs = 0 then
            Empty
        else
            Leaf vs

    let inline private withinRange root i j =
        0 <= i && i < rows root && 0 <= j && j < cols root

    (* Get the value of a location in the tree. *)
    let rec get root i j =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Array2D.get vs i j
            | Node (_, h, w, ne, nw, sw, se) ->
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

    (* Update a tree location wihtout modifying the original tree. *)
    let rec set root i j v =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Leaf (RadTrees.Array2D.set vs i j v)
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

    (* Write to a tree location destructively. *)
    let rec write root i j v =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> vs.[i, j] <- v
            | Node (_, h, w, ne, nw, sw, se) ->
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

    let isBalanced = function
        | Empty
        | Leaf _ -> true
        | Node (d, h, w, _, _, _, _) ->
            if maxDepth < d then
                false
            else
                let n = Fibonacci.fib (d + 1)
                n <= h && n <= w

    (* Concatenate two trees vertically. *)
    let vcat upper lower =
        let canCopy us ls =
            Array2D.length2 us = Array2D.length2 ls && Array2D.length1 us + Array2D.length1 ls <= maxHeight
        let rec vcat0 upper lower =
            match upper, lower with
                | Leaf us, Leaf ls ->
                    Leaf (Array2D.cat1 us ls)

                | Node (du, hu, wu, Empty, nwu, swu, Empty), l ->
                    let sw = vcat0 swu l
                    Node (max du (depth sw + 1), hu + rows l, wu, Empty, nwu, sw, Empty)

                | u, Node (dl, hl, wl, Empty, nwl, swl, Empty) ->
                    let nw = vcat0 u nwl
                    Node (max dl (depth nw + 1), hl + rows u, wl, Empty, nw, swl, Empty)

                | (Node (du, hu, wu, neu, nwu, swu, seu),
                   Node (dl, hl,  _, nel, nwl, Empty, Empty))
                    when cols swu = cols nwl && cols seu = cols nel ->
                        let sw = vcat0 swu nwl
                        let se = vcat0 seu nel
                        let d = max (depth sw) (depth se)
                        Node (max du (d + 1), hu + hl, wu, neu, nwu, sw, se)

                | (Node (du, hu, wu, neu, nwu, Empty, Empty),
                   Node (dl, hl,  _, nel, nwl, swl, sel))
                    when cols nwu = cols nwl && cols neu = cols nel ->
                        let nw = vcat0 nwu nwl
                        let ne = vcat0 neu nel
                        let d = max (depth nw) (depth ne)
                        Node (max dl (d + 1), hu + hl, wu, ne, nw, swl, sel)

                | (Node (du, hu, wu, neu, nwu, Empty, Empty),
                   Node (dl, hl,  _, nel, nwl, Empty, Empty)) ->
                    Node (max du dl, hu + hl, wu, neu, nwu, nwl, nel)

                | _ ->
                    let d = max (depth upper) (depth lower)
                    Node (d + 1, rows upper + rows lower, cols upper, Empty, upper, lower, Empty)
        match upper, lower with
            | Empty, _ -> lower
            | _, Empty -> upper
            | _ when cols upper <> cols lower ->
                failwith (sprintf "Trees must be of same width! u = %A\nl = %A" upper lower)
            | _ -> vcat0 upper lower

    (* Concatenate two trees horizontally. *)
    let hcat left right =
        let inline canCopy ls rs =
            Array2D.length1 ls = Array2D.length1 rs && Array2D.length2 ls + Array2D.length2 rs <= maxWidth
        let rec hcat0 left right =
            match left, right with
                | Leaf ls, Leaf rs when canCopy ls rs ->
                    Leaf (Array2D.cat2 ls rs)

                | Node (ld, lh, lw, lne, lnw, Empty, Empty), r ->
                        let ne = hcat0 lne r
                        Node (max ld (depth ne + 1), lh, lw + cols r, ne, lnw, Empty, Empty)

                | l, Node (rd, rh, rw, rne, rnw, Empty, Empty) ->
                        let nw = hcat0 l rnw
                        Node (max rd (depth nw + 1), rh, cols l + rw, rne, nw, Empty, Empty)

                | (Node (ld, lh, lw, lne, lnw, lsw, lse),
                   Node (rd,  _, rw, Empty, rnw, rsw, Empty))
                    when rows lne = rows rnw && rows lse = rows rsw ->
                        let ne = hcat0 lne rnw
                        let se = hcat0 lse rsw
                        let d = max (depth ne) (depth se)
                        Node (max ld (d + 1), lh, lw + rw, ne, lnw, lsw, se)

                | (Node (ld, lh, lw, Empty, lnw, lsw, Empty),
                   Node (rd,  _, rw, rne, rnw, rsw, rse))
                    when rows lnw = rows rnw && rows lsw = rows rsw ->
                        let nw = hcat0 lnw rnw
                        let sw = hcat0 lsw rsw
                        let d = max (depth nw) (depth sw)
                        Node (max rd (d + 1), lh, lw + rw, rne, nw, sw, rse)

                | (Node (ld, lh, lw, Empty, lnw, lsw, Empty),
                   Node (rd,  _, rw, Empty, rnw, rsw, Empty)) ->
                    Node (max ld rd, lh, lw + rw, rnw, lnw, lsw, rsw)

                | _ ->
                    let d = max (depth left) (depth right)
                    Node (d + 1, rows left, cols left + cols right, right, left, Empty, Empty)
        match left, right with
            | Empty, _ -> right
            | _, Empty -> left
            | _ when rows left <> rows right ->
                    failwith (sprintf "Trees must be of same height! l = %A\nr = %A" left right)
            | _ -> hcat0 left right

    let makeNode ne nw sw se =
        match ne, nw, sw, se with
            | _, Empty, Empty, Empty -> ne
            | Empty, _, Empty, Empty -> nw
            | Empty, Empty, _, Empty -> sw
            | Empty, Empty, Empty, _ -> se
            | Empty, _, _, _ -> vcat nw (hcat sw se)
            | _, Empty, _, _ -> vcat ne (hcat sw se)
            | _, _, Empty, _ -> vcat (hcat nw ne) se
            | _, _, _, Empty -> vcat (hcat nw ne) sw
            | _ when cols nw = cols sw && cols ne = cols se ->
                hcat (vcat nw sw) (vcat ne se)
            | _ when rows nw = rows ne && rows sw = rows se ->
                vcat (hcat nw ne) (hcat sw se)
            | _ -> failwith
                     (sprintf
                        "Children must join to a regular rope. ne = %A\nnw = %A\nsw = %A\nse = %A"
                        ne nw sw se)

    let makeSomeNode ne nw sw se =
        let getOrEmpty = Option.getDefault Empty
        makeNode (getOrEmpty ne) (getOrEmpty nw) (getOrEmpty sw) (getOrEmpty se)

    (* Compute the "subrope" starting from indexes i, j taking h and w
       elements in vertical and horizontal direction. *)
    let rec split root i j h w =
        if h <= 0 || w <= 0 then
            Empty
        else if i <= 0 && rows root <= (i + h) && j <= 0 && cols root <= (j + w) then
            root
        else
            match root with
                | Empty -> Empty
                | Leaf vs -> Leaf (Array2D.subArr vs i j h w)
                | Node (_, _, _, ne, nw, sw, se) ->
                    let nw0 = split nw i j h w
                    let ne0 = split ne i (j - cols nw) h (w - cols nw0)
                    let sw0 = split sw (i - rows nw) j (h - rows nw0) w
                    let se0 = split se (i - rows ne) (j - cols sw) (h - rows ne0) (w - cols sw0)
                    makeNode ne0 nw0 sw0 se0

    let rec hrev = function
        | Empty -> Empty
        | Leaf vs -> Leaf (Array2D.rev2 vs)
        | Node (d, h, w, Empty, nw, sw, Empty) ->
            Node (d, h, w, Empty, hrev nw, hrev sw, Empty)
        | Node (d, h, w, ne, nw, sw, se) ->
            Node (d, h, w, hrev nw, hrev ne, hrev se, hrev sw)

    let rec vrev = function
        | Empty -> Empty
        | Leaf vs -> Leaf (Array2D.rev1 vs)
        | Node (d, h, w, ne, nw, Empty, Empty) ->
            Node (d, h, w, vrev ne, vrev nw, Empty, Empty)
        | Node (d, h, w, ne, nw, sw, se) ->
            Node (d, h, w, vrev se, vrev sw, vrev nw, vrev ne)

    (* Generate a new tree without any intermediate values. *)
    let init h w f =
        let rec init0 h0 w0 h1 w1 =
            let h = h1 - h0
            let w = w1 - w0
            if h <= 0 || w <= 0 then
                Empty
            else if h <= maxHeight && w <= maxWidth then
                Leaf (Array2D.init h w (fun i j -> f (h0 + i) (w0 + j)))
            else if w <= maxWidth then
                let hpv = h0 + h / 2
                vcat (init0 h0 w0 hpv w1) (init0 hpv w0 h1 w1)
            else if h <= maxHeight then
                let wpv = w0 + w / 2
                hcat (init0 h0 w0 h1 wpv) (init0 h0 wpv h1 w1)
            else
                let hpv = h0 + h / 2
                let wpv = w0 + w / 2
                makeNode (init0 h0 wpv hpv w1) (* NE *)
                         (init0 h0 w0 hpv wpv) (* NW *)
                         (init0 hpv w0 h1 wpv) (* SW *)
                         (init0 hpv wpv h1 w1) (* SE *)
        init0 0 0 h w

    let fromArray vss =
        init (Array2D.length1 vss) (Array2D.length2 vss) (Array2D.get vss)

    (* Iterate over a tree from the upper left to the lower right in
       row-first order. *)
    let rec toSeq = function
        | Empty -> Seq.empty
        | Leaf vs -> seq { for i in 0..Array2D.length1 vs - 1 do
                           for j in 0..Array2D.length2 vs - 1 ->
                           vs.[i, j] }
        | Node (_, _, _, ne, nw, sw, se) ->
            seq { yield! toSeq nw; yield! toSeq ne; yield! toSeq sw; yield! toSeq se }

    (* Bulk-operations on quad ropes: *)

    (* Apply a function to every element in the tree and preserves the
       tree structure. *)
    let rec map f root =
        match root with
            | Empty -> Empty
            | Leaf vs -> Leaf (Array2D.map f vs)
            | Node (d, h, w, ne, nw, sw, se) ->
                Node (d, h, w,
                      map f ne,
                      map f nw,
                      map f sw,
                      map f se)

    (* Fold from left to right and top-down in row-first order. *)
    let foldl f state root =
        Seq.fold f state (toSeq root)

    (* Fold from right to left and bottom-up in row-first order. *)
    let foldr f root state =
        Seq.foldBack f (toSeq root) state

    (* Constructor takes sub-ropes in order NE, NW, SW, SE. *)
    type ('a, 'b) Path =
        | Top
        | NW of 'a QuadRope * ('a, 'b) Path * 'a QuadRope * 'a QuadRope
        | NE of ('a, 'b) Path * 'b QuadRope * 'a QuadRope * 'a QuadRope
        | SW of 'b QuadRope * 'b QuadRope * ('a, 'b) Path * 'a QuadRope
        | SE of 'b QuadRope * 'b QuadRope * 'b QuadRope * ('a, 'b) Path

    module Path =

        let west (node, path) =
            match path with
                | NE (path, nw, sw, se) -> nw, NW (node, path, sw, se)
                | SE (ne, nw, sw, path) -> sw, SW (ne, nw, path, node)
                | _ -> node, path

        let east (node, path) =
            match path with
                | NW (ne, path, sw, se) -> ne, NE (path, node, sw, se)
                | SW (ne, nw, path, se) -> se, SE (ne, nw, node, path)
                | _ -> node, path

        let north (node, path) =
            match path with
                | SW (ne, nw, path, se) -> nw, NW (ne, path, node, se)
                | SE (ne, nw, sw, path) -> ne, NE (path, ne, sw, node)
                | _ -> node, path

        let south (node, path) =
            match path with
                | NE (path, nw, sw, se) -> se, SE (node, nw, sw, path)
                | NW (ne, path, sw, se) -> sw, SW (ne, node, path, se)
                | _ -> (node, path)

        let up (node, path) =
            match path with
                | NE (path, nw, sw, se) -> (makeNode node nw sw se), path
                | NW (ne, path, sw, se) -> (makeNode ne node sw se), path
                | SW (ne, nw, path, se) -> (makeNode ne nw node se), path
                | SE (ne, nw, sw, path) -> (makeNode ne nw sw node), path
                | _ -> node, path

        let down (node, path) =
            match node with
                | Empty
                | Leaf _ -> node, path
                | Node (_, _, _, ne, nw, sw, se) ->
                    nw, NW (ne, path, sw, se)

        let rec upperLeftMost (node, path) =
            match node with
                | Empty
                | Leaf _ -> node, path
                | Node (_, _, _, ne, nw, sw, se) ->
                    upperLeftMost (nw, (NW (ne, path, sw, se)))

        let start rope = upperLeftMost (rope, Top)

        let rec walkSouth (node, loc) =
            match loc with
                | Top -> None
                | NW _
                | NE _ -> Some (south (node, loc))
                | SW _ -> Option.map upperLeftMost (walkSouth (up (node, loc)))
                | SE _ -> Option.map (down >> east >> upperLeftMost) (walkSouth (up (node, loc)))

        let rec walkEast (node, loc) =
            match loc with
                | Top -> None
                | NW _
                | SW _ -> Some (east (node, loc))
                | NE _ -> Option.map upperLeftMost (walkEast (up (node, loc)))
                | SE _ -> Option.map (down >> south >> upperLeftMost) (walkEast (up (node, loc)))

    let flatten root =
        let step f a =
            match f a with
                | None -> None
                | Some b -> Some (a, b)
        let makeRow = Seq.unfold (step Path.walkEast) >> Seq.map fst
        Seq.map makeRow (Seq.unfold (step Path.walkSouth) (Path.start root))
