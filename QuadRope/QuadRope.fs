namespace RadTrees

type 'a QuadRope =
    | Empty
    | Leaf of 'a [,]
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope

module QuadRope =

    (* The maximal size of a leaf array in any direction. *)
    let h_max = 6
    let w_max = 5
    let d_max = 4

    (* Initialize Fibonacci numbers at module load time. *)
    ignore (Fibonacci.fib d_max)

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
        | Leaf _ -> 0
        | Node (d, _, _, _, _, _, _) -> d

    let makeLeaf vs =
        if Array2D.length1 vs = 0 || Array2D.length2 vs = 0 then
            Empty
        else
            Leaf vs

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

    let inline private withinRange root i j =
        0 <= i && i < rows root && 0 <= j && j < cols root

    (* Get the value of a location in the tree. *)
    let rec get root i j =
        match root with
            | Empty -> failwith "Empty tree cannot contain values."
            | Leaf vs -> Array2D.get vs i j
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

    let isBalancedH = function
        | Empty
        | Leaf _ -> true
        | Node (d, h, _, _, _, _, _) -> isBalanced d h

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

    let balanceH rope =
        let reduceH = reduce (rebuild (fun nw ne -> makeNode ne nw Empty Empty))
        let rec balanceH0 rope =
            let rs = collect rope []
            reduceH rs
        and collect rope rs  =
            match rope with
                | Empty -> rs
                | Node (_, _, _, ne, nw, Empty, Empty) -> collect nw (collect ne rs)
                | Node (_, _, _, ne, nw, sw, se) ->
                    makeNode (balanceH0 ne) (balanceH0 nw) (balanceH0 sw) (balanceH0 se) :: rs
                | _ -> rope :: rs
        balanceH0 rope

    let balanceV rope =
        let reduceV = reduce (rebuild (fun nw sw -> makeNode Empty nw sw Empty))
        let rec balanceV0 rope =
            let rs = collect rope []
            reduceV rs
        and collect rope rs  =
            match rope with
                | Empty -> rs
                | Node (_, _, _, Empty, nw, sw, Empty) -> collect nw (collect sw rs)
                | Node (_, _, _, ne, nw, sw, se) ->
                    makeNode (balanceV0 ne) (balanceV0 nw) (balanceV0 sw) (balanceV0 se) :: rs
                | _ -> rope :: rs
        balanceV0 rope

    (* Concatenate two trees vertically. *)
    let vcat upper lower =
        let rec vcat0 upper lower =
            match upper, lower with
                | Empty, _ -> lower
                | _, Empty -> upper
                | Leaf us, Leaf ls when Array2D.length1 us + Array2D.length1 ls <= h_max ->
                    Leaf (Array2D.cat1 us ls)

                | Node (du, hu, wu, Empty, nwu, swu, Empty), l ->
                    let sw = vcat0 swu l
                    Node (max du (depth sw + 1), hu + rows l, wu, Empty, nwu, sw, Empty)

                | u, Node (dl, hl, wl, Empty, nwl, swl, Empty) ->
                    let nw = vcat0 u nwl
                    Node (max dl (depth nw + 1), hl + rows u, wl, Empty, nw, swl, Empty)

                | (Node (du, hu, wu, neu, nwu, swu, seu),
                   Node ( _, hl,  _, nel, nwl, Empty, Empty))
                    when cols swu = cols nwl && cols seu = cols nel ->
                        let sw = vcat0 swu nwl
                        let se = vcat0 seu nel
                        let d = max (depth sw) (depth se)
                        Node (max du (d + 1), hu + hl, wu, neu, nwu, sw, se)

                | (Node ( _, hu, wu, neu, nwu, Empty, Empty),
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
        if cols upper <> cols lower then
            failwith (sprintf "Trees must be of same width! u = %A\nl = %A" upper lower)
        vcat0 upper lower

    (* Concatenate two trees horizontally. *)
    let hcat left right =
        let canCopy ls rs =
            Array2D.length2 ls + Array2D.length2 rs <= w_max
        let rec hcat0 left right =
            match left, right with
                | Empty, _ -> right
                | _, Empty -> left
                | Leaf ls, Leaf rs when canCopy ls rs ->
                    Leaf (Array2D.cat2 ls rs)

                | Node (_, _, _, Leaf lnes, lnw, Empty, Empty), Leaf rs when canCopy lnes rs->
                    makeNode (Leaf (Array2D.cat2 lnes rs)) lnw Empty Empty

                | Leaf ls, Node (_, _, _, rne, Leaf rnws, Empty, Empty) when canCopy ls rnws ->
                    makeNode rne (Leaf (Array2D.cat2 ls rnws)) Empty Empty

                | (Node (_, _, _, Leaf lnes, lnw, lsw, Leaf lses),
                   Node (_, _, _, Empty, Leaf rnws, Leaf rsws, Empty))
                    when canCopy lnes rnws && canCopy lses rsws ->
                        let ne = Leaf (Array2D.cat2 lnes rnws)
                        let se = Leaf (Array2D.cat2 lses rsws)
                        makeNode ne lnw lsw se

                | (Node (_, _, _, Empty, Leaf lnws, Leaf lsws, Empty),
                   Node (_, _, _, rne, Leaf rnws, Leaf rsws, rse))
                    when canCopy lnws rnws && canCopy lsws rsws ->
                        let nw = Leaf (Array2D.cat2 lnws rnws)
                        let sw = Leaf (Array2D.cat2 lsws rsws)
                        makeNode rne nw sw rse

                | (Node (_, _, _, Empty, lnw, lsw, Empty),
                   Node (_, _, _, Empty, rnw, rsw, Empty)) ->
                    makeNode rnw lnw lsw rsw

                | _ ->
                    let d = max (depth left) (depth right)
                    Node (d + 1, rows left, cols left + cols right, right, left, Empty, Empty)
        if rows left <> rows right then
            failwith (sprintf "Trees must be of same height! l = %A\nr = %A" left right)
        hcat0 left right

    (* Compute the "subrope" starting from indexes i, j taking h and w
       elements in vertical and horizontal direction. *)
    let rec split root i j h w =
        if h <= 0 || w <= 0 then
            Empty
        else if i <= 0 && rows root <= h && j <= 0 && cols root <= w then
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
            else if h <= h_max && w <= w_max then
                Leaf (Array2D.init h w (fun i j -> f (h0 + i) (w0 + j)))
            else if w <= w_max then
                let hpv = h0 + h / 2
                vcat (init0 h0 w0 hpv w1) (init0 hpv w0 h1 w1)
            else if h <= h_max then
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
                | SE (ne, nw, sw, path) -> ne, NE (path, nw, sw, node)
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
        let rec makeRow f (node, path) =
            seq { yield node, path
                  match f (node, path) with
                  | Some (node, path) -> yield! makeRow f (node, path)
                  | _ -> () }
        Seq.map ((makeRow Path.walkEast) >> (Seq.map fst)) (makeRow Path.walkSouth (Path.start root))
