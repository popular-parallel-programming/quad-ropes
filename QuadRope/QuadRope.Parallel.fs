namespace RadTrees
namespace QuadRope

module Parallel =
    open RadTrees
    open RadTrees.QuadRope

    (* Constructor takes sub-ropes in order NE, NW, SW, SE. *)
    type ('a, 'b) Path =
        | Top
        | NW of 'a QuadRope * ('a, 'b) Path * 'a QuadRope * 'a QuadRope
        | NE of  ('a, 'b) Path * 'b QuadRope * 'a QuadRope * 'a QuadRope
        | SW of 'b QuadRope * 'b QuadRope * ('a, 'b) Path * 'a QuadRope
        | SE of 'b QuadRope * 'b QuadRope * 'b QuadRope * ('a, 'b) Path

    type ('a, 'b) Loc = 'a QuadRope * int * int * ('a, 'b) Path

    module Loc =

        let node ((n, _, _, _) : ('a, 'b) Loc) = n
        let i    ((_, i, _, _) : ('a, 'b) Loc) = i
        let j    ((_, _, j, _) : ('a, 'b) Loc) = j
        let path ((_, _, _, p) : ('a, 'b) Loc) = p

    let west (node, i, j, path) =
        match path with
            | NE (path, nw, sw, se) -> nw, i, j - cols nw, NW (node, path, sw, se)
            | SE (ne, nw, sw, path) -> sw, i, j - cols sw, SW (ne, nw, path, node)
            | _ -> node, i, j, path

    let east (node, i, j, path) =
        match path with
            | NW (ne, path, sw, se) -> ne, i, j + cols node, NE (path, node, sw, se)
            | SW (ne, nw, path, se) -> se, i, j + cols node, SE (ne, nw, node, path)
            | _ -> node, i, j, path

    let north (node, i, j, path) =
        match path with
            | SW (ne, nw, path, se) -> nw, i - rows nw, j, NW (ne, path, node, se)
            | SE (ne, nw, sw, path) -> ne, i - rows ne, j, NE (path, nw, sw, node)
            | _ -> node, i, j, path

    let south (node, i, j, path) =
        match path with
            | NE (path, nw, sw, se) -> se, i + rows node, j, SE (node, nw, sw, path)
            | NW (ne, path, sw, se) -> sw, i + rows node, j, SW (ne, node, path, se)
            | _ -> (node, i, j, path)

    let southWest (node, i, j, path) =
        match path with
            | NE (path, nw, sw, se) -> sw, i + rows nw, j - cols nw, SW (node, nw, path, se)
            | _ -> node, i, j, path

    let up (node, i, j, path) =
        match path with
            | NE (path, nw, sw, se) -> (makeNode node nw sw se), i, j - cols nw, path
            | NW (ne, path, sw, se) -> (makeNode ne node sw se), i, j, path
            | SW (ne, nw, path, se) -> (makeNode ne nw node se), i - rows nw, j, path
            | SE (ne, nw, sw, path) -> (makeNode ne nw sw node), i - rows ne, j - cols sw, path
            | _ -> node, i, j, path

    let down (node, i, j, path) =
        match node with
            | Empty
            | Leaf _ -> node, i, j, path
            | Node (_, _, _, ne, nw, sw, se) ->
                nw, i, j, NW (ne, path, sw, se)

    let rec upperLeftMost (node, i, j, path) =
        match node with
            | Empty
            | Leaf _ -> node, i, j, path
            | _ -> upperLeftMost (down (node, i, j, path))

    let start rope = upperLeftMost (rope, 0, 0, Top)

    let rec walkSouth (node, i, j, loc) =
        match loc with
            | Top -> None
            | NW _
            | NE _ -> Some (south (node, i, j, loc))
            | SW _ -> Option.map upperLeftMost (walkSouth (up (node, i, j, loc)))
            | SE _ -> Option.map (down >> east >> upperLeftMost) (walkSouth (up (node, i, j, loc)))

    let rec walkEast (node, i, j, loc) =
        match loc with
            | Top -> None
            | NW _
            | SW _ -> Some (east (node, i, j, loc))
            | NE _ -> Option.map upperLeftMost (walkEast (up (node, i, j, loc)))
            | SE _ -> Option.map (down >> south >> upperLeftMost) (walkEast (up (node, i, j, loc)))

    type ('a, 'b) Progress =
        | More of 'a
        | Done of 'b

    let rec next loc =
        match Loc.path loc with
            | Top -> Done (Loc.node loc)
            | NE _ -> More ((southWest >> upperLeftMost) loc)
            | NW _ -> More ((east >> upperLeftMost) loc)
            | SW _ -> More ((east >> upperLeftMost) loc)
            | SE _ -> next (up loc)

    let rec splitPath p u path =
        match path with
            | Top -> p, u
            | NW (ne, path, sw, se) -> splitPath p (makeNode ne u sw se) path
            | NE (path, nw, sw, se) -> splitPath (hcat nw p) (vcat u (hcat sw se)) path
            | SW (ne, nw, path, se) -> splitPath (vcat (hcat nw ne) p) (hcat u se) path
            | SE (ne, nw, sw, path) -> splitPath (makeNode ne nw sw p) u path

    let mapUntilSeq cond f node =
        if cond() then
            More node
        else
            Done (map f node)

    let mapUntil cond f rope =
        let rec cmap loc =
            match mapUntilSeq cond f (Loc.node loc) with
                | More node -> More (splitPath Empty node (Loc.path loc))
                | Done propes ->
                    match next (propes, 0, 0, Loc.path loc) with
                        | Done rope -> Done rope
                        | More loc -> cmap loc
        cmap (start rope)

    let inline size rope =
        rows rope * cols rope
