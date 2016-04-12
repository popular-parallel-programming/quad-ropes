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

    type ('a, 'b) Loc = 'a QuadRope * ('a, 'b) Path

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

    let southWest (node, path) =
        match path with
            | NE (path, nw, sw, se) -> sw, SW (node, nw, path, se)
            | _ -> node, path

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
            | _ -> upperLeftMost (down (node, path))

    let start rope = upperLeftMost (rope, Top)

    let rec walkSouth (node, path) =
        match path with
            | Top -> None
            | NW _
            | NE _ -> Some (south (node, path))
            | SW _ -> Option.map upperLeftMost (walkSouth (up (node, path)))
            | SE _ -> Option.map (down >> east >> upperLeftMost) (walkSouth (up (node, path)))

    let rec walkEast (node, path) =
        match path with
            | Top -> None
            | NW _
            | SW _ -> Some (east (node, path))
            | NE _ -> Option.map upperLeftMost (walkEast (up (node, path)))
            | SE _ -> Option.map (down >> south >> upperLeftMost) (walkEast (up (node, path)))

    type ('a, 'b) Progress =
        | More of 'a
        | Done of 'b

    let rec next (node, path) =
        match path with
            | Top -> Done node
            | NE _ -> More ((southWest >> upperLeftMost) (node, path))
            | NW _ -> More ((east >> upperLeftMost) (node, path))
            | SW _ -> More ((east >> upperLeftMost) (node, path))
            | SE _ -> next (up (node, path))

    let rec splitPath p u path =
        match path with
            | Top -> p, u
            | NW (ne, path, sw, se) -> splitPath p (makeNode ne u sw se) path
            | NE (path, nw, sw, se) ->
                splitPath (makeFlatNode nw p) (makeThinNode u (makeFlatNode sw se)) path
            | SW (ne, nw, path, se) ->
                splitPath (makeThinNode (makeFlatNode nw ne) p) (makeFlatNode u se) path
            | SE (ne, nw, sw, path) -> splitPath (makeNode ne nw sw p) u path

    let mapUntilSeq cond f node =
        if cond() then
            More node
        else
            Done (map f node)

    let mapUntil cond f rope =
        let rec cmap (node, path) =
            match mapUntilSeq cond f node with
                | More node -> More (splitPath Empty node path)
                | Done propes ->
                    match next (propes, path) with
                        | Done rope -> Done rope
                        | More loc -> cmap loc
        cmap (start rope)

    let inline size rope =
        rows rope * cols rope
