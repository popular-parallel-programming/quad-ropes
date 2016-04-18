namespace RadTrees.QuadRope

module Parallel =
    open RadTrees

    (* Constructor takes sub-ropes in order NE, NW, SW, SE. *)
    type ('a, 'b) Path =
        | Top
        | NW of 'a QuadRope * ('a, 'b) Path * 'a QuadRope * 'a QuadRope
        | NE of  ('a, 'b) Path * 'b QuadRope * 'a QuadRope * 'a QuadRope
        | SW of 'b QuadRope * 'b QuadRope * ('a, 'b) Path * 'a QuadRope
        | SE of 'b QuadRope * 'b QuadRope * 'b QuadRope * ('a, 'b) Path

    type ('a, 'b) Loc = 'a QuadRope * ('a, 'b) Path

    let west (rope, path) =
        match path with
            | NE (path, nw, sw, se) -> nw, NW (rope, path, sw, se)
            | SE (ne, nw, sw, path) -> sw, SW (ne, nw, path, rope)
            | _ -> rope, path

    let east (rope, path) =
        match path with
            | NW (ne, path, sw, se) -> ne, NE (path, rope, sw, se)
            | SW (ne, nw, path, se) -> se, SE (ne, nw, rope, path)
            | _ -> rope, path

    let north (rope, path) =
        match path with
            | SW (ne, nw, path, se) -> nw, NW (ne, path, rope, se)
            | SE (ne, nw, sw, path) -> ne, NE (path, nw, sw, rope)
            | _ -> rope, path

    let south (rope, path) =
        match path with
            | NE (path, nw, sw, se) -> se, SE (rope, nw, sw, path)
            | NW (ne, path, sw, se) -> sw, SW (ne, rope, path, se)
            | _ -> (rope, path)

    let southWest (rope, path) =
        match path with
            | NE (path, nw, sw, se) -> sw, SW (rope, nw, path, se)
            | _ -> rope, path

    let up (rope, path) =
        match path with
            | NE (path, nw, sw, se) -> (node rope nw sw se), path
            | NW (ne, path, sw, se) -> (node ne rope sw se), path
            | SW (ne, nw, path, se) -> (node ne nw rope se), path
            | SE (ne, nw, sw, path) -> (node ne nw sw rope), path
            | _ -> rope, path

    let down (rope, path) =
        match rope with
            | Empty
            | Leaf _ -> rope, path
            | Node (_, _, _, ne, nw, sw, se) ->
                nw, NW (ne, path, sw, se)

    let rec upperLeftMost (rope, path) =
        match rope with
            | Empty
            | Leaf _ -> rope, path
            | _ -> upperLeftMost (down (rope, path))

    let start rope = upperLeftMost (rope, Top)

    type ('a, 'b) Progress =
        | More of 'a
        | Done of 'b

    let rec next (rope, path) =
        match path with
            | Top -> Done rope
            | NE _ -> More ((southWest >> upperLeftMost) (rope, path))
            | NW _ -> More ((east >> upperLeftMost) (rope, path))
            | SW _ -> More ((east >> upperLeftMost) (rope, path))
            | SE _ -> next (up (rope, path))

    let rec splitPath p u path =
        match path with
            | Top -> p, u
            | NW (ne, path, sw, se) ->
                splitPath p (node ne u sw se) path
            | NE (path, nw, sw, se) ->
                splitPath (flatNode nw p) (thinNode u (flatNode sw se)) path
            | SW (ne, nw, path, se) ->
                splitPath (thinNode (flatNode nw ne) p) (flatNode u se) path
            | SE (ne, nw, sw, path) ->
                splitPath (node ne nw sw p) u path

    let mapUntilSeq cond f rope =
        if cond() then
            More rope
        else
            Done (map f rope)

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
