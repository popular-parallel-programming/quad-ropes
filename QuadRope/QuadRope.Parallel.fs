namespace RadTrees
namespace QuadRope

module Parallel =
    open RadTrees
    open RadTrees.QuadRope
    open RadTrees.QuadRope.Path

    type ('a, 'b) Progress =
        | More of 'a
        | Done of 'b

    let rec next rope path =
        match path with
            | Top -> Done rope
            | NE (path, nw, sw, se) -> More (upperLeftMost (sw, (SW (rope, nw, path, se))))
            | NW (ne, path, sw, se) -> More (upperLeftMost (ne, (NE (path, rope, sw, se))))
            | SW (ne, nw, path, se) -> More (upperLeftMost (se, (SE (ne, nw, rope, path))))
            | SE (ne, nw, sw, path) -> next (makeNode ne nw sw rope) path

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
        let rec cmap node path =
            match mapUntilSeq cond f node with
                | More node -> More (splitPath Empty node path)
                | Done propes ->
                    match next propes path with
                        | Done rope -> Done rope
                        | More (node, path) -> cmap node path
        let node, path = start rope
        cmap node path

    let inline size rope =
        rows rope * cols rope
