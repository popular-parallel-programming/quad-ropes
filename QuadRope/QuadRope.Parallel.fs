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

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module private Path =

        let northEast (rope, path) =
            match path with
                | Top -> rope, path
                | NW (ne, path, sw, se) -> ne, NE (path, rope, sw, se)
                | NE _ -> rope, path
                | SW (ne, nw, path, se) -> ne, NE (path, nw, rope, se)
                | SE (ne, nw, sw, path) -> ne, NE (path, nw, sw, rope)

        let northWest (rope, path) =
            match path with
                | Top
                | NW _ -> rope, path
                | NE (path, nw, sw, se) -> nw, NW (rope, path, sw, se)
                | SW (ne, nw, path, se) -> nw, NW (ne, path, rope, se)
                | SE (ne, nw, sw, path) -> nw, NW (ne, path, sw, rope)

        let southWest (rope, path) =
            match path with
                | Top -> rope, path
                | NW (ne, path, sw, se) -> sw, SW (ne, rope, path, se)
                | NE (path, nw, sw, se) -> sw, SW (rope, nw, path, se)
                | SW _ -> rope, path
                | SE (ne, nw, sw, path) -> sw, SW (ne, nw, path, rope)

        let southEast (rope, path) =
            match path with
                | Top -> rope, path
                | NW (ne, path, sw, se) -> se, SE (ne, rope, sw, path)
                | NE (path, nw, sw, se) -> se, SE (rope, nw, sw, path)
                | SW (ne, nw, path, se) -> se, SE (ne, nw, rope, path)
                | SE _ -> rope, path

        let down (rope, path) =
            match rope with
                | Node (_, _, _, ne, nw, sw, se) -> nw, NW (ne, path, sw, se)
                | _ -> rope, path

        let up (rope, path) =
            match path with
                | Top -> rope, path
                | NW (ne, path, sw, se) -> node ne rope sw se, path
                | NE (path, nw, sw, se) -> node rope nw sw se, path
                | SW (ne, nw, path, se) -> node ne nw rope se, path
                | SE (ne, nw, sw, path) -> node ne nw sw rope, path

        let rec upperLeftMost (rope, path) =
            match rope with
                | Empty
                | Leaf _ -> rope, path
                | _ -> upperLeftMost (down (rope, path))

        let start rope = upperLeftMost (rope, Top)

    open Path

    type ('a, 'b) Progress =
        | More of 'a
        | Done of 'b

    /// Get the next leaf that follows the location (rope, path). It
    /// might seem unintuitive to process nodes in NW, NE, SW, SE
    /// order, but this allows us to re-use pseudo-constructors and
    /// other convenience functions.
    let rec next (rope, path : ('a, 'b) Path) =
        match path with
            | Top -> Done rope
            | NW (ne, path, sw, se) -> More (upperLeftMost (ne, NE (path, rope, sw, se)))
            | NE (path, nw, sw, se) -> More (upperLeftMost (sw, SW (rope, nw, path, se)))
            | SW (ne, nw, path, se) -> More (upperLeftMost (se, SE (ne, nw, rope, path)))
            | SE (ne, nw, sw, path) -> next (node ne nw sw rope, path)

    let node0 ne nw sw se =
        let d = max (max (depth ne) (depth nw)) (max (depth sw) (depth se))
        let h = max (rows nw + rows sw) (rows ne + rows se)
        let w = max (cols ne + cols nw) (cols sw + cols se)
        Node (d + 1, h, w, ne, nw, sw, se)

    /// Split the rope along the given path and return the processed
    /// and the unprocessed part. Here, it becomes clear why the NW,
    /// NE, SW, SE order is more desirable, as it allows us to use all
    /// the standard pseudo-constructors for quad ropes.
    let rec splitPath p u = function
        | Top -> p, u
        | NW (ne, path, sw, se) ->
            splitPath (node0 Empty p Empty Empty) (node0 ne u sw se) path
        | NE (path, nw, sw, se) ->
            splitPath (node0 p nw Empty Empty) (node0 u Empty sw se) path
        | SW (ne, nw, path, se) ->
            splitPath (node0 ne nw p Empty) (node0 Empty Empty u se) path
        | SE (ne, nw, sw, path) ->
            splitPath (node0 ne nw sw p) u path

    let rec mergePath rope l r =
        match l, r with
            | Top, Top -> rope
            | NW (_, l, _, _), NW (ne, r, sw, se) ->
                mergePath (node ne rope sw se) l r
            | NE (l, nw, _, _), NE (r, _, sw, se) ->
                mergePath (node rope nw sw se) l r
            | SW (ne, nw, l, _), SW (_, _, r, se) ->
                mergePath (node ne nw rope se) l r
            | SE (ne, nw, sw, l), SE (_, _, _, r) ->
                mergePath (node ne nw sw rope) l r
            | _ -> failwith "paths have different structure"

    let rebuild rope path =
        let rec build f = function
            | Top -> f
            | NE (path, _, _, _) -> build (down >> northEast >> f) path
            | NW (_, path, _, _) -> build (down >> f) path
            | SW (_, _, path, _) -> build (down >> southWest >> f) path
            | SE (_, _, _, path) -> build (down >> southEast >> f) path
        (build id path) (rope, Top)

    let rec mapUntilSeq cond f (rope, path) =
        if cond() then
            More (rope, path)
        else
            let rope' = map f rope
            match next (rope', path) with
                | Done rope -> Done rope
                | More (rope, path) -> mapUntilSeq cond f (rope, path)

    let mapUntil cond f rope =
        let u', path = start rope
        match mapUntilSeq cond f (u', path) with
            | Done rope -> Done rope
            | More (u, path) -> More (splitPath Empty u path, path)

    let split2 rope =
        match rope with
            | Node (_, h, w, ne, nw, sw, se) ->
                if h < w then
                    thinNode nw sw, thinNode ne se, hcat
                else
                    flatNode nw ne, flatNode sw se, vcat
            | rope -> rope, Empty, (fun rope _ -> rope)

    let rec pmap cond par2 f rope =
        match (mapUntil cond f rope) with
            | More ((_, u), path) ->
                let u1, u2, cat = split2 u
                let p1, p2 = par2 (fun () -> pmap cond par2 f u1) (fun () -> pmap cond par2 f u2)
                let p0, path0 = rebuild (cat p1 p2) path
                mergePath p0 path path0
            | Done rope -> rope
