module RadTrees.LazySplitting.QuadRope

open RadTrees
open RadTrees.Types

/// The path allows for splitting always only in one dimension. This
/// allows us to join a split a quad rope and join it later again
/// correctly.
type ('a, 'b) Path when 'a : equality and 'b : equality =
    | Top
    | North of ('a, 'b) Path * 'a QuadRope
    | West of ('a, 'b) Path * 'a QuadRope
    | South of 'b QuadRope * ('a, 'b) Path
    | East of 'b QuadRope * ('a, 'b) Path

type ('a, 'b) Progress =
    | More of 'a
    | Done of 'b

type Direction =
    | Horizontal
    | Vertical

/// Find the logically "first" leaf or sparse child of a quad rope.
let rec first qr p =
    match qr with
        | Node (_, _, _, _, Empty, nw, sw, Empty) ->
            first nw (North (p, sw))
        | Node (_, _, _, _, ne, nw, Empty, Empty) ->
            first nw (West (p, ne))
        | Node (_, _, _, _, ne, nw, sw, se) when QuadRope.cols nw = QuadRope.cols sw ->
            first (QuadRope.thinNode nw sw) (West (p, QuadRope.thinNode ne se))
        | Node (_, _, _, _, ne, nw, sw, se) ->
            first (QuadRope.flatNode nw ne) (North (p, QuadRope.flatNode sw se))
        | _ ->
            qr, p

/// Return the first leaf or the argument quad rope to process along
/// with the path to it.
let start qr =
    first qr Top

/// Return the next leaf or sparse quad rope to process.
let rec next qr p =
    match p with
        | Top -> Done qr
        | North (p', south) -> More (first south (South (qr, p')))
        | West (p', east) -> More (first east (East (qr, p')))
        | South (north, p') -> next (QuadRope.thinNode north qr) p'
        | East (west, p') -> next (QuadRope.flatNode west qr) p'

// TODO: Temporary for better debugging. Built-in assert does not work properly.
let myAssert b msg =
    if not b then
        failwith (sprintf "Assertion failed: %A" msg)

type 'a Partial =
    | Full of 'a
    | Partial of 'a * 'a * 'a -> 'a -> 'a

/// Split a quad rope along the path. The erroneous assumption is that
/// we can always reconstruct split the path and construct two valid
/// quad ropes. This is not the case, because if of a full quad rope,
/// only the NW leaf has been processed, we end up with being unable
/// to concatenate NE, SW and SE to each other.
let rec splitPath (pqr : 'b QuadRope) (uqr : 'a QuadRope) p dim =
    match p with
        | Top -> pqr, uqr, dim
        | North (p', south) ->
            // Impossible to build a quad rope out of non-matching
            myAssert ((QuadRope.cols uqr) = (QuadRope.cols south)) "uqr and south differ in cols"
            splitPath pqr (QuadRope.thinNode uqr south) p' QuadRope.vcat
        | West (p', east) ->
            splitPath pqr (QuadRope.flatNode uqr east) p' QuadRope.hcat
        | South (north, p') ->
            myAssert ((QuadRope.cols pqr) = (QuadRope.cols north)) "pqr and north differ in cols"
            splitPath (QuadRope.thinNode pqr north) uqr p' QuadRope.vcat
        | East (west, p') ->
            splitPath (QuadRope.flatNode pqr west) uqr p' QuadRope.hcat

/// Map f on qr sequentially until either there is no more work left
/// or until cond() nondeterministically evaluates to true.
let mapUntil cond f qr =
    let rec lp qr p =
        match next (QuadRope.map f qr) p with
            | More (qr, p) ->
                if cond() then
                    More (splitPath Empty qr p QuadRope.hcat)
                else
                    lp qr p
            | Done qr -> Done qr
    let s, p = first qr Top
    lp s p

/// Split quad rope into its children in the most favorable way
/// (i.e. avoid splitting thin nodes vertically and flat nodes
/// vertically) and return the result together with a function to
/// combine the results into a single quad rope again.
let split = function
    | Node (_, _, _, _, ne, nw, Empty, Empty) ->
        nw, ne, QuadRope.flatNode
    | Node (_, _, _, _, Empty, nw, sw, Empty) ->
        nw, sw, QuadRope.thinNode
    | Node (_, _, _, _, ne, nw, sw, se) when QuadRope.cols nw = QuadRope.cols sw ->
        QuadRope.thinNode nw sw, QuadRope.thinNode ne se, QuadRope.flatNode
    | Node (_, _, _, _, ne, nw, sw, se) ->
        QuadRope.flatNode nw ne, QuadRope.flatNode sw se, QuadRope.thinNode
    | qr -> // Cannot split without destroying quad rope structure. TODO: Reconsider.
        qr, Empty, (fun qr _ -> qr)

open Utils.Tasks

/// Map f over q in parallel using lazy tree splitting.
let rec map f qr =
    match qr with
        | Empty -> Empty
        | Leaf _
        | Sparse _ -> QuadRope.map f qr
        | _ ->
            // Map sequentially until threads are idle.
            // TODO: Change condition back to isIdle().
            match mapUntil (fun () -> true) f (QuadRope.materialize qr) with
                | More (pqr, uqr, merge) -> merge pqr (parmap f uqr)
                | Done pqr -> pqr

/// Call map in parallel if possible.
and parmap f uqr =
    match split uqr with
        | _, Empty, _ -> map f uqr
        | uqr1, uqr2, merge ->
            let pqr1, pqr2 = par2 (fun () -> map f uqr1) (fun () -> map f uqr2)
            merge pqr1 pqr2
