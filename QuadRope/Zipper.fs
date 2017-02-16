module RadTrees.LazySplitting.QuadRope

open RadTrees
open RadTrees.Types


type private ('a, 'b) Progress =
    | More of 'a
    | Done of 'b


type private ('a, 'b) Path when 'a : equality and 'b : equality =
    | Top
    | HCatL of ('a, 'b) Path * 'a QuadRope
    | HCatR of 'b QuadRope * ('a, 'b) Path
    | VCatL of ('a, 'b) Path * 'a QuadRope
    | VCatR of 'b QuadRope * ('a, 'b) Path


/// Get the "first", i.e. the upper-left-most leaf of the quad
/// rope.
let rec private first qr p =
    match qr with
        | HCat (_, _, _, _, a, b) ->
            first a (HCatL (p, b))
        | VCat (_, _, _, _, a, b) ->
            first a (VCatL (p, b))
        | _ -> qr, p


/// Shorthand for first qr Top.
let private start qr = first qr Top


/// Get the next leaf, if any.
let rec private next qr p =
    match p with
        | Top -> Done qr
        | HCatL (p', b) -> More (first b (HCatR (qr, p')))
        | HCatR (a, p') -> next (hnode a qr) p'
        | VCatL (p', b) -> More (first b (VCatR (qr, p')))
        | VCatR (a, p') -> next (vnode a qr) p'


/// Split the path into processed and unprocessed branches.
let rec private splitPath a b p =
    /// NOTE: This will not work. The problem is that we can not
    /// guarantee that, if we split a node in two, that the remainder
    /// will be of the correct size to be merged with the accumulated
    /// trees. This problem can be elided by diallowing either hcat or
    /// vcat, thereby forcing all quad ropes to have only exactly k
    /// many rows or columns. Quad ropes of height or width k cannot
    /// be composed vertically or horizontally, respectively. Hence,
    /// LTS is possible for 1D-ropes, where k = 1 for either
    /// dimension.
    match p with
        | Top -> a, b

        // It may be possible though to detect the cases where this is
        // not possible and to "finish" mapping over the missing
        // branch first (maybe in parallel) in order to be able to
        // deliver a well formed quad rope.

        // Current branch is left, concatenate the other one to
        // the right.
        | HCatL (p', b') -> splitPath a (hnode b b') p'
        | VCatL (p', b') -> splitPath a (vnode b b') p'

        // Current branch is right, concatenate the other one to
        // the left.
        | HCatR (a', p') -> splitPath (hnode a' a) b p'
        | VCatR (a', p') -> splitPath (vnode a' a) b p'


/// Map f sequentially over f until some condition is true.
let private mapUntil cond f qr =
    let rec loop qr p =
        match next (QuadRope.map f qr) p with
            | More (qr, p) when cond () ->
                More (splitPath Empty qr p)
            | More (qr, p) -> loop qr p
            | Done qr -> Done qr
    start qr ||> loop


/// LTS map. Apply f to elements of qr in parallel. Spawn new
/// tasks if processors are idle. TODO: Current check is always
/// true.
let rec map f qr =
    match qr with
        | HCat _ ->
            match mapUntil (fun () -> true) f qr with
                | Done qr -> qr
                | More (a, b) -> hnode a (parmap f b)
        | VCat _ ->
            match mapUntil (fun () -> true) f qr with
                | Done qr -> qr
                | More (a, b) -> vnode a (parmap f b)
        | _ ->
            QuadRope.map f qr
/// Split quad rope into two without slicing overhead. Note that
/// resulting quad ropes are probably not of the same size. Let
/// LTS take care of that.
and private parmap f qr =
    match qr with
        | HCat (_, _, _, _, a, b) ->
            par2 (fun () -> map f a) (fun () -> map f b) ||> hnode
        | VCat (_, _, _, _, a, b) ->
            par2 (fun () -> map f a) (fun () -> map f b) ||> vnode
        | _ ->
            QuadRope.map f qr
