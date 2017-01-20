module RadTrees.LazySplitting.QuadRope

open RadTrees
open RadTrees.Types

    type ('a, 'b) Progress =
        | More of 'a
        | Done of 'b

    type ('a, 'b) Path when 'a : equality and 'b : equality =
        | Top
        | HCatL of ('a, 'b) Path * 'a QuadRope
        | HCatR of 'b QuadRope * ('a, 'b) Path
        | VCatL of ('a, 'b) Path * 'a QuadRope
        | VCatR of 'b QuadRope * ('a, 'b) Path

    let rec first qr p =
        match qr with
            | HCat (_, _, _, _, a, b) ->
                first a (HCatL (p, b))
            | VCat (_, _, _, _, a, b) ->
                first a (VCatL (p, b))
            | _ -> qr, p

    let start qr = first qr Top

    let rec next qr p =
        match p with
            | Top -> Done qr
            | HCatL (p', b) -> More (first b (HCatR (qr, p')))
            | HCatR (a, p') -> next (hnode a qr) p'
            | VCatL (p', b) -> More (first b (VCatR (qr, p')))
            | VCatR (a, p') -> next (vnode a qr) p'

    let rec splitPath pqr uqr p =
        match p with
            | Top -> pqr, uqr
            | HCatL (p', b) -> splitPath pqr (hnode uqr b) p'
            | HCatR (a, p') -> splitPath (hnode pqr a) uqr p'
            | VCatL (p', b) -> splitPath pqr (vnode uqr b) p'
            | VCatR (a, p') -> splitPath (vnode pqr a) uqr p'

    let mapUntil cond f qr =
        let rec loop qr p =
            match next (QuadRope.map f qr) p with
                | More (qr, p) when cond () ->
                    More (splitPath Empty qr p)
                | More (qr, p) -> loop qr p
                | Done qr -> Done qr
        start qr ||> loop

    let rec map f qr =
        match qr with
            | HCat _ ->
                match mapUntil (fun () -> true) f qr with
                    | Done qr -> qr
                    | More (pqr, uqr) -> hnode pqr (parmap f uqr)
            | VCat _ ->
                match mapUntil (fun () -> true) f qr with
                    | Done qr -> qr
                    | More (pqr, uqr) -> vnode pqr (parmap f uqr)
            | _ ->
                QuadRope.map f qr
    and parmap f qr =
        match qr with
            | HCat (_, _, _, _, a, b) ->
                par2 (fun () -> map f a) (fun () -> map f b) ||> hnode
            | VCat (_, _, _, _, a, b) ->
                par2 (fun () -> map f a) (fun () -> map f b) ||> vnode
            | _ ->
                QuadRope.map f qr
