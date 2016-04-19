namespace RadTrees.QuadRope

[<RequireQualifiedAccessAttribute>]
module Parallel =
    open System.Threading
    open System.Threading.Tasks

    open RadTrees

    let inline private task f =
        Task<_ QuadRope>.Factory.StartNew(System.Func<_>(f))

    let inline private result (t : _ Task) =
        t.Result

    let inline private await2 (t0 : _ Task) t1 =
        Task.WaitAll(t0, t1)

    let inline private await4 (t0 : _ Task) t1 t2 t3 =
        Task.WaitAll(t0, t1, t2, t3)

    let private par2 f g =
        let ft = task f
        let gt = task g
        await2 ft gt
        result ft, result gt

    let private par4 f g h k =
        let ft = task f
        let gt = task g
        let ht = task h
        let kt = task k
        await4 ft gt ht kt
        result ft, result gt, result ht, result kt

    /// Apply a function f to all scalars in parallel.
    let rec map f = function
        | Node (_, _, _, ne, nw, Empty, Empty) ->
            let ne0, nw0 = par2 (fun () -> map f ne) (fun () -> map f nw)
            flatNode nw0 ne0
        | Node (_, _, _, Empty, nw, sw, Empty) ->
            let nw0, sw0 = par2 (fun () -> map f nw) (fun () -> map f sw)
            thinNode nw0 sw0
        | Node (_, _, _, ne, nw, sw, se) ->
            let ne0, nw0, sw0, se0 = par4 (fun () -> map f ne)
                                          (fun () -> map f nw)
                                          (fun () -> map f sw)
                                          (fun () -> map f se)
            node ne0 nw0 sw0 se0
        | rope -> QuadRope.map f rope

    /// Apply f to all scalars in parallel and reduce the results
    /// row-wise using g.
    let rec mapHreduce f g = function
        | Node (_, _, _, ne, nw, Empty, Empty) ->
            let ne0, nw0 = par2 (fun () -> mapHreduce f g ne) (fun () -> mapHreduce f g nw)
            match ne0 with
                | Empty -> nw0
                | _ -> zip g nw0 ne0
        | Node (_, _, _, Empty, nw, sw, Empty) ->
            let nw0, sw0 = par2 (fun () -> mapHreduce f g nw) (fun () -> mapHreduce f g sw)
            thinNode nw0 sw0
        | Node (_, _, _, ne, nw, sw, se) ->
            let ne0, nw0, sw0, se0 = par4 (fun () -> mapHreduce f g ne)
                                          (fun () -> mapHreduce f g nw)
                                          (fun () -> mapHreduce f g sw)
                                          (fun () -> mapHreduce f g se)
            let w = thinNode nw0 sw0
            let e = thinNode ne0 se0
            zip g w e
        | rope -> QuadRope.mapHreduce f g rope

    /// Apply f to all scalars in parallel and reduce the results
    /// column-wise using g.
    let rec mapVreduce f g = function
        | Node (_, _, _, ne, nw, Empty, Empty) ->
            let ne0, nw0 = par2 (fun () -> mapVreduce f g ne) (fun () -> mapVreduce f g nw)
            flatNode nw0 ne0
        | Node (_, _, _, Empty, nw, sw, Empty) ->
            let nw0, sw0 = par2 (fun () -> mapVreduce f g nw) (fun () -> mapVreduce f g sw)
            match sw0 with
                | Empty -> nw0
                | _ -> zip g nw0 sw0
        | Node (_, _, _, ne, nw, sw, se) ->
            let ne0, nw0, sw0, se0 = par4 (fun () -> mapHreduce f g ne)
                                          (fun () -> mapHreduce f g nw)
                                          (fun () -> mapHreduce f g sw)
                                          (fun () -> mapHreduce f g se)
            let n = flatNode nw0 ne0
            let s = flatNode sw0 se0
            zip g n s
        | rope -> QuadRope.mapHreduce f g rope

    /// Reduce all rows of rope by f.
    let hreduce f rope = mapHreduce id f rope

    /// Reduce all columns of rope by f.
    let vreduce f rope = mapVreduce id f rope
