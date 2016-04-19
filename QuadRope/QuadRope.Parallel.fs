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

    /// Generate a new tree in parallel.
    let init h w f =
        let rec init0 h0 w0 h1 w1 =
            let h = h1 - h0
            let w = w1 - w0
            if h <= 0 || w <= 0 then
                Empty
            else if h <= h_max && w <= w_max then
                leaf (ViewArray2D.init h w (fun i j -> f (h0 + i) (w0 + j)))
            else if w <= w_max then
                let hpv = h0 + h / 2
                let n, s = par2 (fun () -> init0 h0 w0 hpv w1) (fun () -> init0 hpv w0 h1 w1)
                thinNode n s
            else if h <= h_max then
                let wpv = w0 + w / 2
                let w, e = par2 (fun () -> init0 h0 w0 h1 wpv) (fun () -> init0 h0 wpv h1 w1)
                flatNode w e
            else
                let hpv = h0 + h / 2
                let wpv = w0 + w / 2
                let ne, nw, sw, se = par4 (fun () -> init0 h0 wpv hpv w1)
                                          (fun () -> init0 h0 w0 hpv wpv)
                                          (fun () -> init0 hpv w0 h1 wpv)
                                          (fun () -> init0 hpv wpv h1 w1)
                node ne nw sw se
        init0 0 0 h w

    /// Reallocate a rope form the ground up in parallel. Sometimes,
    /// this is the only way to improve performance of a badly
    /// composed quad rope.
    let inline reallocate rope =
        init (rows rope) (cols rope) (get rope)

    /// Initialize a rope in parallel where all elements are
    /// <code>e</code>.
    let inline initAll h w e =
        init h w (fun _ _ -> e)

    /// Initialize a rope in parallel with all zeros.
    let inline initZeros h w = initAll h w 0

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
