namespace RadTrees.QuadRope

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

    let rec pmap f = function
        | Node (_, _, _, ne, nw, Empty, Empty) ->
            let ne0 = task (fun () -> pmap f ne)
            let nw0 = task (fun () -> pmap f nw)
            await2 ne0 nw0
            flatNode (result nw0) (result ne0)
        | Node (_, _, _, Empty, nw, sw, Empty) ->
            let nw0 = task (fun () -> pmap f nw)
            let sw0 = task (fun () -> pmap f sw)
            await2 nw0 sw0
            thinNode (result nw0) (result sw0)
        | Node (_, _, _, ne, nw, sw, se) ->
            let ne0 = task (fun () -> pmap f ne)
            let nw0 = task (fun () -> pmap f nw)
            let sw0 = task (fun () -> pmap f sw)
            let se0 = task (fun () -> pmap f se)
            await4 ne0 nw0 sw0 se0
            node (result ne0) (result nw0) (result sw0) (result se0)
        | rope -> map f rope
