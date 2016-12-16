// Copyright (c) 2016 Florian Biermann, fbie@itu.dk

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:

// * The above copyright notice and this permission notice shall be
//   included in all copies or substantial portions of the Software.

// * The software is provided "as is", without warranty of any kind,
//   express or implied, including but not limited to the warranties of
//   merchantability, fitness for a particular purpose and
//   noninfringement. In no event shall the authors or copyright holders be
//   liable for any claim, damages or other liability, whether in an action
//   of contract, tort or otherwise, arising from, out of or in connection
//   with the software or the use or other dealings in the software.

module RadTrees.Utils

module Tasks =
    open System.Threading.Tasks

    let inline private task (f : unit -> 'a) =
        Task<'a>.Factory.StartNew(System.Func<'a>(f))

    let inline private result (t : _ Task) =
        t.Result

    let inline private await  (t0 : _ Task) =
        Task.WaitAll(t0)

    let inline private await2 (t0 : _ Task) t1 =
        Task.WaitAll(t0, t1)

    let inline private await3 (t0 : _ Task) t1 t2 =
        Task.WaitAll(t0, t1, t2)

    let inline private await4 (t0 : _ Task) t1 t2 t3 =
        Task.WaitAll(t0, t1, t2, t3)

    /// Eagerly execute two functions f and g in parallel. This
    /// function blocks the current thread until f and g are computed
    /// and returns their (unwrapped) results.
    let epar2 f g =
        let ft = task f
        let gres = g()
        await ft
        result ft, gres

    /// Eagerly execute four functions in parallel. This function
    /// blocks the current thread until all functions are computed and
    /// returns their (unwrapped) results.
    let epar4 f g h k =
        let ft = task f
        let gt = task g
        let ht = task h
        let kres = k()
        await3 ft gt ht
        result ft, result gt, result ht, kres

    // Globally shared state for determining whether threadpool
    // threads are idle.
    let mutable private idleCount = 0UL
    let mutable private lastIdle = idleCount

    /// Push a task to the work queue to determine whether there is an
    /// idle thread. The idea is that this only gets executed if there
    /// is no other work to do on the queue. It is unlikely that this
    /// is the last item on the queue, but the heuristics may work
    /// well enough for practical purposes.
    let private pushIdleTask() =
        if lastIdle = idleCount then
            task (fun () -> idleCount <- idleCount + 1UL) |> ignore
        else
            ()

    /// True if a workpool thread is heuristically idle, false
    /// otherwise. This is non-deterministic.
    let private isIdle() =
        if lastIdle < idleCount then
            lastIdle <- idleCount
            true
        else
            false

    /// Maybe execute two lambda expressions in parallel and return
    /// their (unwrapped) results.
    let lazypar2 f g =
        if isIdle() then
            pushIdleTask()
            epar2 f g
        else
            f(), g()

    /// Maybe execute four lambda expressions in parallel and return
    /// their (unwrapped) results.
    let lazypar4 f g h k =
        let f', g' = epar2 f g
        let h', k' = epar2 h k
        f', g', h', k'

    // References to functions make it easy to switch between
    // implementations.
    let par2 = lazypar2
    let par4 = lazypar4

    /// Get the number of maximum threads set0.
    let numthreads() =
        let mutable workers = 0
        let mutable completions = 0
        System.Threading.ThreadPool.GetMaxThreads(&workers, &completions)
        workers

    /// Wrapper for System.Threading.Tasks.Parallel.For
    let parfor l u f =
        System.Threading.Tasks.Parallel.For(l, u, System.Action<int> f) |> ignore

module Bits =

    let inline radix bits =
        1 <<< bits

    let inline mask bits =
        (radix bits) - 1

    let inline index bits depth i =
        (i >>> (depth * bits)) &&& mask bits

module Fibonacci =

    /// Alternative using plain arrays
    /// Enough to use 47 elements since fib 46 < 2^31 < fib 47.

    /// With this code, fib 33 is 17 times faster for some reason.

    let private fibs = Array.init 47 id

    for i in [2 .. Array.length fibs - 1] do
        fibs.[i] <- fibs.[i-1] + fibs.[i-2]

    let fib n =
        fibs.[n]

    /// Return the n of the first Fibonacci number that is greater than m.
    let nth m =
        Array.findIndex ((<=) m) fibs

/// This module contains a bunch of functions that convert a C#
/// function into an F# function conveniently. More versions for
/// even more parameters will probably be added in the future.
module Functions =
    open System

    let inline toFunc0 (f : 'a Func) =
        let f0 () =
            f.Invoke()
        f0

    let inline toFunc1 (f : ('b0, 'a) Func) =
        let f1 b0 =
            f.Invoke(b0)
        f1

    let inline toFunc2 (f : ('b0, 'b1, 'a) Func) =
        let f2 b0 b1 =
            f.Invoke(b0, b1)
        f2

    let inline toFunc3 (f : ('b0, 'b1, 'b2, 'a) Func) =
        let f3 b0 b1 b2 =
            f.Invoke(b0, b1, b2)
        f3

    let inline toFunc4 (f : ('b0, 'b1, 'b2, 'b3, 'a) Func) =
        let f4 b0 b1 b2 b3 =
            f.Invoke(b0, b1, b2, b3)
        f4
