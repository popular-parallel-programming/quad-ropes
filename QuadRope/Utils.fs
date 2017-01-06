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
    let par2 f g =
        let ft = task f
        let gres = g()
        await ft
        result ft, gres

    /// Eagerly execute four functions in parallel. This function
    /// blocks the current thread until all functions are computed and
    /// returns their (unwrapped) results.
    let par4 f g h k =
        let ft = task f
        let gt = task g
        let ht = task h
        let kres = k()
        await3 ft gt ht
        result ft, result gt, result ht, kres

    type private State =
        | Done
        | Stop

    let private getDone tasks =
        Array.filter (fun (t : 'a Task) -> t.IsCompleted) tasks

    let private getRunning tasks =
        Array.filter (fun (t : 'a Task) -> not t.IsCompleted) tasks

    let rec private awaitAllUnless p (tasks : 'a Task []) =
        // We wrap tasks in continuation tasks in order to use
        // Task.WaitAny, which does not accept Tasks with return values.
        Task.WaitAny (Array.map (fun (t : 'a Task) -> t.ContinueWith (fun (t : 'a Task) -> ())) tasks)
        |> ignore
        let results = (getDone >> Array.map result) tasks
        if Array.length tasks = Array.length results then
            Done
        else if Array.exists p results then
            Stop
        else
            awaitAllUnless p (getRunning tasks)

    let par2unless e f g =
        let ft = task f
        let gt = task g
        match awaitAllUnless ((=) e) [| ft; gt |] with
            | Done -> result ft, result gt
            | Stop -> e, e

    let par4unless e f g h k =
        let ft = task f
        let gt = task g
        let ht = task h
        let kt = task k
        match awaitAllUnless ((=) e) [| ft; gt; ht; kt |] with
            | Done -> result ft, result gt, result ht, result kt
            | Stop -> e, e, e, e

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
