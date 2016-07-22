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

    /// Execute two functions f and g in parallel. This function
    /// blocks the current thread until f and g are computed and
    /// returns their (unwrapped) results.
    let par2 f g =
        let ft = task f
        let gres = g()
        await ft
        result ft, gres

    /// Execute four functions in parallel. This function blocks the
    /// current thread until all functions are computed and returns
    /// their (unwrapped) results.
    let par4 f g h k =
        let ft = task f
        let gt = task g
        let ht = task h
        let kres = k()
        await3 ft gt ht
        result ft, result gt, result ht, kres

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
