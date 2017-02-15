// Copyright (c) 2017 Florian Biermann, fbie@itu.dk

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

module RadTrees.Benchmark

open CommandLine
open LambdaMicrobenchmarking

open RadTrees
open Types
open Examples

/// Set the number of thread pool threads to t.
let setThreads t =
    // Set min threads first, otherwise setting max threads might fail.
    System.Threading.ThreadPool.SetMinThreads(1, 1) |> ignore
    if not (System.Threading.ThreadPool.SetMaxThreads(t, t)) then
        failwith "# Error: could not change the number of thread pool threads."

/// Options for command line.
type Options = {
  [<Option('m', "mode", Required = true, HelpText = "Which benchmark to run.")>] mode : string;
  [<Option('s', "size", Default = 100, HelpText = "Input size.")>] size : int;
  [<Option('t', "threads", Default = 1, HelpText = "Number of threads.")>] threads : int }

/// Benchmark base functions.
let all (opts : Options) =
    /// Benchmark sequential functions.
    let allSeq (opts : Options) =
        let rope = QuadRope.init opts.size opts.size (*)
        Script.Of("QuadRope.init",    fun () -> QuadRope.init opts.size opts.size (*))
              .Of("QuadRope.map",     fun () -> QuadRope.map (fun x -> x * x) rope)
              .Of("QuadRope.zip",     fun () -> QuadRope.zip (+) rope rope)
              .WithHead().RunAll() |> ignore
        Script.Of("QuadRope.reduce",  fun () -> QuadRope.reduce (+) 0 rope).RunAll() |> ignore

        let arr = Array2D.init opts.size opts.size (*)
        Script.Of("Array2D.init",    fun () -> Array2D.init opts.size opts.size (*))
              .Of("Array2D.map",     fun () -> Array2D.map (fun x -> x * x) arr)
              .Of("Array2D.zip",     fun () -> Array2D.map2 (+) arr arr)
              .RunAll() |> ignore
        Script.Of("Array2D.reduce",  fun () -> Array2D.reduce (+) arr).RunAll() |> ignore


    /// Benchmark parallel functions.
    let allPar (opts : Options) =
        let rope = QuadRope.init opts.size opts.size (*)
        Script.Of("QuadRope.init",    fun () -> Parallel.QuadRope.init opts.size opts.size (*))
              .Of("QuadRope.map",     fun () -> Parallel.QuadRope.map (fun x -> x * x) rope)
              .Of("QuadRope.zip",     fun () -> Parallel.QuadRope.zip (+) rope rope)
              .WithHead().RunAll() |> ignore
        Script.Of("QuadRope.reduce",  fun () -> Parallel.QuadRope.reduce (+) 0 rope).RunAll() |> ignore

        let arr = Array2D.init opts.size opts.size (*)
        Script.Of("Array2D.init",    fun () -> Parallel.Array2D.init opts.size opts.size (*))
              .Of("Array2D.map",     fun () -> Parallel.Array2D.map (fun x -> x * x) arr)
              .Of("Array2D.zip",     fun () -> Parallel.Array2D.map2 (+) arr arr)
              .RunAll() |> ignore
        Script.Of("Array2D.reduce",  fun () -> Parallel.Array2D.reduce (+) arr).RunAll() |> ignore


    // Don't run sequentially with task overhead.
    if opts.threads = 1 then
        allSeq opts
    else
        allPar opts


/// Benchmark van Der Corput sequence computation.
let vanDerCorput (opts : Options) =
    Script.Of("QuadRope.vdc", fun () -> VanDerCorput.QuadRope.vanDerCorput opts.size)
          .WithHead().RunAll() |> ignore
    Script.Of("Array2D.vdc",  fun () -> VanDerCorput.Array2D.vanDerCorput opts.size)
          .RunAll() |> ignore


/// Benchmark matrix multiplication algorithms.
let matMult (opts: Options) =
    // Array matrices
    let arr1 = Array2D.init opts.size opts.size (fun i j -> float (i * j))
    let arr2 = Array2D.init opts.size opts.size (fun i j -> if i < j then 0.0 else 1.0)

    // Quad rope matrices
    let qr = QuadRope.init opts.size opts.size (fun i j -> float (i * j))
    let ud = QuadRope.init opts.size opts.size (fun i j -> if i < j then 0.0 else 1.0)
    let us = QuadRope.SparseDouble.upperDiagonal opts.size 1.0

    if opts.threads = 1 then
        Script.Of("QuadRope.mmult dense",  fun () -> MatMult.QuadRope.mmult qr ud)
              .Of("QuadRope.mmult sparse", fun () -> MatMult.QuadRope.mmult qr us)
              .WithHead().RunAll() |> ignore
        Script.Of("Array2D.mmult",      fun () -> MatMult.Array2D.mmult arr1 arr2)
              .Of("Array2D.imperative", fun () -> MatMult.Imperative.mmult arr1 arr2)
              .RunAll() |> ignore
    else
        Script.Of("QuadRope.mmult dense",  fun () -> MatMult.QuadRope.mmultPar qr ud)
              .Of("QuadRope.mmult sparse", fun () -> MatMult.QuadRope.mmultPar qr us)
              .WithHead().RunAll() |> ignore
        Script.Of("Array2D.mmult",      fun () -> MatMult.Array2D.mmultPar arr1 arr2)
              .Of("Array2D.imperative", fun () -> MatMult.Imperative.mmultPar arr1 arr2)
              .RunAll() |> ignore


/// Benchmark computing Fibonacci sequences.
let fibonacci (opts : Options) =
    Script.Of("QuadRope.fibonacci", fun () -> Fibonacci.QuadRope.fibseq opts.size)
          .WithHead().RunAll() |> ignore
    Script.Of("Array2D.fibonacci", fun () -> Fibonacci.Array2D.fibseq opts.size)
          .RunAll() |> ignore


/// Benchmark integer factorization.
let factorize (opts : Options) =
    let qr = QuadRope.init opts.size opts.size (fun i j -> 1000 + i * j)
    let arr = Array2D.init opts.size opts.size (fun i j -> 1000 + i * j)
    if opts.threads = 1 then
        Script.Of("QuadRope.factorize", fun () -> Factorize.QuadRope.factorize qr)
              .WithHead().RunAll() |> ignore
        Script.Of("Array2D.factorize",  fun () -> Factorize.Array2D.factorize arr)
              .RunAll() |> ignore
    else
        Script.Of("QuadRope.factorize", fun () -> Factorize.QuadRope.factorizePar qr)
              .WithHead().RunAll() |> ignore
        Script.Of("Array2D.factorize",  fun () -> Factorize.Array2D.factorizePar arr)
              .RunAll() |> ignore


/// Benchmark the sieve of Erastothenes.
let sieve (opts : Options) =
    Script.Of("QuadRope.sieve", fun () -> Sieve.QuadRope.sieve opts.size).WithHead().RunAll() |> ignore
    Script.Of("Array2D.sieve",  fun () -> Sieve.Array2D.sieve opts.size).RunAll() |> ignore


/// Benchmark index operations.
let index (opts : Options) =
    let qr = QuadRope.init opts.size opts.size (*)
    let arr = Array2D.init opts.size opts.size (*)
    let idx = opts.size / 2
    Script.Of("QuadRope.get", fun () -> QuadRope.get qr idx idx)
          .Of("Array2D.get",  fun () -> Array2D.get arr idx idx).WithHead().RunAll() |> ignore


/// A map of benchmark functions and their names. If I had time, this
/// could be done with attributes instead.
let benchmarks =
    [ "all", all;
      "vdc", vanDerCorput;
      "mmult", matMult;
      "fibs", fibonacci;
      "primes", factorize;
      "sieve", sieve;
      "index", index ] |> Collections.Map


/// True if we are running on mono.
let isMono = System.Type.GetType "Mono.Runtime" <> null


/// Run the benchmark that is required by command line args.
let runBenchmark (opts : Options) =
    if not isMono then
        setThreads opts.threads
    else if opts.threads <> 1 then
        printfn "# Warning: cannot change thread pool thread count on Mono."

    Script.WarmupIterations <- 3
    Script.Iterations <- 50
    (benchmarks.[opts.mode]) opts

[<EntryPoint>]
let main argv =
    match Parser.Default.ParseArguments<Options> argv with
        | :? Parsed<Options> as parsed -> runBenchmark parsed.Value
        | :? NotParsed<Options> as notParsed -> failwithf "%A" notParsed.Errors
    0
