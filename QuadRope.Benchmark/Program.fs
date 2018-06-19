﻿// Copyright (c) 2017 Florian Biermann, fbie@itu.dk

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

module QuadRope.Benchmark

open CommandLine
open LambdaMicrobenchmarking

open QuadRope.Types
open QuadRope.Examples


/// Benchmark a thunk with a message.
let benchmark (message, (thunk : unit -> 'a )) =
    Script.Of (message, System.Func<'a> thunk)


/// Add another thunk with a message to the script.
let (&>) (script : 'a Script) (message, (thunk : unit -> 'a )) =
    script.Of (message, System.Func<'a> thunk)


/// Run the benchmark script.
let run (script : _ Script) =
    script.RunAll() |> ignore


/// Run the benchmark script and print header beforehand.
let runWithHead (script : _ Script) =
    script.WithHead() |> run


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

        benchmark ("QuadRope.init",   fun () -> QuadRope.init opts.size opts.size (*))
               &> ("QuadRope.map",    fun () -> QuadRope.map (fun x -> x * x) rope)
               &> ("QuadRope.zip",    fun () -> QuadRope.zip (+) rope rope)
               &> ("QuadRope.scan",   fun () -> QuadRope.scan (fun a b c d -> a + b + c + d) 0 rope)
               &> ("QuadRope.hrev",   fun () -> QuadRope.hrev rope)
               &> ("QuadRope.vrev",   fun () -> QuadRope.vrev rope)
               &> ("QuadRope.transpose", fun () -> QuadRope.transpose rope)
               |> runWithHead
        benchmark ("QuadRope.reduce", fun () -> QuadRope.reduce (+) 0 rope) |> run

        let arr = Array2D.init opts.size opts.size (*)
        benchmark ("Array2D.init",    fun () -> Array2D.init opts.size opts.size (*))
               &> ("Array2D.map",     fun () -> Array2D.map (fun x -> x * x) arr)
               &> ("Array2D.zip",     fun () -> Array2DExt.map2 (+) arr arr)
               &> ("Array2D.scan",    fun () -> Array2DExt.scan (fun a b c d -> a + b + c + d) 0 arr)
               &> ("Array2D.hrev",    fun () -> Array2DExt.rev2 arr)
               &> ("Array2D.vrev",    fun () -> Array2DExt.rev1 arr)
               &> ("Array2D.transpose", fun () -> Array2DExt.transpose arr)
               |> run
        benchmark ("Array2D.reduce",  fun () -> Array2DExt.reduce (+) arr) |> run


    /// Benchmark parallel functions.
    let allPar (opts : Options) =
        let rope = QuadRope.init opts.size opts.size (*)
        benchmark ("QuadRope.init",   fun () -> Parallel.QuadRope.init opts.size opts.size (*))
               &> ("QuadRope.map",    fun () -> Parallel.QuadRope.map (fun x -> x * x) rope)
               &> ("QuadRope.zip",    fun () -> Parallel.QuadRope.zip (+) rope rope)
               &> ("QuadRope.scan",   fun () -> Parallel.QuadRope.scan (fun a b c d -> a + b + c + d) 0 rope)
               |> runWithHead
        benchmark ("QuadRope.reduce", fun () -> Parallel.QuadRope.reduce (+) 0 rope) |> run

        let arr = Array2D.init opts.size opts.size (*)
        benchmark ("Array2D.init",    fun () -> Parallel.Array2DExt.init opts.size opts.size (*))
               &> ("Array2D.map",     fun () -> Parallel.Array2DExt.map (fun x -> x * x) arr)
               &> ("Array2D.zip",     fun () -> Parallel.Array2DExt.map2 (+) arr arr)
               |> run
        benchmark ("Array2D.reduce",  fun () -> Parallel.Array2DExt.reduce (+) arr) |> run


    // Don't run sequentially with task overhead.
    if opts.threads = 1 then
        allSeq opts
    else
        allPar opts


/// Benchmark van Der Corput sequence computation.
let vanDerCorput (opts : Options) =
    if opts.threads = 1 then
        benchmark ("QuadRope.vdc", fun () -> VanDerCorput.QuadRope.vanDerCorput opts.size) |> runWithHead
        benchmark ("Array2D.vdc",  fun () -> VanDerCorput.Array2D.vanDerCorput opts.size) |> run
    else
        benchmark ("QuadRope.vdc", fun () -> VanDerCorput.QuadRope.vanDerCorputPar opts.size) |> runWithHead
        benchmark ("Array2D.vdc",  fun () -> VanDerCorput.Array2D.vanDerCorputPar opts.size) |> run


/// Benchmark matrix multiplication algorithms.
let matMult (opts: Options) =
    // Array matrices
    let arr1 = Array2D.init opts.size opts.size (fun i j -> if i < j then 0.0 else 1.0)
    let arr2 = Array2D.init opts.size opts.size (fun i j -> float (i * j))

    // Quad rope matrices
    let ud = QuadRope.init opts.size opts.size (fun i j -> if i < j then 0.0 else 1.0)
    let us = QuadRope.SparseDouble.upperDiagonal opts.size 1.0
    let qr = QuadRope.init opts.size opts.size (fun i j -> float (i * j))

    if opts.threads = 1 then
        benchmark ("QuadRope.mmult dense",  fun () -> MatMult.QuadRope.mmult ud qr)
               &> ("QuadRope.mmult sparse", fun () -> MatMult.QuadRope.mmult us qr)
               // &> ("QuadRope.mmult imp",    fun () -> MatMult.ImperativeQuadRope.mmult ud qr)
               |> runWithHead
        benchmark ("Array2D.mmult",      fun () -> MatMult.Array2D.mmult arr1 arr2)
               // &> ("Array2D.imperative", fun () -> MatMult.Imperative.mmult arr1 arr2)
               |> run
    else
        benchmark ("QuadRope.mmult dense",      fun () -> MatMult.QuadRope.mmultPar ud qr)
               // &> ("QuadRope.mmult dense opt",  fun () -> MatMult.QuadRope.mmultOpt ud qr)
               &> ("QuadRope.mmult sparse",     fun () -> MatMult.QuadRope.mmultPar us qr)
               // &> ("QuadRope.mmult sparse opt", fun () -> MatMult.QuadRope.mmultOpt us qr)
               // &> ("QuadRope.mmult imp",        fun () -> MatMult.ImperativeQuadRope.mmultPar ud qr)
               |> runWithHead
        // benchmark ("Array2D.mmult",      fun () -> MatMult.Array2D.mmultPar arr1 arr2)
               // &> ("Array2D.imperative", fun () -> MatMult.Imperative.mmultPar arr1 arr2)
               // |> run


/// Benchmark computing Fibonacci sequences.
let fibonacci (opts : Options) =
    benchmark ("QuadRope.fibonacci", fun () -> Fibonacci.QuadRope.fibseq opts.size) |> runWithHead
    benchmark ("Array2D.fibonacci", fun () -> Fibonacci.Array2D.fibseq opts.size) |> run


/// Benchmark integer factorization.
let factorize (opts : Options) =
    let qr = QuadRope.init opts.size opts.size (fun i j -> 1000 + i * j)
    let arr = Array2D.init opts.size opts.size (fun i j -> 1000 + i * j)
    if opts.threads = 1 then
        benchmark ("QuadRope.factorize", fun () -> Factorize.QuadRope.factorize qr) |> runWithHead
        benchmark ("Array2D.factorize",  fun () -> Factorize.Array2D.factorize arr) |> run
    else
        benchmark ("QuadRope.factorize", fun () -> Factorize.QuadRope.factorizePar qr) |> runWithHead
        // benchmark ("Array2D.factorize",  fun () -> Factorize.Array2D.factorizePar arr) |> run


/// Benchmark the sieve of Erastothenes.
let sieve (opts : Options) =
    benchmark ("QuadRope.sieve", fun () -> Sieve.QuadRope.sieve opts.size) |> runWithHead
    benchmark ("Array2D.sieve",  fun () -> Sieve.Array2D.sieve opts.size) |> run


/// Benchmark index operations.
let index (opts : Options) =
    let qr = QuadRope.init opts.size opts.size (*)
    let arr = Array2D.init opts.size opts.size (*)
    let idx = opts.size / 2
    benchmark ("QuadRope.get", fun () -> QuadRope.get qr idx idx)
           &> ("Array2D.get",  fun () -> Array2D.get arr idx idx) |> runWithHead
    benchmark ("QuadRope.set", fun () -> QuadRope.set qr idx idx 0) |> run
    benchmark ("Array2D.set",  fun () -> Array2DExt.set arr idx idx 0) |> run


/// Benchmark Smith-Waterman alignment algorithm.
let alignment (opts : Options) =
    let rnd = System.Random 85491
    let acgt = "ACGT"
    let mkstr() = String.init opts.size (fun _ -> string acgt.[rnd.Next(acgt.Length)])
    let a = mkstr()
    let b = mkstr()

    if opts.threads = 1 then
        benchmark ("QuadRope.align", fun () -> SmithWaterman.QuadRope.align a b) |> runWithHead
        benchmark ("Array2D.align",  fun () -> SmithWaterman.Array2D.align a b) |> run
    else
        benchmark ("QuadRope.align", fun () -> SmithWaterman.QuadRope.alignPar a b) |> runWithHead
        // printfn "Array2D.align #Not available."


/// Benchmark Conway's Game of Life.
let gameOfLife (opts : Options) =
    let rnd = System.Random(987)
    let world = QuadRope.init opts.size opts.size (fun _ _ -> rnd.Next(0, 2))
    if opts.threads = 1 then
        benchmark ("QuadRope.gameOfLife x 100", fun () -> GameOfLife.QuadRope.gameOfLife 100 world)
        |> runWithHead
        let arrayWorld = QuadRope.toArray2D world
        benchmark ("Array2D.gameOfLife x 100", fun () -> GameOfLife.Array2D.gameOfLife 100 arrayWorld)
        |> run
    else
        benchmark ("QuadRope.gameOfLife x 100", fun () -> GameOfLife.QuadRope.gameOfLifePar 100 world)
        |> runWithHead


let batcher (opts : Options) =
    let rnd = System.Random(987) in
    let numbers = QuadRope.init (pown 2 opts.size) 1 (fun _ _ -> rnd.Next(0, System.Int32.MaxValue)) in
    let array_numbers = QuadRope.toArray2D numbers in
    if opts.threads = 1 then
        benchmark ("QuadRope.batcher", fun () -> Batcher.QuadRope.batcher numbers)         |> runWithHead;
        benchmark ("Array2D.batcher",  fun () -> Batcher.Array2D.batcher array_numbers)    |> run
    else
        benchmark ("QuadRope.batcher", fun () -> Batcher.QuadRope.batcherPar numbers)      |> runWithHead;
        benchmark ("Array2D.batcher",  fun () -> Batcher.Array2D.batcherPar array_numbers) |> run


/// A map of benchmark functions and their names. If I had time, this
/// could be done with attributes instead.
let benchmarks =
    [ "all", all;
      "vdc", vanDerCorput;
      "mmult", matMult;
      "fibs", fibonacci;
      "primes", factorize;
      "sieve", sieve;
      "index", index;
      "align", alignment;
      "gol", gameOfLife;
      "batcher", batcher] |> Collections.Map


/// Print all registered benchmarks.
let printBenchmarks () =
    printfn "Available benchmarks:\n%s"
            ((Map.toSeq >> Seq.map fst >> String.concat "\n") benchmarks)


/// Set the number of thread pool threads to t.
let setThreads t =
    // Set min threads first, otherwise setting max threads might fail.
    let bits = if t > System.IntPtr.Size * 8 then -1L else (1L <<< t) - 1L in
    System.Diagnostics.Process.GetCurrentProcess().ProcessorAffinity <- System.IntPtr(bits);
    let threads = t * 2 in
    System.Threading.ThreadPool.SetMinThreads(threads, threads) |> ignore
    if not (System.Threading.ThreadPool.SetMaxThreads(threads, threads)) then
        failwith "# Error: could not change the number of thread pool threads."
    else
        printfn "# Word size: %d bits" <| System.IntPtr.Size * 8;
        printfn "# Processors: %d" t;


/// True if we are running on mono.
let isMono = not (isNull (System.Type.GetType "Mono.Runtime"))


/// Run the benchmark that is required by command line args.
let runBenchmark (opts : Options) =
    if not isMono then
        setThreads opts.threads
    else if opts.threads <> 1 then
        printfn "# Warning: cannot change thread pool thread count on Mono."

    Script.WarmupIterations <- 3
    Script.Iterations <- 10
    match Map.tryFind opts.mode benchmarks with
        | Some f -> f opts; 0
        | None -> printfn "No such benchmark: %s" opts.mode; 1


[<EntryPoint>]
let main argv =
    match Parser.Default.ParseArguments<Options> argv with
        | :? Parsed<Options> as parsed -> runBenchmark parsed.Value
        | _ -> printBenchmarks(); 1
