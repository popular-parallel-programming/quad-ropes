// -*- c-basic-offset: 4; indent-tabs-mode: nil-*-
using System;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

using CommandLine;

namespace RadTrees.Benchmark
{

    class Benchmark
    {
        /// <summary>
        ///   Benchmark an action, no setup.
        /// </summary>
        public static void Mark(string msg, Action f)
        {
	    try {
		Infrastructure.Mark8Setup(msg, i => { f(); return i; });
	    } catch (Exception e) {
                Console.WriteLine(e.ToString());
	    }
        }

        /// <summary>
        ///   Benchmark an action, set the number of threads as setup.
        /// </summary>
        public static void MarkThreads(string msg, int threads, Action f)
        {
	    try {
		Infrastructure.Mark8Setup(msg,
                                          String.Format("{0,2}", threads),
                                          i => { f(); return i; },
                                          () => SetThreads(threads));
	    } catch (Exception e) {
                Console.WriteLine(e.ToString());
	    }
        }

        /// <summary>
        ///   Set maximum allowed threads of the thread pool
        /// </summary>
	public static void SetThreads(int nthreads)
	{
	    // Set min threads first, otherwise setting max threads might fail.
	    System.Threading.ThreadPool.SetMinThreads(1, 1);
	    if (!System.Threading.ThreadPool.SetMaxThreads(nthreads, nthreads))
		Console.WriteLine("# Error: could not change the number of thread pool threads.");
	}

	// Functions of type int * int -> int.
	public static FSharpFunc<int, FSharpFunc<int, int>> times =
	    Utils.Functions.toFunc2<int, int, int>((i, j) => i * j);
	public static FSharpFunc<int, FSharpFunc<int, int>> plus =
	    Utils.Functions.toFunc2<int, int, int>((x, y) => x + y);

	// Functions of type int -> int.
	public static FSharpFunc<int, int> timesTwo =
	    Utils.Functions.toFunc1<int, int>(i => 2 * i);
	public static FSharpFunc<int, int> getZeros =
	    Utils.Functions.toFunc1<int, int>(i => 0);

        /// <summary>
        ///   Repeat a function n times. The function is executed
        ///   first with t on both sides, then the result is used on
        ///   the left side in each successive iteration.
        /// </summary>
        public static T DoNTimes<T>(int n, Func<T, T, T> f, T t)
        {
            T acc = t;
            for (int i = 0; i < n; ++i)
                acc = f(acc, t);
            return acc;
        }

        public static void RunSequentialRope(int size)
        {
	    var rope = QuadRopeModule.init(size, size, times);
            var hzeros = QuadRopeModule.initZeros(size, 1);
            var vzeros = QuadRopeModule.initZeros(1, size);
            MarkThreads("QuadRope.init",    1, () => QuadRopeModule.init(size, size, times));
            MarkThreads("QuadRope.map",     1, () => QuadRopeModule.map(timesTwo, rope));
            MarkThreads("QuadRope.reduce",  1, () => QuadRopeModule.reduce<int>(plus, rope));
            MarkThreads("QuadRope.zip",     1, () => QuadRopeModule.zip(plus, rope, rope));
            MarkThreads("QuadRope.hfold",   1, () => QuadRopeModule.hfold(times, hzeros, rope));
	    MarkThreads("QuadRope.vfold",   1, () => QuadRopeModule.vfold(times, vzeros, rope));
            MarkThreads("QuadRope.hreduce", 1, () => QuadRopeModule.hreduce(times, rope));
	    MarkThreads("QuadRope.vreduce", 1, () => QuadRopeModule.vreduce(times, rope));
//	    MarkThreads("QuadRope.hscan",   1, () => QuadRopeModule.hscan(times, getZeros, rope));
//	    MarkThreads("QuadRope.vscan",   1, () => QuadRopeModule.vscan(times, getZeros, rope));
	}

        public static void RunParallelRope(int size, int threads)
        {
	    var rope = QuadRopeModule.init(size, size, times);
            var hzeros = QuadRopeModule.initZeros(size, 1);
            var vzeros = QuadRopeModule.initZeros(1, size);
            MarkThreads("QuadRope.init",   threads, () => Parallel.QuadRopeModule.init(size, size, times));
            MarkThreads("QuadRope.map",    threads, () => Parallel.QuadRopeModule.map(timesTwo, rope));
            MarkThreads("QuadRope.reduce", threads, () => Parallel.QuadRopeModule.reduce<int>(plus, rope));
            MarkThreads("QuadRope.zip",    threads, () => Parallel.QuadRopeModule.zip(plus, rope, rope));
            MarkThreads("QuadRope.hfold",  threads, () => Parallel.QuadRopeModule.hfold(times, hzeros, rope));
	    MarkThreads("QuadRope.vfold",  threads, () => Parallel.QuadRopeModule.vfold(times, vzeros, rope));
            MarkThreads("QuadRope.hreduce",threads, () => Parallel.QuadRopeModule.hreduce(times, rope));
	    MarkThreads("QuadRope.vreduce",threads, () => Parallel.QuadRopeModule.vreduce(times, rope));
//	    MarkThreads("QuadRope.hscan",  threads,
//                      () => Parallel.QuadRopeModule.hscan(times, getZeros, rope));
//	    MarkThreads("QuadRope.vscan", threads
//                      () => Parallel.QuadRopeModule.vscan(times, getZeros, rope));
	}

	public static void RunSequentialArray(int size)
        {
            var arr = Array2DModule.Initialize(size, size, times);
	    MarkThreads("Array2D.init",    1, () => Array2DModule.Initialize(size, size, times));
            MarkThreads("Array2D.map",     1, () => Array2DModule.Map(timesTwo, arr));
            MarkThreads("Array2D.reduce",  1, () => Array2D.reduce<int>(plus, arr));
            MarkThreads("Array2D.zip",     1, () => Array2D.map2(plus, arr, arr));
            MarkThreads("Array2D.hfold",   1, () => Array2D.fold2(times, getZeros, arr));
	    MarkThreads("Array2D.vfold",   1, () => Array2D.fold1(times, getZeros, arr));
            MarkThreads("Array2D.hreduce", 1, () => Array2D.reduce2(times, arr));
	    MarkThreads("Array2D.vreduce", 1, () => Array2D.reduce1(times, arr));
//	    MarkThreads("Array2D.hscan",   1, () => Array2D.scan2(times, getZeros, arr));
//	    MarkThreads("Array2D.vscan",   1, () => Array2D.scan1(times, getZeros, arr));
        }

        public static void RunParallelArray(int size, int threads)
        {
            var arr = Array2DModule.Initialize(size, size, times);
	    MarkThreads("Array2D.init",    threads, () => Parallel.Array2D.init(size, size, times));
            MarkThreads("Array2D.map",     threads, () => Parallel.Array2D.map(timesTwo, arr));
            MarkThreads("Array2D.reduce",  threads, () => Parallel.Array2D.reduce<int>(plus, arr));
            MarkThreads("Array2D.zip",     threads, () => Parallel.Array2D.map2(plus, arr, arr));
            MarkThreads("Array2D.hfold",   threads, () => Parallel.Array2D.fold2(times, getZeros, arr));
	    MarkThreads("Array2D.vfold",   threads, () => Parallel.Array2D.fold1(times, getZeros, arr));
            MarkThreads("Array2D.hreduce", threads, () => Parallel.Array2D.reduce2(times, arr));
	    MarkThreads("Array2D.vreduce", threads, () => Parallel.Array2D.reduce1(times, arr));
//	    MarkThreads("Array2D.hscan",   threads, () => Parallel.Array2D.scan2(times, getZeros, arr));
//	    MarkThreads("Array2D.vscan",   threads, () => Parallel.Array2D.scan1(times, getZeros, arr));
        }

        /// <summary>
        ///   Starts the correct version (sequential or parallel) of
        ///   functions to benchmark, depending on the set number of
        ///   threads.
        /// </summary>
        public static void Run(Options opts)
        {
            if (opts.Threads == 1) {
                RunSequentialRope(opts.Size);
                RunSequentialArray(opts.Size);
            } else {
                RunParallelRope(opts.Size, opts.Threads);
                RunParallelArray(opts.Size, opts.Threads);
            }
        }

        public static void Indexing(Options opts)
        {
            Random rnd = new Random(892);
            int i = rnd.Next(opts.Size);
            int j = rnd.Next(opts.Size);

            var arr = Array2DModule.Initialize(opts.Size, opts.Size, times);
            Mark("Array2D.get", () => Array2DModule.Get(arr, i, j));
            Mark("Array2D.set", () => Array2D.set(arr, i, j, 0));

            var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
            Mark("QuadRope.get", () => QuadRopeModule.get(rope, i, j));
            Mark("QuadRope.set", () => QuadRopeModule.set(rope, i, j, 0));
        }

        public static void Zip(Options opts)
        {
            var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
            var hrev = QuadRopeModule.hrev(rope);
            var vrev = QuadRopeModule.vrev(rope);
            Mark("QuadRope.zip opt",  () => QuadRopeModule.zip(plus, rope, rope));
            Mark("QuadRope.zip hrev", () => QuadRopeModule.zip(plus, rope, hrev));
            Mark("QuadRope.zip vrev", () => QuadRopeModule.zip(plus, rope, vrev));
        }

        public static void Concatenating(Options opts)
        {
            Random rnd = new Random();

            // Initialize ropes for hcat, hight must be equal.
            Types.QuadRope<int>[] ropes = new Types.QuadRope<int>[100];
            for (int i = 0; i < ropes.Length; ++i)
                ropes[i] = QuadRopeModule.init(opts.Size, rnd.Next(1, opts.Size), times);

            var rope = QuadRopeModule.init(opts.Size, 1, times);
            Mark("QuadRope.hcat x 100", () =>
                    {
                     for (int i = 0; i < 100; ++i)
                         rope = QuadRopeModule.hcat(rope, ropes[i]);
                    });
            Console.WriteLine("# Resulting rope depth is " + QuadRopeModule.depth(rope));
            Console.WriteLine("# Resulting rope is "
                              + (QuadRopeModule.isBalancedH(rope) ? "balanced." : "unbalanced."));

            // Initialize ropes for vcat, width must be equal.
            for (int i = 0; i < ropes.Length; ++i)
                ropes[i] = QuadRopeModule.init(rnd.Next(1, opts.Size), opts.Size, times);

            rope = QuadRopeModule.init(1, opts.Size, times);
            Mark("QuadRope.vcat x 100", () =>
                    {
                     for (int i = 0; i < 100; ++i)
                         rope = QuadRopeModule.vcat(rope, ropes[i]);
                    });
            Console.WriteLine("# Resulting rope depth is " + QuadRopeModule.depth(rope));
            Console.WriteLine("# Resulting rope is "
                              + (QuadRopeModule.isBalancedV(rope) ? "balanced." : "unbalanced."));

        }

        public static void vanDerCorput(Options opts)
        {
            Mark("vdc Array2D", () => Examples.Array2D.vanDerCorput(opts.Size));
            Mark("vdc Array2D.Parallel", () => Examples.Array2D.Parallel.vanDerCorput(opts.Size));
            Mark("vdc QuadRope", () => Examples.QuadRope.vanDerCorput(opts.Size));
            Mark("vdc QuadRope.Parallel", () => Examples.QuadRope.Parallel.vanDerCorput(opts.Size));
        }

        public static void Factorization(Options opts)
        {
            int start = 1000;
            var init = Utils.Functions.toFunc2<int, int, int>((i, j) => i * j + start);
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, init);
            var rope = QuadRopeModule.init(opts.Size, opts.Size, init);
            for (int t = 2; t <= opts.Threads; ++t) {
                MarkThreads("primes Array2D",  t, () => Examples.Array2D.Parallel.factorize(arr));
                MarkThreads("primes QuadRope", t, () => Examples.QuadRope.Parallel.factorize(rope));
            }
        }

        public static void Fibonacci(Options opts)
        {
            MarkThreads("fibseq Array2D",  opts.Threads, () => Examples.Array2D.fibseq(opts.Size));
            MarkThreads("fibseq QuadRope", opts.Threads, () => Examples.QuadRope.fibseq(opts.Size));
        }

        public static void MatrixMultiplication(Options opts)
        {
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, times);
            var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
            MarkThreads("mmult Array2D",      1, () => Examples.Array2D.mmult(arr, arr));
            MarkThreads("mmult QuadRope",     1, () => Examples.QuadRope.mmult(rope, rope));
            for (int t = 2; t <= opts.Threads; ++t) {
                MarkThreads("mmult Array2D",  t, () => Examples.Array2D.Parallel.mmult(arr, arr));
                MarkThreads("mmult QuadRope", t, () => Examples.QuadRope.Parallel.mmult(rope, rope));
            }
        }

        public static void Reallocation(Options opts)
        {
            var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
            Mark("seq reallocate", () => QuadRopeModule.reallocate(rope));
            Mark("par reallocate", () => Parallel.QuadRopeModule.reallocate(rope));
        }

        static Dictionary<string, Action<Options>> tests = new Dictionary<string, Action<Options>>()
        {
            {"all", Run},
            {"index", Indexing},
            {"zip", Zip},
            {"cat", Concatenating},
            {"realloc", Reallocation},
            {"vdc", vanDerCorput},
            {"primes", Factorization},
            {"fibseq", Fibonacci},
            {"mmult", MatrixMultiplication}
        };

        public static void PrintModes()
        {
            foreach(string key in tests.Keys)
                Console.WriteLine("  " + key);
        }

	public static void Main(string[] args)
        {
	    Options opts = new Options();
	    if (Parser.Default.ParseArguments(args, opts)) {
                try {
                    var run = tests[opts.Mode];
                    Infrastructure.SystemInfo();
                    SetThreads(opts.Threads);
                    run(opts);
                } catch (KeyNotFoundException e) {
                    Console.WriteLine("No such mode: \"" + opts.Mode + "\". Available modes:");
                    PrintModes();
                }
            } else {
                Console.WriteLine(opts.ToString());
                Console.WriteLine("Available modes:");
                PrintModes();
            }
        }
    }


}
