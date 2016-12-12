// -*- c-basic-offset: 4; indent-tabs-mode: nil-*-

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
        ///   Benchmark an action, log size of work.
        /// </summary>
        public static void MarkWork(string msg, int work, Action f)
        {
            try {
                Infrastructure.Mark8Setup(msg,
                                          String.Format("{0,2}", work),
                                          i => { f(); return i; });
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


	public static FSharpFunc<int, FSharpFunc<int, double>> timesInt =
	    Utils.Functions.toFunc2<int, int, double>((x, y) => (double)(x * y));
        public static FSharpFunc<double, FSharpFunc<double, double>> times =
	    Utils.Functions.toFunc2<double, double, double>((x, y) => x * y);
        public static FSharpFunc<int, FSharpFunc<int, double>> plusInt =
	    Utils.Functions.toFunc2<int, int, double>((x, y) => (double)(x + y));
	public static FSharpFunc<double, FSharpFunc<double, double>> plus =
	    Utils.Functions.toFunc2<double, double, double>((x, y) => x + y);
	public static FSharpFunc<double, double> square =
            Utils.Functions.toFunc1<double, double>(i => i * i);
	public static FSharpFunc<int, double> getZeros =
            Utils.Functions.toFunc1<int, double>(i => 0.0);
	public static FSharpFunc<int, FSharpFunc<int, double>> getZeros2 =
            Utils.Functions.toFunc2<int, int, double>((i, j) => 0.0);

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
	    var rope = QuadRopeModule.init(size, size, timesInt);
            var hzeros = QuadRopeModule.init(size, 1, getZeros2);
            var vzeros = QuadRopeModule.init(1, size, getZeros2);
            MarkThreads("QuadRope.init",    1, () => QuadRopeModule.init(size, size, timesInt));
            MarkThreads("QuadRope.map",     1, () => QuadRopeModule.map(square, rope));
            MarkThreads("QuadRope.reduce",  1, () => QuadRopeModule.reduce<double>(plus, 0.0, rope));
            MarkThreads("QuadRope.zip",     1, () => QuadRopeModule.zip(plus, rope, rope));
            MarkThreads("QuadRope.hreduce", 1, () => QuadRopeModule.hreduce(times, 1.0, rope));
	    MarkThreads("QuadRope.vreduce", 1, () => QuadRopeModule.vreduce(times, 1.0, rope));
//	    MarkThreads("QuadRope.hscan",   1, () => QuadRopeModule.hscan(times, getZeros, rope));
//	    MarkThreads("QuadRope.vscan",   1, () => QuadRopeModule.vscan(times, getZeros, rope));
	}

        public static void RunParallelRope(int size, int threads)
        {
	    var rope = QuadRopeModule.init(size, size, timesInt);
            var hzeros = QuadRopeModule.init(size, 1, getZeros2);
            var vzeros = QuadRopeModule.init(1, size, getZeros2);
            MarkThreads("QuadRope.init",   threads, () => Parallel.QuadRopeModule.init(size, size, timesInt));
            MarkThreads("QuadRope.map",    threads, () => Parallel.QuadRopeModule.map(square, rope));
            MarkThreads("QuadRope.reduce", threads, () => Parallel.QuadRopeModule.reduce<double>(plus, 0.0, rope));
            MarkThreads("QuadRope.zip",    threads, () => Parallel.QuadRopeModule.zip(plus, rope, rope));
            MarkThreads("QuadRope.hreduce",threads, () => Parallel.QuadRopeModule.hreduce(times, 1.0, rope));
	    MarkThreads("QuadRope.vreduce",threads, () => Parallel.QuadRopeModule.vreduce(times, 1.0, rope));
//	    MarkThreads("QuadRope.hscan",  threads,
//                      () => Parallel.QuadRopeModule.hscan(times, getZeros, rope));
//	    MarkThreads("QuadRope.vscan", threads
//                      () => Parallel.QuadRopeModule.vscan(times, getZeros, rope));
	}

	public static void RunSequentialArray(int size)
        {
            var arr = Array2DModule.Initialize(size, size, timesInt);
	    MarkThreads("Array2D.init",    1, () => Array2DModule.Initialize(size, size, timesInt));
            MarkThreads("Array2D.map",     1, () => Array2DModule.Map(square, arr));
            MarkThreads("Array2D.reduce",  1, () => Array2D.reduce<double>(plus, arr));
            MarkThreads("Array2D.zip",     1, () => Array2D.map2(plus, arr, arr));
            MarkThreads("Array2D.hreduce", 1, () => Array2D.reduce2(times, arr));
	    MarkThreads("Array2D.vreduce", 1, () => Array2D.reduce1(times, arr));
//	    MarkThreads("Array2D.hscan",   1, () => Array2D.scan2(times, getZeros, arr));
//	    MarkThreads("Array2D.vscan",   1, () => Array2D.scan1(times, getZeros, arr));
        }

        public static void RunParallelArray(int size, int threads)
        {
            var arr = Array2DModule.Initialize(size, size, timesInt);
	    MarkThreads("Array2D.init",    threads, () => Parallel.Array2D.init(size, size, timesInt));
            MarkThreads("Array2D.map",     threads, () => Parallel.Array2D.map(square, arr));
            MarkThreads("Array2D.reduce",  threads, () => Parallel.Array2D.reduce<double>(plus, arr));
            MarkThreads("Array2D.zip",     threads, () => Parallel.Array2D.map2(plus, arr, arr));
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

            var arr = Array2DModule.Initialize(opts.Size, opts.Size, timesInt);
            var rope = QuadRopeModule.init(opts.Size, opts.Size, timesInt);

            Mark("Array2D.get", () => Array2DModule.Get(arr, i, j));
            Mark("QuadRope.get", () => QuadRopeModule.get(rope, i, j));

            Mark("Immutable.set", () => Array2D.set(arr, i, j, 0.0));
            Mark("Mutable.set", () => Array2DModule.Set<double>(arr, i, j, 0.0));
            Mark("QuadRope.set", () => QuadRopeModule.set(rope, i, j, 0.0));
        }

        public static void Zip(Options opts)
        {
            var rope = QuadRopeModule.init(opts.Size, opts.Size, timesInt);
            var hrev = QuadRopeModule.hrev(rope);
            var vrev = QuadRopeModule.vrev(rope);
            Mark("QuadRope.zip.opt",  () => QuadRopeModule.zip(times, rope, rope));
            Mark("QuadRope.zip.hrev", () => QuadRopeModule.zip(times, rope, hrev));
            Mark("QuadRope.zip.vrev", () => QuadRopeModule.zip(times, rope, vrev));
        }

        public static void Concatenating(Options opts)
        {
            Random rnd = new Random();

            // Initialize ropes for hcat, hight must be equal.
            Types.QuadRope<double>[] ropes = new Types.QuadRope<double>[100];
            for (int i = 0; i < ropes.Length; ++i)
                ropes[i] = QuadRopeModule.init(opts.Size, rnd.Next(1, opts.Size), timesInt);

            var rope = QuadRopeModule.init(opts.Size, 1, timesInt);
            Mark("QuadRope.hcatX100", () =>
                    {
                     for (int i = 0; i < 100; ++i)
                         rope = QuadRopeModule.hcat(rope, ropes[i]);
                    });
            Console.WriteLine("# Resulting rope depth is " + QuadRopeModule.depth(rope));
            Console.WriteLine("# Resulting rope is "
                              + (QuadRopeModule.isBalancedH(rope) ? "balanced." : "unbalanced."));

            // Initialize ropes for vcat, width must be equal.
            for (int i = 0; i < ropes.Length; ++i)
                ropes[i] = QuadRopeModule.init(rnd.Next(1, opts.Size), opts.Size, timesInt);

            rope = QuadRopeModule.init(1, opts.Size, timesInt);
            Mark("QuadRope.vcatX100", () =>
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

            SetThreads(opts.Threads);
            if (opts.Threads == 1) {
                MarkWork("Array2D.vdc",  opts.Size, () => Examples.Array2D.vanDerCorput(opts.Size));
                MarkWork("QuadRope.vdc", opts.Size, () => Examples.QuadRope.vanDerCorput(opts.Size));
            } else {
                MarkWork("Array2D.vdc",  opts.Size, () => Examples.Array2D.Parallel.vanDerCorput(opts.Size));
                MarkWork("QuadRope.vdc", opts.Size, () => Examples.QuadRope.Parallel.vanDerCorput(opts.Size));
            }
        }

        public static void Factorization(Options opts)
        {
            int start = 100000;
            var init = Utils.Functions.toFunc2<int, int, int>((i, j) => i * j + start);
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, init);
            var rope = QuadRopeModule.init(opts.Size, opts.Size, init);
            MarkThreads("Array2D.primes",      1, () => Examples.Array2D.factorize(arr));
            MarkThreads("QuadRope.primes",     1, () => Examples.QuadRope.factorize(rope));
            for (int t = 2; t <= opts.Threads; ++t) {
                MarkThreads("Array2D.primes",  t, () => Examples.Array2D.Parallel.factorize(arr));
                MarkThreads("QuadRope.primes", t, () => Examples.QuadRope.Parallel.factorize(rope));
            }
        }

        public static void Fibonacci(Options opts)
        {
            MarkThreads("Array2D.fibseq",  opts.Threads, () => Examples.Array2D.fibseq(opts.Size));
            MarkThreads("QuadRope.fibseq", opts.Threads, () => Examples.QuadRope.fibseq(opts.Size));
        }

        public static void MatrixMultiplication(Options opts)
        {
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, timesInt);
            var rope = QuadRopeModule.init(opts.Size, opts.Size, timesInt);
            MarkThreads("Array2D.mmult",      1, () => Examples.Array2D.mmult(arr, arr));
            MarkThreads("Array2D.mmultImp",   1, () => Examples.Array2D.mmultImp(arr, arr));
            MarkThreads("QuadRope.mmult",     1, () => Examples.QuadRope.mmult(rope, rope));
            for (int t = 2; t <= opts.Threads; ++t) {
                MarkThreads("Array2D.mmult",    t, () => Examples.Array2D.Parallel.mmult(arr, arr));
                MarkThreads("Array2D.mmultImp", t, () => Examples.Array2D.Parallel.mmultImp(arr, arr));
                MarkThreads("QuadRope.mmult",   t, () => Examples.QuadRope.Parallel.mmult(rope, rope));
            }
        }

        public static void Reallocation(Options opts)
        {
            var rope = QuadRopeModule.init(opts.Size, opts.Size, timesInt);
            Mark("seq-reallocate", () => QuadRopeModule.reallocate(rope));
            Mark("par-reallocate", () => Parallel.QuadRopeModule.reallocate(rope));
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
