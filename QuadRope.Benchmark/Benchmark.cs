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
        public static void Mark(string msg, Action f)
        {
	    try {
		Infrastructure.Mark8(msg, i => { f(); return i; });
	    } catch (Exception e) {
                Console.WriteLine(e.ToString());
	    }
        }

	public static void SetThreads(int nthreads)
	{
	    // Set min threads first, otherwise setting max threads might fail.
	    System.Threading.ThreadPool.SetMinThreads(1, 0);
	    if (!System.Threading.ThreadPool.SetMaxThreads(nthreads, 0))
		Console.WriteLine("# Error: could not change the number of thread pool threads.");
	    Console.WriteLine("# Threads     {0}", nthreads);
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

        public static T DoNTimes<T>(int n, Func<T, T, T> f, T t)
        {
            T acc = t;
            for (int i = 0; i < n; ++i)
                acc = f(acc, t);
            return acc;
        }

        public static void RunSequentialRope(Options opts)
        {
	    var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
            var hzeros = QuadRopeModule.initZeros(opts.Size, 1);
            var vzeros = QuadRopeModule.initZeros(1, opts.Size);
            Mark("QuadRope.init",    () => QuadRopeModule.init(opts.Size, opts.Size, times));
            Mark("QuadRope.map",     () => QuadRopeModule.map(timesTwo, rope));
            Mark("QuadRope.reduce",  () => QuadRopeModule.reduce<int>(plus, rope));
            Mark("QuadRope.zip",     () => QuadRopeModule.zip(plus, rope, rope));
            Mark("QuadRope.hfold",   () => QuadRopeModule.hfold(times, hzeros, rope));
	    Mark("QuadRope.vfold",   () => QuadRopeModule.vfold(times, vzeros, rope));
            Mark("QuadRope.hreduce", () => QuadRopeModule.hreduce(times, rope));
	    Mark("QuadRope.vreduce", () => QuadRopeModule.vreduce(times, rope));
//	    Mark("QuadRope.hscan",   () => QuadRopeModule.hscan(times, getZeros, rope));
//	    Mark("QuadRope.vscan",   () => QuadRopeModule.vscan(times, getZeros, rope));
	}

        public static void RunParallelRope(Options opts)
        {
	    var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
            var hzeros = QuadRopeModule.initZeros(opts.Size, 1);
            var vzeros = QuadRopeModule.initZeros(1, opts.Size);
            Mark("QuadRope.init",    () => Parallel.QuadRopeModule.init(opts.Size, opts.Size, times));
            Mark("QuadRope.map",     () => Parallel.QuadRopeModule.map(timesTwo, rope));
            Mark("QuadRope.reduce",  () => Parallel.QuadRopeModule.reduce<int>(plus, rope));
            Mark("QuadRope.zip",     () => Parallel.QuadRopeModule.zip(plus, rope, rope));
            Mark("QuadRope.hfold",   () => Parallel.QuadRopeModule.hfold(times, hzeros, rope));
	    Mark("QuadRope.vfold",   () => Parallel.QuadRopeModule.vfold(times, vzeros, rope));
            Mark("QuadRope.hreduce", () => Parallel.QuadRopeModule.hreduce(times, rope));
	    Mark("QuadRope.vreduce", () => Parallel.QuadRopeModule.vreduce(times, rope));
//	    Mark("QuadRope.hscan",   () => Parallel.QuadRopeModule.hscan(times, getZeros, rope));
//	    Mark("QuadRope.vscan",   () => Parallel.QuadRopeModule.vscan(times, getZeros, rope));
	}

	public static void RunSequentialArray(Options opts)
        {
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, times);
	    Mark("Array2D.init",    () => Array2DModule.Initialize(opts.Size, opts.Size, times));
            Mark("Array2D.map",     () => Array2DModule.Map(timesTwo, arr));
            Mark("Array2D.reduce",  () => Array2D.reduce<int>(plus, arr));
            Mark("Array2D.zip",     () => Array2D.map2(plus, arr, arr));
            Mark("Array2D.hfold",   () => Array2D.fold2(times, getZeros, arr));
	    Mark("Array2D.vfold",   () => Array2D.fold1(times, getZeros, arr));
            Mark("Array2D.hreduce", () => Array2D.reduce2(times, arr));
	    Mark("Array2D.vreduce", () => Array2D.reduce1(times, arr));
//	    Mark("Array2D.hscan",   () => Array2D.scan2(times, getZeros, arr));
//	    Mark("Array2D.vscan",   () => Array2D.scan1(times, getZeros, arr));
        }

        public static void RunParallelArray(Options opts)
        {
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, times);
	    Mark("Array2D.init",    () => Parallel.Array2D.init(opts.Size, opts.Size, times));
            Mark("Array2D.map",     () => Parallel.Array2D.map(timesTwo, arr));
            Mark("Array2D.reduce",  () => Parallel.Array2D.reduce<int>(plus, arr));
            Mark("Array2D.zip",     () => Parallel.Array2D.map2(plus, arr, arr));
            Mark("Array2D.hfold",   () => Parallel.Array2D.fold2(times, getZeros, arr));
	    Mark("Array2D.vfold",   () => Parallel.Array2D.fold1(times, getZeros, arr));
            Mark("Array2D.hreduce", () => Parallel.Array2D.reduce2(times, arr));
	    Mark("Array2D.vreduce", () => Parallel.Array2D.reduce1(times, arr));
//	    Mark("Array2D.hscan",   () => Parallel.Array2D.scan2(times, getZeros, arr));
//	    Mark("Array2D.vscan",   () => Parallel.Array2D.scan1(times, getZeros, arr));
        }

        public static void Run(Options opts)
        {
            if (opts.Threads == 1) {
                RunSequentialRope(opts);
                RunSequentialArray(opts);
            } else {
                SetThreads(opts.Threads);
                RunParallelRope(opts);
                RunParallelArray(opts);
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
            Console.WriteLine("# This benchmark is useless if balancing happens during concatenation.");
            var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
            var vskewed = DoNTimes(100, (l, r) => QuadRopeModule.vcat(l, r), rope);
            var hskewed = DoNTimes(100, (l, r) => QuadRopeModule.hcat(l, r), rope);
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, times);

            Mark("QuadRope.hcat", () => QuadRopeModule.hcat(rope, rope));
	    Mark("QuadRope.hbalance", () => QuadRopeModule.hbalance(hskewed));
            Mark("QuadRope.hbalance (Boehm et al.)", () => QuadRopeModule.Boehm.hbalance(hskewed));

            Mark("QuadRope.vcat", () => QuadRopeModule.vcat(rope, rope));
	    Mark("QuadRope.vbalance", () => QuadRopeModule.vbalance(vskewed));
            Mark("QuadRope.vbalance (Boehm et al.)", () => QuadRopeModule.Boehm.vbalance(vskewed));

            Mark("Array2D.hcat", () => Array2D.cat2(arr, arr));
            Mark("Array2D.vcat", () => Array2D.cat1(arr, arr));
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

            Mark("primes Array2D.Parallel", () => Examples.Array2D.Parallel.factorize(arr));
            Mark("primes QuadRope.Parallel", () => Examples.QuadRope.Parallel.factorize(rope));
        }

        public static void Fibonacci(Options opts)
        {
            Mark("fibseq Array2D", () => Examples.Array2D.fibseq(opts.Size));
            Mark("fibseq QuadRope", () => Examples.QuadRope.fibseq(opts.Size));
        }

        public static void MatrixMultiplication(Options opts)
        {
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, times);
            var rope = QuadRopeModule.init(opts.Size, opts.Size, times);

            Mark("mmult Array2D",           () => Examples.Array2D.matmult(arr, arr));
//            Mark("mmult Array2D.Parallel",  () => Examples.Array2D.Parallel.matmult(arr, arr));
            Mark("mmult QuadRope",          () => Examples.QuadRope.matmult(rope, rope));
            Mark("mmult QuadRope.Parallel", () => Examples.QuadRope.Parallel.matmult(rope, rope));
        }

        public static void Reallocation(Options opts)
        {
            var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
            Mark("QuadRope.reallocate", () => QuadRopeModule.reallocate(rope));
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
