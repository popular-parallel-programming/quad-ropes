// -*- c-basic-offset: 4; indent-tabs-mode: nil-*-
using System;
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

	public static void RunBaseline(Options opts)
	{
	    Mark("QuadRope.init", () => QuadRopeModule.init(opts.Size, opts.Size, times));
	    var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
	    Mark("QuadRope.map", () => QuadRopeModule.map(timesTwo, rope));
	    Mark("QuadRope.reduce", () => QuadRopeModule.reduce(plus, rope));
	    Mark("QuadRope.zip", () => QuadRopeModule.zip(times, rope,
							  QuadRopeModule.hrev(QuadRopeModule.vrev(rope))));
	}

	public static void RunParallel(Options opts)
	{
	    if (opts.Threads == 1) {
		RunBaseline(opts);
		return;
	    }
	    SetThreads(opts.Threads);
            Mark("QuadRope.init", () => Parallel.QuadRopeModule.init(opts.Size, opts.Size, times));
	    var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
	    Mark("QuadRope.map", () => Parallel.QuadRopeModule.map(timesTwo, rope));
	    Mark("QuadRope.reduce", () => Parallel.QuadRopeModule.reduce(plus, rope));
	    Mark("QuadRope.zip", () =>
                 Parallel.QuadRopeModule.zip(times, rope,
                                             QuadRopeModule.hrev(QuadRopeModule.vrev(rope))));
	}

        public static void RunSequential(Options opts)
        {
	    RunBaseline(opts);
	    var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
	    Mark("QuadRope.hfold",
		 () => QuadRopeModule.hfold(times, QuadRopeModule.initZeros(opts.Size, 1), rope));
	    Mark("QuadRope.vfold",
		 () => QuadRopeModule.vfold(times, QuadRopeModule.initZeros(1, opts.Size), rope));
	    Mark("QuadRope.hscan", () => QuadRopeModule.hscan(times, getZeros, rope));
	    Mark("QuadRope.vscan", () => QuadRopeModule.vscan(times, getZeros, rope));
	    Mark("QuadRope.hcat", () => DoNTimes(100, (l, r) => QuadRopeModule.hcat(l, r), rope));
	    Mark("QuadRope.hcat + hbalance",
		 () => QuadRopeModule.hbalance(DoNTimes(100, (l, r) => QuadRopeModule.hcat(l, r), rope)));
//	    Mark("QuadRope.hcat + reallocate",
//		 () => QuadRopeModule.reallocate(QuadRopeModule.hcat(rope, rope)));

	    Mark("QuadRope.vcat", () => DoNTimes(100, (u, l) => QuadRopeModule.vcat(u, l), rope));
	    Mark("QuadRope.vcat + vbalance",
		 () => QuadRopeModule.vbalance(DoNTimes(100, (u, l) => QuadRopeModule.vcat(u, l), rope)));
//	    Mark("QuadRope.vcat + reallocate",
//		 () => QuadRopeModule.reallocate(QuadRopeModule.vcat(rope, rope)));

	    // Indexing.
	    {
	        Random rnd = new Random(892);
		int i = rnd.Next(opts.Size);
		int j = rnd.Next(opts.Size);
		Mark("QuadRope.index", () => QuadRopeModule.get(rope, i, j));
	    }

	    // Persistent update of a pseudo-random index pair.
	    {
		Random rnd = new Random(368);
		int i = rnd.Next(opts.Size);
		int j = rnd.Next(opts.Size);
		Mark("QuadRope.set", () => QuadRopeModule.set(rope, i, j, 0));
	    }
	}

	public static void RunArray(Options opts) {
            var arr = Array2DModule.Initialize(opts.Size, opts.Size, times);
	    Mark("Array2D.init", () => Array2DModule.Initialize(opts.Size, opts.Size, times));
            Mark("Array2D.map", () => Array2DModule.Map(timesTwo, arr));
            Mark("Array2D.reduce", () => Array2D.reduce<int>(plus, arr));
            Mark("Array2D.zip", () => Array2D.map2(plus, arr, arr));
            Mark("Array2D.hfold", () => Array2D.fold2(times, getZeros, arr));
	    Mark("Array2D.vfold", () => Array2D.fold1(times, getZeros, arr));
	    Mark("Array2D.hscan", () => Array2D.scan2(times, getZeros, arr));
	    Mark("Array2D.vscan", () => Array2D.scan1(times, getZeros, arr));
//	    Mark("Array2D.hcat", () => DoNTimes(100, (l, r) => Array2D.cat2(l, r), arr));
//	    Mark("Array2D.vcat", () => DoNTimes(100, (l, r) => Array2D.cat1(l, r), arr));

	    // Indexing.
	    {
	        Random rnd = new Random(892);
		int i = rnd.Next(opts.Size);
		int j = rnd.Next(opts.Size);
		Mark("Array2D.index", () => Array2DModule.Get(arr, i, j));
	    }

	    // Persistent update of a pseudo-random index pair.
	    {
		Random rnd = new Random(368);
		int i = rnd.Next(opts.Size);
		int j = rnd.Next(opts.Size);
		Mark("Array2D.set", () => Array2D.set(arr, i, j, 0));
	    }
        }

        public static void vanDerCorput(Options opts) {
            Mark("vdc Array2D", () => Examples.Array2D.vanDerCorput(opts.Size));
            Mark("vdc QuadRope", () => Examples.QuadRope.vanDerCorput(opts.Size));
        }

	public static void Main(string[] args)
        {
	    Options opts = new Options();
	    if (Parser.Default.ParseArguments(args, opts)) {
		Infrastructure.SystemInfo();
		switch (opts.Mode) {
		    case "array":
			RunArray(opts);
			break;
		    case "sequential":
			RunSequential(opts);
			break;
		    case "parallel":
			RunParallel(opts);
			break;
                    case "vdc":
                        vanDerCorput(opts);
                        break;
		    default:
			Console.WriteLine("Mode not supported: \"{0}\"", opts.Mode);
			break;
		}
	    }
	}
    }


}
