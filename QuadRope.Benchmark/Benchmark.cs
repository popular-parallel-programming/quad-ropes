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
	    Functions.toFunc2<int, int, int>((i, j) => i * j);
	public static FSharpFunc<int, FSharpFunc<int, int>> plus =
	    Functions.toFunc2<int, int, int>((x, y) => x + y);

	// Functions of type int -> int.
	public static FSharpFunc<int, int> timesTwo =
	    Functions.toFunc1<int, int>(i => 2 * i);
	public static FSharpFunc<int, int> getZeros =
	    Functions.toFunc1<int, int>(i => 0);

	public static void RunBaseline(Options opts)
	{
	    Mark("QuadRope.init", () => QuadRopeModule.init(opts.Size, opts.Size, times));
	    var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
	    Mark("QuadRope.map", () => QuadRopeModule.map(timesTwo, rope));
	    Mark("QuadRope.hreduce", () => QuadRopeModule.hreduce(plus, rope));
	    Mark("QuadRope.vreduce", () => QuadRopeModule.vreduce(plus, rope));
	}

	public static void RunParallel(Options opts)
	{
	    if (opts.Threads == 1) {
		RunBaseline(opts);
		return;
	    }
	    SetThreads(opts.Threads);
	    Mark("QuadRope.Parallel.init", () => QuadRope.Parallel.init(opts.Size, opts.Size, times));
	    var rope = QuadRopeModule.init(opts.Size, opts.Size, times);
	    Mark("QuadRope.Parallel.map", () => QuadRope.Parallel.map(timesTwo, rope));
	    Mark("QuadRope.Parallel.hreduce", () => QuadRope.Parallel.hreduce(plus, rope));
	    Mark("QuadRope.Parallel.vreduce", () => QuadRope.Parallel.vreduce(plus, rope));
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
	    Mark("QuadRope.hcat", () => QuadRopeModule.hcat(rope, rope));
	    Mark("QuadRope.hcat + hbalance",
		 () => QuadRopeModule.hbalance(QuadRopeModule.hcat(rope, rope)));
	    Mark("QuadRope.hcat + reallocate",
		 () => QuadRopeModule.reallocate(QuadRopeModule.hcat(rope, rope)));

	    Mark("QuadRope.vcat", () => QuadRopeModule.vcat(rope, rope));
	    Mark("QuadRope.vcat + vbalance",
		 () => QuadRopeModule.vbalance(QuadRopeModule.vcat(rope, rope)));
	    Mark("QuadRope.vcat + reallocate",
		 () => QuadRopeModule.reallocate(QuadRopeModule.vcat(rope, rope)));

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
	    Mark("Array2D.hfold", () => Array2D.fold2(times, getZeros, arr));
	    Mark("Array2D.vfold", () => Array2D.fold1(times, getZeros, arr));
	    Mark("Array2D.hscan", () => Array2D.scan2(times, getZeros, arr));
	    Mark("Array2D.vscan", () => Array2D.scan1(times, getZeros, arr));
	    Mark("Array2D.hreduce", () => Array2D.reduce2(plus, arr));
	    Mark("Array2D.vreduce", () => Array2D.reduce1(plus, arr));
	    Mark("Array2D.hcat", () => Array2D.cat2(arr, arr));
	    Mark("Array2D.vcat", () => Array2D.cat1(arr, arr));

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
		    default:
			Console.WriteLine("Mode not supported: \"{0}\"", opts.Mode);
			break;
		}
	    }
	}
    }

    // Benchmark code by https://www.itu.dk/~sestoft/papers/benchmarking.pdf
    class Infrastructure {

	public static void SystemInfo() {
	    Console.WriteLine("# OS          {0}",
			      Environment.OSVersion.VersionString);
	    Console.WriteLine("# .NET vers.  {0}",
			      Environment.Version);
	    Console.WriteLine("# 64-bit OS   {0}",
			      Environment.Is64BitOperatingSystem);
	    Console.WriteLine("# 64-bit proc {0}",
			      Environment.Is64BitProcess);
	    Console.WriteLine("# CPU         {0}; {1} \"cores\"",
			      Environment.GetEnvironmentVariable("PROCESSOR_IDENTIFIER"),
			      Environment.ProcessorCount);
	    Console.WriteLine("# Date        {0:s}",
			      DateTime.Now);
	}

	public static double Mark8(String msg, String info, Func<int,double> f,
				   int n = 10, double minTime = 0.25) {
	    int count = 1, totalCount = 0;
	    double dummy = 0.0, runningTime = 0.0, st = 0.0, sst = 0.0;
	    do {
		count *= 2;
		st = sst = 0.0;
		for (int j=0; j<n; j++) {
		    Timer t = new Timer();
		    for (int i=0; i<count; i++)
			dummy += f(i);
		    runningTime = t.Check();
		    double time = runningTime * 1e9 / count;
		    st += time;
		    sst += time * time;
		    totalCount += count;
		}
	    } while (runningTime < minTime && count < Int32.MaxValue/2);
	    double mean = st/n, sdev = Math.Sqrt((sst - mean*mean*n)/(n-1));
	    Console.WriteLine("{0,-25} {1}{2,15:F1} ns {3,10:F2} {4,10:D}", msg, info, mean, sdev, count);
	    return dummy / totalCount;
	}

	public static double Mark8(String msg, Func<int,double> f,
				   int n = 10, double minTime = 0.25) {
	    return Mark8(msg, "", f, n, minTime);
	}

	public static double Mark8Setup(String msg, String info, Func<int,double> f,
					Action setup = null, int n = 10, double minTime = 0.25) {
	    int count = 1, totalCount = 0;
	    double dummy = 0.0, runningTime = 0.0, st = 0.0, sst = 0.0;
	    do {
		count *= 2;
		st = sst = 0.0;
		for (int j=0; j<n; j++) {
		    Timer t = new Timer();
		    for (int i=0; i<count; i++) {
			t.Pause();
			if (setup != null)
			    setup();
			t.Play();
			dummy += f(i);
		    }
		    runningTime = t.Check();
		    double time = runningTime * 1e9 / count;
		    st += time;
		    sst += time * time;
		    totalCount += count;
		}
	    } while (runningTime < minTime && count < Int32.MaxValue/2);
	    double mean = st/n, sdev = Math.Sqrt((sst - mean*mean*n)/(n-1));
	    Console.WriteLine("{0,-25} {1}{2,15:F1} ns {3,10:F2} {4,10:D}", msg, info, mean, sdev, count);
	    return dummy / totalCount;
	}

	public static double Mark8Setup(String msg, Func<int,double> f,
					Action setup = null, int n = 10, double minTime = 0.25) {
	    return Mark8Setup(msg, "", f, setup, n, minTime);
	}
    }

    public class Timer {
        private readonly System.Diagnostics.Stopwatch stopwatch
                = new System.Diagnostics.Stopwatch();
        public Timer() { Play(); }
        public double Check() { return stopwatch.ElapsedMilliseconds / 1000.0; }
        public void Pause() { stopwatch.Stop(); }
        public void Play() { stopwatch.Start(); }
    }
}
