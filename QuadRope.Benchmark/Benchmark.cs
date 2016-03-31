using System;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;

namespace RadTrees.Benchmark
{

    delegate int intop(int i, int j);

    class Benchmark
    {
        delegate int intint(int x);
        
        const int size = 100;

        public static void Run()
        {
            var times = FSharpFunc<int, FSharpFunc<int, int>>.FromConverter(i => FSharpFunc<int, int>.FromConverter(j => i * j));
            var timesTwo = FSharpFunc<int, int>.FromConverter(i => 2 * i);

            var x = QuadRope.init(size, size, times);
            var arr = Array2DModule.Initialize(size, size, times);

            Infrastructure.Mark8("QuadRope.map", i => { QuadRope.map(timesTwo, x); return i; });
            Infrastructure.Mark8("Array2D.map", i => { Array2DModule.Map(timesTwo, arr); return i; });

            Infrastructure.Mark8("QuadRope.foldH", i => { QuadRope.foldH(times, 0, x); return i; });
            Infrastructure.Mark8("Array2D.foldH", i => { Array2D.fold1(times, 0, arr); return i; });
            Infrastructure.Mark8("QuadRope.foldV", i => { QuadRope.foldV(times, 0, x); return i; });
            Infrastructure.Mark8("Array2D.foldV", i => { Array2D.fold2(times, 0, arr); return i; });
        }

	public static void Main(string[] args)
        {
	    Run();
	}
    }

    // Benchmark code by https://www.itu.dk/~sestoft/papers/benchmarking.pdf
    class Infrastructure {
	private static void SystemInfo() {
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
