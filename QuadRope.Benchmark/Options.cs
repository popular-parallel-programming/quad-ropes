using System;
using CommandLine;

namespace RadTrees.Benchmark
{

    /// <summary>
    ///   Base options shared by every execution mode.
    /// </summary>
    public class Options
    {
	private string mode;

	[Option('m', "mode", DefaultValue = "sequential",
		HelpText = "Mode to run. One of \"sequential\", \"parallel\" or \"array\".")]
	public string Mode {
	    get { return mode;}
	    set { mode = value.ToLower(); }
	}

	[Option('s', "size", DefaultValue = 1000,
		HelpText = "Size of the data structure in each dimension.")]
	public int Size { get; set; }

	private int threads;

	[Option('t', "threads", DefaultValue = 0,
		HelpText = "Number of threads to use.")]
	public int Threads
	{
	    get {
		if (threads == 0)
		    return Environment.ProcessorCount;
		return threads;
	    }
	    set { threads = value; }
	}
    }
}
