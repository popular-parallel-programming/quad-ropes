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
using CommandLine;

namespace RadTrees.Benchmark
{

    /// <summary>
    ///   Base options shared by every execution mode.
    /// </summary>
    public class Options
    {
	private string mode;

	[Option('m', "mode", DefaultValue = "all",
		HelpText = "Mode to run.")]
	public string Mode {
	    get { return mode;}
	    set { mode = value.ToLower(); }
	}

	[Option('s', "size", DefaultValue = 1000,
		HelpText = "Size of the data structure in each dimension.")]
	public int Size { get; set; }

	private int threads;

	[Option('t', "threads", DefaultValue = 1,
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
