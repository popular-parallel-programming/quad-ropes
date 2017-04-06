# Quad Ropes #

A quad rope is a two-dimensional binary tree with small, contiguous arrays at its leafs. Quad ropes combine the ideas of a [rope](https://en.wikipedia.org/wiki/Rope_(data_structure)) and [quad trees](https://en.wikipedia.org/wiki/Quadtree).

Two-dimensional array-like data structures are often used in declarative array programming. The overall idea is to allow for constant-time concatenation while roughly retaining efficiency of standard immutable 2D-arrays. Fast concatenation is important for immutable arrays as it allows a more complex gradual construction than a simple ```unfold```.

Another positive property of immutable trees is that they are inherently parallel. At each node, we can spawn new parallel tasks that are handled by the .Net Task-Parallel Library (TPL).


## Runtime Complexity Overview ##

| Operation | Immutable 2D Array | Quad Rope  |
|-----------|--------------------|------------|
| Index     | O(1)               | O(log n)   |
| Set       | O(n)               | O(log n)   |
| Map       | O(n)               | O(n + m)   |
| Reduce    | O(n)               | O(n + m)   |
| Concat    | O(n)               | O(log n)   |

In benchmarks, quad ropes often have only little overhead compared to standard immutable 2D arrays. In more interesting algorithms, quad ropes are often faster. In particular, quad ropes can handle nested parallelism much better than nested for-loops over arrays without any additional tweaking of the TPL thread pool.


## Parallel Operations ##

The majority of functions on quad ropes are available in a parallelized version. Currently, quad ropes use eager tree splitting. Recent research suggests that a more dynamic scheduling approach would be more appropriate.


## Benchmarking ##

**NB:** To reproduce the results of the ARRAY'17 paper, [checkout this commit](https://github.com/popular-parallel-programming/quad-ropes/commit/a89ac162271f9277fa8bc87596b3417238d256ee).

You can test the performance of this quad rope implementation on your own machine. First, compile in release mode.

On Windows:

```
> build --paket
> build --release
```

On Linux and Mac with Mono:

```
$ make paket
$ make release
```

Now, you can run the benchmarks. The results will be written into the [logs](\logs) folder.


On Windows:

```
> scripts\benchmark-all.bat
```

You wan run individual benchmarks without writing to a file:

```
> scripts\benchmark.bat --help

QuadRope 1.0.0.0
Copyright ©  2017

  -m, --mode       Required. Which benchmark to run.

  -s, --size       (Default: 100) Input size.

  -t, --threads    (Default: 1) Number of threads.

  --help           Display this help screen.

  --version        Display version information.

Available benchmarks:
align
all
fibs
index
mmult
primes
sieve
vdc
```

On Linux and Mac, run

```
$ scripts\benchmark --help
```

to get the same output.


## Unit Tests ##

We use [FsCheck](https://github.com/fscheck/FsCheck) for testing.

On Windows:

```
> debug.bat
> test.bat
```

On Linux and Mac:

```
$ make debug
$ make test
```

## Contributions ##

Please contribute by opening an issue, using quad ropes in your projects or making a pull request.


## Maintainers ##

This repository is maintained by [Florian Biermann](https://github.com/fbie).
