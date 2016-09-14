# Quad Ropes #

A quad rope is an immutable, four-ary tree with two-dimensional arrays at its leaves. They combine the ideas of a [rope](https://en.wikipedia.org/wiki/Rope_(data_structure)) and [quad trees](https://en.wikipedia.org/wiki/Quadtree).

Two-dimensional array-like data structures are often used in declarative array programming. The overall idea is to allow for constant-time concatenation while retaining efficiency when compared to native 2D-arrays. Fast concatenation is important for immutable arrays as it allows a more complex gradual construction than a simple ```unfold```.

Another positive property of immutable trees is that they are inherently parallel. At each node, we can spawn two or four tasks, which are handled by the .Net Thread-Parallel Library (TPL).

## Runtime Overview ##


| Operation | Immutable 2D Array | Quad Rope  |
|-----------|--------------------|------------|
| Index     | O(1)               | O(log n)   |
| Set       | O(n)               | O(log n)   |
| Map       | O(n)               | O(n log n) |
| Reduce    | O(n)               | O(n log n) |
| Concat    | O(n)               | O(1)       |

In benchmarks, quad ropes often have only little overhead compared to native 2D arrays. In more interesting algorithms, quad ropes are often faster. In particular, quad ropes can handle nested parallelism much better than nested for-loops over arrays without any tweaking of the TPL thread pool.

## Parallel Operations ##

The majority of functions on quad ropes are available in a parallelized version. Currently, quad ropes use eager tree splitting. Recent research suggests that a more dynamic scheduling approach would be more appropriate.

## Contributions ##

Please contribute by opening an issue, using quad ropes in your projects or making a pull request.

## Things Left to Do ##

- [ ] Implement a general "wavefront" ```scan``` in two dimensions.
- [ ] Parallelize ```scan```.
- [ ] Re-implement all functions that do not yet allocate a single array.
- [ ] Improve code quality and documentation.

## Maintainers ##

This repository is maintained by [Florian Biermann](https://github.com/fbie).
