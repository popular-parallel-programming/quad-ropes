# Rad Trees #

Trees are rad! See the [definition of "rad" at Urban Dictionary.](http://www.urbandictionary.com/define.php?term=Rad)

This repository holds various implementations of trees in multiple rad dimensions.

## Purpose ##

We want to explore different kinds of trees trees to back the implementation of two-dimensional arrays in Funcalc. Because these data structures are of general interest, they reside outside of the Funcalc repository.

### Criteria ###

We need a number of efficient operations on 2D arrays in Funcalc. For instance, we want fast concatenation in both, horizontal and in vertical directions. Furthermore, we want to perform operations like ```zip``` or ```map``` in parallel.

### Ropes ###

Bergstrom et al. have used ropes for dynamically scheduling parallel work. Ropes have other nice properties, like amortized O(1) concatenation and O(log n) update and indexing. There are some numeric computations, like computing van der Corput sequences, that require repeatedly concatenation of intermediate arrays. Therefore, we expand ropes to two dimensions and call them *quad ropes*.

### Other Data Structures ###

Relaxed radix bound trees improve the performance of radix tree concatenation because they do not require the entire tree to be balanced. That is, we follow the approach outlined in section 2.5 of the technical report by Bagwell & Rompf.

## References ##

- [Bergstrom et al., 2010: **Lazy Tree Splitting**](http://dl.acm.org/citation.cfm?id=1863558)
- [Boehm et al., 1995: **Ropes: An Alternative to Strings**](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.9450)
- [Bagwell & Rompf, 2011: **RRB Trees: Efficient Immutable Vectors**](http://infoscience.epfl.ch/record/169879/files/RMTrees.pdf)
- [Stucki et al., 2015: **RRB Vector: A Practical General Purpose Immutable Sequence**](http://dl.acm.org/citation.cfm?id=2784739)
