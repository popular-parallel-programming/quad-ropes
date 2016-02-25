# Rad Trees #

Trees are rad, especially radix trees are! See the [definition of "rad" at Urban Dictionary.](http://www.urbandictionary.com/define.php?term=Rad)

This repository holds various implementations of radix trees in multiple rad dimensions.

## Purpose ##

We want to use relaxed radix bound trees to back the implementation of two-dimensional arrays in Funcalc. Because these data structures are of general interest, they reside outside of the Funcalc repository.

Relaxed radix bound trees improve on the performance of concatenation because they do not require the entire tree to be balanced. That is, we follow the approach outlined in section 2.5 of the technical report by Bagwell & Rompf.

## References ##

- [Bagwell & Rompf, 2011: **RRB Trees: Efficient Immutable Vectors**](http://infoscience.epfl.ch/record/169879/files/RMTrees.pdf)
- [Stucki et al., 2015: **RRB Vector: A Practical General Purpose Immutable Sequence**](http://dl.acm.org/citation.cfm?id=2784739)
