// -*- c-basic-offset: 4; indent-tabs-mode: nil -*-

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

using static RadTrees.Types;
using static RadTrees.Utils;

namespace RadTrees.QuadRope.Object
{
    public class SequentialQuadRope<T> : IQuadRope<T>
    {
        private SequentialQuadRope(QuadRope<T> rope)
                : base(rope)
        { }

        public override IQuadRope<T> BalanceHorizontally()
        {
            return new SequentialQuadRope<T>(QuadRopeModule.hbalance(rope));
        }

        public override IQuadRope<T> BalanceVertically()
        {
            return new SequentialQuadRope<T>(QuadRopeModule.vbalance(rope));
        }

        public override IQuadRope<T> ConcatHorizontally(IQuadRope<T> other)
        {
            return new SequentialQuadRope<T>(QuadRopeModule.hcat(rope, other.rope));
        }

        public override IQuadRope<T> ConcatVertically(IQuadRope<T> other)
        {
            return new SequentialQuadRope<T>(QuadRopeModule.vcat(rope, other.rope));
        }

        public override IQuadRope<S> FoldHorizontally<S>(Func<S, T, S> f, IQuadRope<S> states)
        {
            return new SequentialQuadRope<S>(QuadRopeModule.hfold(Functions.toFunc2(f), states.rope, rope));
        }

        public override IQuadRope<S> FoldVertically<S>(Func<S, T, S> f, IQuadRope<S> states)
        {
            return new SequentialQuadRope<S>(QuadRopeModule.vfold(Functions.toFunc2(f), states.rope, rope));
        }

        public override IQuadRope<S> Map<S>(Func<T, S> f)
        {
            return new SequentialQuadRope<S>(QuadRopeModule.map(Functions.toFunc1(f), rope));
        }

        public override S MapReduce<S>(Func<T, S> f, Func<S, S, S> g)
        {
            return QuadRopeModule.mapreduce(Functions.toFunc1(f), Functions.toFunc2(g), rope);
        }

        public override IQuadRope<S> MapReduceHorizontally<S>(Func<T, S> f, Func<S, S, S> g)
        {
            return new SequentialQuadRope<S>(QuadRopeModule.hmapreduce(Functions.toFunc1(f),
                                                                       Functions.toFunc2(g),
                                                                       rope));
        }

        public override IQuadRope<S> MapReduceVertically<S>(Func<T, S> f, Func<S, S, S> g)
        {
            return new SequentialQuadRope<S>(QuadRopeModule.vmapreduce(Functions.toFunc1(f),
                                                                       Functions.toFunc2(g),
                                                                       rope));
        }

        public override T Reduce(Func<T, T, T> f)
        {
            return QuadRopeModule.reduce(Functions.toFunc2(f), rope);
        }

        public override IQuadRope<T> ReduceHorizontally(Func<T, T, T> f)
        {
            return new SequentialQuadRope<T>(QuadRopeModule.hreduce(Functions.toFunc2(f), rope));
        }

        public override IQuadRope<T> ReduceVertically(Func<T, T, T> f)
        {
            return new SequentialQuadRope<T>(QuadRopeModule.vreduce(Functions.toFunc2(f), rope));
        }

        public override IQuadRope<T> ReverseHorizontally()
        {
            return new SequentialQuadRope<T>(QuadRopeModule.hrev(rope));
        }

        public override IQuadRope<T> ReverseVertically()
        {
            return new SequentialQuadRope<T>(QuadRopeModule.vrev(rope));
        }

        public override IQuadRope<T> ScanHorizontally(Func<T, T, T> f, Func<int, T> states)
        {
            return new SequentialQuadRope<T>(QuadRopeModule.hscan(Functions.toFunc2(f),
                                                                  Functions.toFunc1(states),
                                                                  rope));
        }

        public override IQuadRope<T> ScanVertically(Func<T, T, T> f, Func<int, T> states)
        {
            return new SequentialQuadRope<T>(QuadRopeModule.vscan(Functions.toFunc2(f),
                                                                  Functions.toFunc1(states),
                                                                  rope));
        }

        public override IQuadRope<T> Set(int row, int col, T v)
        {
            return new SequentialQuadRope<T>(QuadRopeModule.set(rope, row, col, v));
        }

        public override IQuadRope<T> Slice(int row, int col, int rows, int cols)
        {
            return new SequentialQuadRope<T>(QuadRopeModule.slice(row, col, rows, cols, rope));
        }

        public override IQuadRope<T> Transpose()
        {
            return new SequentialQuadRope<T>(QuadRopeModule.transpose(rope));
        }
    }
}
