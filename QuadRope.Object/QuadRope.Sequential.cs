// -*- c-basic-offset: 4; indent-tabs-mode: nil -*-

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
            return new SequentialQuadRope<T>(QuadRopeModule.slice(rope, row, col, rows, cols));
        }

        public override IQuadRope<T> Transpose()
        {
            return new SequentialQuadRope<T>(QuadRopeModule.transpose(rope));
        }
    }
}
