// -*- c-basic-offset: 4; indent-tabs-mode: nil -*-

using System;

using static RadTrees.Types;
using static RadTrees.Utils;

namespace RadTrees.QuadRope.Object
{
    public class ParallelQuadRope<T> : IQuadRope<T>
    {
	private ParallelQuadRope(QuadRope<T> rope)
	    : base(rope)
	{ }

        public override IQuadRope<T> BalanceHorizontally()
        {
            // NB: Not parallel!
            return new ParallelQuadRope<T>(QuadRopeModule.hbalance(rope));
        }

        public override IQuadRope<T> BalanceVertically()
        {
            // NB: Not parallel!
            return new ParallelQuadRope<T>(QuadRopeModule.vbalance(rope));
        }

        public override IQuadRope<T> ConcatHorizontally(IQuadRope<T> other)
        {
            // NB: Not parallel!
            return new ParallelQuadRope<T>(QuadRopeModule.hcat(rope, other.rope));
        }

        public override IQuadRope<T> ConcatVertically(IQuadRope<T> other)
        {
            // NB: Not parallel!
            return new ParallelQuadRope<T>(QuadRopeModule.vcat(rope, other.rope));
        }

        public override IQuadRope<S> FoldHorizontally<S>(Func<S, T, S> f, IQuadRope<S> states)
        {
            // NB: Not parallel!
            return new ParallelQuadRope<S>(QuadRopeModule.hfold(Functions.toFunc2(f), states.rope, rope));
        }

        public override IQuadRope<S> FoldVertically<S>(Func<S, T, S> f, IQuadRope<S> states)
        {
            // NB: Not parallel!
            return new ParallelQuadRope<S>(QuadRopeModule.vfold(Functions.toFunc2(f), states.rope, rope));
        }

        public override IQuadRope<S> Map<S>(Func<T, S> f)
        {
            return new ParallelQuadRope<S>(Parallel.QuadRopeModule.map(Functions.toFunc1(f), rope));
        }

        public override IQuadRope<S> MapReduceHorizontally<S>(Func<T, S> f, Func<S, S, S> g)
        {
            return new ParallelQuadRope<S>(Parallel.QuadRopeModule.hmapreduce(Functions.toFunc1(f),
                                                                              Functions.toFunc2(g),
                                                                              rope));
        }

        public override IQuadRope<S> MapReduceVertically<S>(Func<T, S> f, Func<S, S, S> g)
        {
            return new ParallelQuadRope<S>(Parallel.QuadRopeModule.vmapreduce(Functions.toFunc1(f),
                                                                              Functions.toFunc2(g),
                                                                              rope));
        }

        public override IQuadRope<T> ReduceHorizontally(Func<T, T, T> f)
        {
            return new ParallelQuadRope<T>(Parallel.QuadRopeModule.hreduce(Functions.toFunc2(f), rope));
        }

        public override IQuadRope<T> ReduceVertically(Func<T, T, T> f)
        {
            return new ParallelQuadRope<T>(Parallel.QuadRopeModule.vreduce(Functions.toFunc2(f), rope));
        }

        public override IQuadRope<T> ReverseHorizontally()
        {
            return new ParallelQuadRope<T>(Parallel.QuadRopeModule.hrev(rope));
        }

        public override IQuadRope<T> ReverseVertically()
        {
            return new ParallelQuadRope<T>(Parallel.QuadRopeModule.vrev(rope));
        }

        public override IQuadRope<T> ScanHorizontally(Func<T, T, T> f, Func<int, T> states)
        {
            // NB: Not parallel!
            return new ParallelQuadRope<T>(QuadRopeModule.hscan(Functions.toFunc2(f),
                                                                Functions.toFunc1(states),
                                                                rope));
        }

        public override IQuadRope<T> ScanVertically(Func<T, T, T> f, Func<int, T> states)
        {
            // NB: Not parallel!
            return new ParallelQuadRope<T>(QuadRopeModule.vscan(Functions.toFunc2(f),
                                                                Functions.toFunc1(states),
                                                                rope));
        }

        public override IQuadRope<T> Set(int row, int col, T v)
        {
            // NB: Not parallel!
            return new ParallelQuadRope<T>(QuadRopeModule.set(rope, row, col, v));
        }

        public override IQuadRope<T> Slice(int row, int col, int rows, int cols)
        {
            // NB: Not parallel!
            return new ParallelQuadRope<T>(QuadRopeModule.slice(rope, row, col, rows, cols));
        }

        public override IQuadRope<T> Transpose()
        {
            return new ParallelQuadRope<T>(Parallel.QuadRopeModule.transpose(rope));
        }

    }
}
