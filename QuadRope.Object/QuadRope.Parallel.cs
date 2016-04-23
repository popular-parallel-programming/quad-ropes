// -*- c-basic-offset: 4; indent-tabs-mode: nil -*-

using System;

namespace RadTrees.QuadRope.Object
{
    public class ParallelQuadRope<T> : IQuadRope<T>
    {
	private ParallelQuadRope(RadTrees.QuadRope<T> rope)
	    : base(rope)
	{ }

        public override IQuadRope<T> BalanceHorizontally()
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> BalanceVertically()
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> ConcatHorizontally(IQuadRope<T> other)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> ConcatVertically(IQuadRope<T> other)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<S> FoldHorizontally<S>(Func<S, T, S> f, IQuadRope<S> states)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<S> FoldVertically<S>(Func<S, T, S> f, IQuadRope<S> states)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<S> Map<S>(Func<T, S> f)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<S> MapReduceHorizontally<S>(Func<T, S> f, Func<S, S, S> g)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<S> MapReduceVertically<S>(Func<T, S> f, Func<S, S, S> g)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> ReduceHorizontally(Func<T, T, T> f)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> ReduceVertically(Func<T, T, T> f)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> ReverseHorizontally()
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> ReverseVertically()
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> ScanHorizontally(Func<T, T, T> f, Func<int, T> states)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> ScanVertically(Func<T, T, T> f, Func<int, T> states)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> Set(int row, int col, T v)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> Slice(int row, int col, int rows, int cols)
        {
            throw new NotImplementedException();
        }

        public override IQuadRope<T> Transpose()
        {
            throw new NotImplementedException();
        }
    }
}
