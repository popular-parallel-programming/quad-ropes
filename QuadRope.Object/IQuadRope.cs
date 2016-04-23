// -*- c-basic-offset: 4; indent-tabs-mode: nil -*-

using System;

namespace RadTrees.QuadRope.Object
{
    public abstract class IQuadRope<T>
    {
        protected internal QuadRope<T> rope;

        /// <summary>
        ///   Construct a new wrapper instance for the rope parameter.
        /// </summary>
        protected IQuadRope(QuadRope<T> rope)
        {
            this.rope = rope;
        }

        /// <summary>
        ///   The number of rows of this instance.
        /// </summary>
        public int Rows
        {
            get { return QuadRopeModule.rows(rope); }
        }

        /// <summary>
        ///   The number of columns of this instance.
        /// </summary>
        public int Cols
        {
            get { return QuadRopeModule.cols(rope); }
        }

        /// <summary>
        ///   True if this instance contains no values, false
        ///   otherwise.
        /// </summary>
        public bool IsEmpty
        {
            get { return QuadRopeModule.isEmpty(rope); }
        }

        /// <summary>
        ///   Return the value stored at index row, col.
        /// </summary>
        public T Get(int row, int col)
        {
            return QuadRopeModule.get<T>(rope, row, col);
        }

        public T this [int row, int col]
        {
            get { return Get(row, col); }
        }

        /// <summary>
        ///   Return a new quad rope instance where the value at index
        ///   row, col has been replaced with v. This does not modify
        ///   the current instance.
        /// </summary>
        public abstract IQuadRope<T> Set(int row, int col, T v);

        /// <summary>
        ///   Overwrite the value at index row, col with v. This will
        ///   change this instance and all instances that share
        ///   data. Use judiciously!
        /// </summary>
        public void Write(int row, int col, T v)
        {
            QuadRopeModule.write<T>(rope, row, col, v);
        }

        /// <summary>
        ///   True if the rope is horizontally balanced, false otherwise.
        /// </summary>
        public bool IsBalancedHorizontally
        {
            get { return QuadRopeModule.isBalancedH(rope); }
        }

        /// <summary>
        ///   True if the rope is vertically balanced, false otherwise.
        /// </summary>
        public bool IsBalancedVertically
        {
            get { return QuadRopeModule.isBalancedV(rope); }
        }

        /// <summary>
        ///   Create a new instance where the underlying rope is
        ///   horizontally balanced.
        /// </summary>
        public abstract IQuadRope<T> BalanceHorizontally();

        /// <summary>
        ///   Cerate a new instance where the underlying rope is
        ///   vertically balanced.
        /// </summary>
        public abstract IQuadRope<T> BalanceVertically();

        /// <summary>
        ///   Concatenate the rows of this and that.
        /// </summary>
        public abstract IQuadRope<T> ConcatHorizontally(IQuadRope<T> other);

        /// <summary>
        ///   Concatenate the columns of this and that.
        /// </summary>
        public abstract IQuadRope<T> ConcatVertically(IQuadRope<T> other);

        /// <summary>
        ///   Produce a new rope starting at index row, col taking as
        ///   many rows and cols as specified. If the difference
        ///   between the last row or column to the row, col index is
        ///   less than the specified number of rows or columns,
        ///   respectively, the lesser amount of rows or columns
        ///   counts.
        /// </summary>
        public abstract IQuadRope<T> Slice(int row, int col, int rows, int cols);

        /// <summary>
        ///   Produce a new rope where the order of columns is reversed.
        /// </summary>
        public abstract IQuadRope<T> ReverseHorizontally();

        /// <summary>
        ///   Produce a new rope where the order of rows is reversed.
        /// </summary>
        public abstract IQuadRope<T> ReverseVertically();

        /// <summary>
        ///   Produce a new transposed rope.
        /// </summary>
        public abstract IQuadRope<T> Transpose();

        /// <summary>
        ///   Produce a new rope with f applied to every value.
        /// </summary>
        public abstract IQuadRope<S> Map<S>(Func<T, S> f);

        /// <summary>
        ///   Produce a new thin rope of a single column with f
        ///   applied to every value and the single rows reduced by g.
        /// </summary>
        public abstract IQuadRope<S> MapReduceHorizontally<S>(Func<T, S> f, Func<S, S, S> g);

        /// <summary>
        ///   Produce a new flat rope of a single row with f applied
        ///   to every value and the single columns reduced by g.
        /// </summary>
        public abstract IQuadRope<S> MapReduceVertically<S>(Func<T, S> f, Func<S, S, S> g);

        /// <summary>
        ///   Produce a new thin rope of a single column with the
        ///   single rows reduced by f.
        /// </summary>
        public abstract IQuadRope<T> ReduceHorizontally(Func<T, T, T> f);

        /// <summary>
        ///   Produce a new flat rope of a single row with the
        ///   single columns reduced by f.
        /// </summary>
        public abstract IQuadRope<T> ReduceVertically(Func<T, T, T> f);

        /// <summary>
        ///   Fold the rope row-wise using f and starting from
        ///   states. States must be a single-column rope with the
        ///   same number of rows as this.
        /// </summary>
        public abstract IQuadRope<S> FoldHorizontally<S>(Func<S, T, S> f, IQuadRope<S> states);

        /// <summary>
        ///   Fold the rope column-wise using f and starting from
        ///   states. States must be a single-row rope with the same
        ///   number of columns as this.
        /// </summary>
        public abstract IQuadRope<S> FoldVertically<S>(Func<S, T, S> f, IQuadRope<S> states);

        /// <summary>
        ///   Compute the row-wise prefix-sum for the associative
        ///   function f. States is a function that produces an
        ///   initial value for each row. The resulting rope is
        ///   single-column.
        /// </summary>
        public abstract IQuadRope<T> ScanHorizontally(Func<T, T, T> f, Func<int, T> states);

        /// <summary>
        ///   Compute the column-wise prefix-sum for the associative
        ///   function f. States is a function that produces an
        ///   initial value for each column. The resulting rope is
        ///   single-row.
        /// </summary>
        public abstract IQuadRope<T> ScanVertically(Func<T, T, T> f, Func<int, T> states);

        private static Func<bool, bool, bool> lambdaAnd = (b0, b1) => b0 && b1;

        /// <summary>
        ///   True if all values in this rope satisfy predicate
        ///   p. False otherwise.
        /// </summary>
        public bool ForAll(Func<T, bool> p)
        {
            return IsEmpty || MapReduceHorizontally(p, lambdaAnd).ReduceVertically(lambdaAnd)[0, 0];
        }

        private static Func<bool, bool, bool> lambdaOr = (b0, b1) => b0 || b1;

        /// <summary>
        ///   True if there exists an element in this rope what
        ///   satisfies predicate p. False otherwise.
        /// </summary>
        public bool Exists(Func<T, bool> p)
        {
            return !IsEmpty && MapReduceHorizontally(p, lambdaAnd).ReduceVertically(lambdaOr)[0, 0];
        }
    }
}
