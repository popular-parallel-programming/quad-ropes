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

namespace RadTrees.QuadRope.Object
{
    public abstract class IQuadRope<T>
    {
        protected internal Types.QuadRope<T> qr;

        /// <summary>
        ///   Construct a new wrapper instance for the rope parameter.
        /// </summary>
        protected IQuadRope(Types.QuadRope<T> qr)
        {
            this.qr = qr;
        }

        /// <summary>
        ///   The number of rows of this instance.
        /// </summary>
        public int Rows
        {
            get { return QuadRopeModule.rows(qr); }
        }

        /// <summary>
        ///   The number of columns of this instance.
        /// </summary>
        public int Cols
        {
            get { return QuadRopeModule.cols(qr); }
        }

        /// <summary>
        ///   True if this instance contains no values, false
        ///   otherwise.
        /// </summary>
        public bool IsEmpty
        {
            get { return QuadRopeModule.isEmpty(qr); }
        }

        /// <summary>
        ///   Return the value stored at index row, col.
        /// </summary>
        public T Get(int row, int col)
        {
            return QuadRopeModule.get<T>(qr, row, col);
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
        ///   Apply f to all elements of the rope and reduce all
        ///   elements to a single scalar by applying g.
        /// </summary>
        public abstract S MapReduce<S>(Func<T, S> f, Func<S, S, S> g, S epsilon);

        /// <summary>
        ///   Produce a new thin rope of a single column with f
        ///   applied to every value and the single rows reduced by g.
        /// </summary>
        public abstract IQuadRope<S> MapReduceHorizontally<S>(Func<T, S> f, Func<S, S, S> g, S epsilon);

        /// <summary>
        ///   Produce a new flat rope of a single row with f applied
        ///   to every value and the single columns reduced by g.
        /// </summary>
        public abstract IQuadRope<S> MapReduceVertically<S>(Func<T, S> f, Func<S, S, S> g, S epsilon);

        /// <summary>
        ///   Reduce the rope to a single scalar value by applying f.
        /// </summary>
        public abstract T Reduce(Func<T, T, T> f, T epsilon);

        /// <summary>
        ///   Produce a new thin rope of a single column with the
        ///   single rows reduced by f.
        /// </summary>
        public abstract IQuadRope<T> ReduceHorizontally(Func<T, T, T> f, T epsilon);

        /// <summary>
        ///   Produce a new flat rope of a single row with the
        ///   single columns reduced by f.
        /// </summary>
        public abstract IQuadRope<T> ReduceVertically(Func<T, T, T> f, T epsilon);

        /// <summary>
        ///   Compute the 2D prefix-sum for the associative operator
        ///   <code>plus</code>. The function <code>minus</code> is
        ///   assumed to be the inverse of <code>plus</code>.
        /// </summary>
        public abstract IQuadRope<T> Scan(Func<T, T, T, T, T> f, T initial);

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

        /// <summary>
        ///   True if all values in this rope satisfy predicate
        ///   p. False otherwise.
        /// </summary>
        public abstract bool ForAll(Func<T, bool> p);

        /// <summary>
        ///   True if there exists an element in this rope what
        ///   satisfies predicate p. False otherwise.
        /// </summary>
        public abstract bool Exists(Func<T, bool> p);
    }
}
