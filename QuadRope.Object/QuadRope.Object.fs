// Copyright (c) 2017 Florian Biermann, fbie@itu.dk

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

namespace QuadRope.QuadRope.Object

open System

open QuadRope
open QuadRope.Types
open QuadRope.Utils

/// An object wrapper for the functional quad rope type.
type QuadRope<'a when 'a : equality> internal (qr : 'a Types.QuadRope) =

    // The actual quad rope structure.
    member internal this.qr = qr


    /// Initialize a new quad rope of r rows and c cols. The value at
    /// each index pair (i, h) is initialized by calling function f on
    /// the index values.
    static member Init(r, c, f) =
        new QuadRope<_>(QuadRope.init r c f)

    /// Initialize a new quad rope of r rows and c cols in
    /// parallel. The value at each index pair (i, h) is initialized
    /// by calling function f on the index values.
    static member InitPar(r, c, f) =
        new QuadRope<_>(Parallel.QuadRope.init r c f)

    /// Initialize a new quad rope of r rows and c cols. The value at
    /// each index pair (i, h) is initialized by calling Func f on
    /// the index values.
    static member InitFunc(r, c, f) =
        new QuadRope<_>(QuadRope.init r c (Functions.toFunc2 f))

    /// Initialize a new quad rope of r rows and c cols in
    /// parallel. The value at each index pair (i, h) is initialized
    /// by calling Func f on the index values.
    static member InitFuncPar(r, c, f) =
        new QuadRope<_>(Parallel.QuadRope.init r c (Functions.toFunc2 f))



    /// Initialize a quad rope from a 2D array.
    static member FromArray2D array =
        new QuadRope<_>(QuadRope.fromArray2D array)

    /// Initialize a quad rope from a 2D array.
    static member FromArray2DPar array =
        new QuadRope<_>(Parallel.QuadRope.fromArray2D array)



    /// Construct a 2D array from this quad rope.
    member this.ToArray2D() =
        QuadRope.toArray2D this.qr

    /// Construct a 2D array from this quad rope in parallel.
    member this.ToArray2DPar() =
        Parallel.QuadRope.toArray2D this.qr



    /// The number of rows in this quad rope.
    member this.Rows = QuadRope.rows this.qr

    /// The number of columns in this quad rope.
    member this.Cols = QuadRope.cols this.qr

    /// True if this quad rope is empty; false otherwise.
    member this.IsEmpty = QuadRope.isEmpty this.qr



    /// Get the value at row i, column j.
    member this.Get(i, j) =
        QuadRope.get this.qr i j

    /// Set the value at row i, column j to v.
    member this.Set(i, j, v) =
        new QuadRope<_>(QuadRope.set this.qr i j v)


    member this.Item
        with get(i, j) = this.Get(i, j)


    /// Combine this and another quad rope in row-direction. The
    /// elements of this quad rope will be "above" the elements of the
    /// other quad rope.
    member this.VCat(other : QuadRope<_>) =
        new QuadRope<_>(QuadRope.vcat this.qr other.qr)

    /// Combine this and another quad rope in column-direction. The
    /// elements of this quad rope will be "left of" the elements of
    /// the other quad rope.
    member this.HCat(other : QuadRope<_>) =
        new QuadRope<_>(QuadRope.hcat this.qr other.qr)



    /// Reverse the row order.
    member this.ReverseRows() =
        new QuadRope<_>(QuadRope.vrev this.qr)

    /// Reverse the row order in parallel.
    member this.ReverseRowsPar() =
        new QuadRope<_>(Parallel.QuadRope.vrev this.qr)

    /// Reverse the column order.
    member this.ReverseCols() =
        new QuadRope<_>(QuadRope.hrev this.qr)

    /// Reverse the column order in parallel.
    member this.ReverseColsPar() =
        new QuadRope<_>(Parallel.QuadRope.hrev this.qr)

    /// Transpose the quad rope.
    member this.Transpose() =
        new QuadRope<_>(QuadRope.transpose this.qr)

    /// Transpose the quad rope.
    member this.TransposePar() =
        new QuadRope<_>(Parallel.QuadRope.transpose this.qr)



    /// Apply function f to all elements.
    member this.Map f =
        new QuadRope<_>(QuadRope.map f this.qr)

    /// Apply function to to all elements and their indices.
    member this.Mapi f =
        new QuadRope<_>(QuadRope.mapi f this.qr)

    /// Apply a Func to all elements.
    member this.MapFunc f =
        new QuadRope<_>(QuadRope.map (Functions.toFunc1 f) this.qr)

    /// Apply a Func to all elements and their indices.
    member this.MapiFunc f =
        new QuadRope<_>(QuadRope.mapi (Functions.toFunc3 f) this.qr)

    /// Apply function f to all elements.
    member this.MapPar f =
        new QuadRope<_>(Parallel.QuadRope.map f this.qr)

    /// Apply function to to all elements and their indices.
    member this.MapiPar f =
        new QuadRope<_>(Parallel.QuadRope.mapi f this.qr)

    /// Apply a Func to all elements.
    member this.MapFuncPar f =
        new QuadRope<_>(Parallel.QuadRope.map (Functions.toFunc1 f) this.qr)

    /// Apply a Func to all elements and their indices.
    member this.MapiFuncPar f =
        new QuadRope<_>(Parallel.QuadRope.mapi (Functions.toFunc3 f) this.qr)



    /// Map function f to all elements and then combine the results
    /// with function g, starting from the initial value epsilon.
    member this.MapReduce(f, g, epsilon) =
        QuadRope.mapreduce f g epsilon this.qr

    /// Map Func f to all elements and then combine the results
    /// with function g, starting from the initial value epsilon.
    member this.MapReduceFunc(f, g, epsilon) =
        QuadRope.mapreduce (Functions.toFunc1 f) (Functions.toFunc2 g) epsilon this.qr

    /// Combine all elements using function g, starting from epsilon.
    member this.Reduce(g, epsilon) =
        QuadRope.reduce g epsilon this.qr

    /// Combine all elements using Func g, starting from epsilon.
    member this.ReduceFunc(g, epsilon) =
        QuadRope.reduce (Functions.toFunc2 g) epsilon this.qr



    /// Map function f to all elements and then combine the results
    /// with function g, starting from the initial value epsilon.
    member this.MapReducePar(f, g, epsilon) =
        Parallel.QuadRope.mapreduce f g epsilon this.qr

    /// Map Func f to all elements and then combine the results
    /// with function g, starting from the initial value epsilon.
    member this.MapReduceFuncPar(f, g, epsilon) =
        Parallel.QuadRope.mapreduce (Functions.toFunc1 f) (Functions.toFunc2 g) epsilon this.qr

    /// Combine all elements using function g, starting from epsilon.
    member this.ReducePar(g, epsilon) =
        Parallel.QuadRope.reduce g epsilon this.qr

    /// Combine all elements using Func g, starting from epsilon.
    member this.ReduceFuncPar(g , epsilon) =
        Parallel.QuadRope.reduce (Functions.toFunc2 g) epsilon this.qr



    /// Combine this and another quad rope element-wise using function
    /// f. Requires that both quad ropes have equally many rows and
    /// columns. Will throw if this is not the case.
    member this.ZipWith(f, other : QuadRope<_>) =
        new QuadRope<_>(QuadRope.zip f this.qr other.qr)

    /// Combine this and another quad rope element-wise using Func
    /// f. Requires that both quad ropes have equally many rows and
    /// columns. Will throw if this is not the case.
    member this.ZipWithFunc(f, other : QuadRope<_>) =
        new QuadRope<_>(QuadRope.zip (Functions.toFunc2 f) this.qr other.qr)

    /// Combine this and another quad rope element-wise in parallel
    /// using function f. Requires that both quad ropes have equally
    /// many rows and columns. Will throw if this is not the case.
    member this.ZipWithPar(f, other : QuadRope<_>) =
        new QuadRope<_>(Parallel.QuadRope.zip f this.qr other.qr)

    /// Combine this and another quad rope element-wise in parallel
    /// using Func f. Requires that both quad ropes have equally many
    /// rows and columns. Will throw if this is not the case.
    member this.ZipWithFuncPar(f, other : QuadRope<_>) =
        new QuadRope<_>(Parallel.QuadRope.zip (Functions.toFunc2 f) this.qr other.qr)



    /// Compute the partial prefix sum of the elements in this quad
    /// rope using function f and starting all columns and rows from
    /// epsilon.
    member this.Scan(f, epsilon) =
        new QuadRope<_>(QuadRope.scan f epsilon this.qr)

    /// Compute the partial prefix sum of the elements in this quad
    /// rope using Func f and starting all columns and rows from
    /// epsilon.
    member this.ScanFunc(f, epsilon) =
        new QuadRope<_>(QuadRope.scan (Functions.toFunc4 f) epsilon this.qr)

    /// Compute the partial prefix sum of the elements in this quad
    /// rope in parallel using function f and starting all columns and
    /// rows from epsilon.
    member this.ScanPar(f, epsilon) =
        new QuadRope<_>(Parallel.QuadRope.scan f epsilon this.qr)

    /// Compute the partial prefix sum of the elements in this quad
    /// rope in parallel using Func f and starting all columns and
    /// rows from epsilon.
    member this.ScanFuncPar(f, epsilon) =
        new QuadRope<_>(Parallel.QuadRope.scan (Functions.toFunc4 f) epsilon this.qr)


    /// Compute the partial prefix sum of each row using function f
    /// and starting from epsilon.
    member this.RowScan(f, epsilon) =
        new QuadRope<_>(QuadRope.hscan f (fun _ -> epsilon) this.qr)

    /// Compute the partial prefix sum of each row using Func f
    /// and starting from epsilon.
    member this.RowScanFunc(f, epsilon) =
        new QuadRope<_>(QuadRope.hscan (Functions.toFunc2 f) (fun _ -> epsilon) this.qr)

    /// Compute the partial prefix sum of each column using function f
    /// and starting from epsilon.
    member this.ColScan(f, epsilon) =
        new QuadRope<_>(QuadRope.vscan f (fun _ -> epsilon) this.qr)

    /// Compute the partial prefix sum of each column using function f
    /// and starting from epsilon.
    member this.ColScanFunc(f, epsilon) =
        new QuadRope<'a>(QuadRope.vscan (Functions.toFunc2 f) (fun _ -> epsilon) this.qr)



    /// Return true if there exists an element in this quad rope for
    /// which the property p holds. False otherwise.
    member this.Exists p =
        QuadRope.exists p this.qr

    /// Return true if the property p holds for all elements in this
    /// quad rope. False otherwise.
    member this.Forall p =
        QuadRope.forall p this.qr



    // Standard overrides.

    override this.ToString() =
        QuadRope.toString this.qr

    override this.Equals(o : obj) =
        match o with
            | :? QuadRope<'a> as other -> QuadRope.equals this.qr other.qr
            | _ -> false

    override this.GetHashCode() =
        let mutable code = 51
        QuadRope.iter (fun v -> code <- code + (73 * FSharp.Core.LanguagePrimitives.GenericHash v)) this.qr
        code



    /// A generic sparse quad rope that can optimize some operations
    /// for an arbitrary ring. Use this to define sparse quad rope
    /// variants of your own types.
    type SparseQuadRope<'a when 'a : equality> internal (zero : 'a,
                                                         one : 'a,
                                                         plus : 'a -> 'a -> 'a,
                                                         mul : 'a -> 'a -> 'a,
                                                         qr : 'a Types.QuadRope) =
        inherit QuadRope<'a>(qr)

        member internal this.zero = zero
        member internal this.one = one
        member internal this.plus = plus
        member internal this.mul = mul

        /// Compute the generic sum of this quad rope's elements.
        member this.Sum() =
            QuadRope.reduce this.plus this.zero this.qr

        /// Compute the generic product of this quad rope's elements.
        member this.Product() =
            QuadRope.Sparse.prod this.mul this.zero this.one this.qr

        /// Compute the point-wise generic product of two quad ropes.
        member this.PointwiseMultiply (other : QuadRope<_>) =
            new SparseQuadRope<_>(this.zero,
                                  this.one,
                                  this.plus,
                                  this.mul,
                                  QuadRope.Sparse.pointwise this.mul this.zero this.one this.qr other.qr)


        /// Initialize a new identity matrix of size n * n for the
        /// generic ring (zero, one, plus, mul).
        static member Identity(zero, one, plus, mul, n) =
            new SparseQuadRope<_>(zero,
                                  one,
                                  plus,
                                  mul,
                                  QuadRope.Sparse.identity zero one n)

        /// Initialize a new upper diagonal matrix of size n * n for
        /// the generic ring (zero, one, plus, mul).
        static member UpperDiagonal(zero, one, plus, mul, n) =
            new SparseQuadRope<_>(zero,
                                  one,
                                  plus,
                                  mul,
                                  QuadRope.Sparse.upperDiagonal zero n one)


        /// Initialize a new lower diagonal matrix of size n * n for
        /// the generic ring (zero, one, plus, mul).
        static member LowerDiagonal(zero, one, plus, mul, n) =
            new SparseQuadRope<_>(zero,
                                  one,
                                  plus,
                                  mul,
                                  QuadRope.Sparse.lowerDiagonal zero n one)




    /// A sparse quad rope of doubles that performs optimization
    /// applicable to the ring (0, 1, +, *).
    type DoubleQuadRope internal (qr : float Types.QuadRope) =
        inherit QuadRope<float>(qr)

        /// Compute the sum of all numbers.
        member this.Sum() =
            QuadRope.SparseDouble.sum this.qr

        /// Compute the sum of all numbers in parallel.
        member this.SumPar() =
            Parallel.QuadRope.SparseDouble.sum this.qr



        /// Compute the product of all numbers.
        member this.Product() =
            QuadRope.SparseDouble.prod this.qr

        /// Compute the product of all numbers in parallel.
        member this.ProductPar() =
            Parallel.QuadRope.SparseDouble.prod this.qr



        /// Generate a new identity matrix quad rope of size n * n.
        static member Identity n =
            QuadRope.SparseDouble.identity n

        /// Generate a new upper diagonal matrix of size n * n with v
        /// at all non-zero indexes.
        static member UpperDiagonal(n, v) =
            new DoubleQuadRope(QuadRope.SparseDouble.upperDiagonal n v)

        /// Generate a new lower diagonal matrix of size n * n with v
        /// at all non-zero indexes.
        static member LowerDiagonal(n, v) =
            new DoubleQuadRope(QuadRope.SparseDouble.lowerDiagonal n v)



        /// Multiply the values of this quad rope to the values of
        /// another quad rope point-wise.
        member this.PointwiseMultiply (other : QuadRope<_>) =
            new DoubleQuadRope(QuadRope.SparseDouble.pointwise this.qr other.qr)

        /// Multiply the values of this quad rope to the values of
        /// another quad rope point-wise.
        member this.PointwiseMultiplyPar (other : QuadRope<_>) =
            new DoubleQuadRope(Parallel.QuadRope.SparseDouble.pointwise this.qr other.qr)
