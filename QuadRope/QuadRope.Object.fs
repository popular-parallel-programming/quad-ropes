namespace RadTrees.QuadRope

module Object =

    open System
    open RadTrees
    open Functions

    [<CompiledNameAttribute("QuadRope")>]
    type 'a SeqRope private (rope : 'a QuadRope) =

        member private this.rope = rope

        /// Initialize rope of height h, width w with function f.
        static member Init (h, w, f) =
            SeqRope (QuadRope.init h w (toFunc2 f))

        /// Initialize rope of height h, width w with e at every
        /// index.
        static member Init (h, w, e) =
            SeqRope (QuadRope.initAll h w e)

        /// Initialize rope of height h, width w containing only
        /// zeros.
        static member InitZeros (h, w) =
            SeqRope (QuadRope.initZeros h w)

        /// Return the number of rows in this rope.
        member this.Rows with get() = QuadRope.rows rope

        /// Return the number of columns in this rope.
        member this.Cols with get () = QuadRope.cols rope

        /// Check whether this rope is empty.
        member this.IsEmpty with get() = QuadRope.isEmpty rope

        /// Get the value at index i, j.
        member this.Get (i, j) =
            QuadRope.get this.rope i j

        /// Set the value at index i, j to v. This function is
        /// persistent.
        member this.Set (i, j, v) =
            SeqRope (QuadRope.set rope i j v)

        /// Overwrite the value at index i, j with v. This function
        /// uses a side-effect.
        member this.Write (i, j, v) =
            QuadRope.write rope i j v |> ignore

        /// Check whether this rope is horizontally balanced.
        member this.IsBalancedH with get() = QuadRope.isBalancedH rope

        /// Check whether this rope is vertically balanced.
        member this.IsBalancedV with get() = QuadRope.isBalancedV rope

        /// Produce a horizontally balanced rope.
        member this.BalanceH () =
            SeqRope (QuadRope.hbalance rope)

        /// Produce a vertically balanced rope.
        member this.BalanceV () =
            SeqRope (QuadRope.vbalance rope)

        /// Concatenate this rope and the other rope horizontally.
        member this.CatH (other : _ SeqRope) =
            SeqRope (QuadRope.hcat rope other.rope)

        /// Concatenate this rope and the other rope vertically.
        member this.CatV (other : _ SeqRope) =
            SeqRope (QuadRope.vcat rope other.rope)

        /// Produce a sub-rope starting from index i, j taking h rows
        /// and w columns. The number of rows and columns are the
        /// minimum of the passed argument and the distance of the
        /// initial index to the last row or column of this rope.
        member this.Sub (row, col, h, w) =
            SeqRope (QuadRope.split rope row col h w)

        /// Reverse the columns of this rope.
        member this.ReverseCols () =
            SeqRope (QuadRope.hrev rope)

        /// Reverse the rows of this rope.
        member this.ReverseRows () =
            SeqRope (QuadRope.vrev rope)

        /// Compute the transpose of this rope.
        member this.Transpose () =
            SeqRope (QuadRope.transpose rope)

        /// Apply function f to all values in this rope.
        member this.Map f =
            SeqRope (QuadRope.map (toFunc1 f) rope)

        /// Apply function f to all values in this rope and reduce
        /// them row-wise by function g.
        member this.MapReduceH (f, g) =
            SeqRope (QuadRope.mapHreduce (toFunc1 f) (toFunc2 g) rope)

        /// Apply function f to all values in this rope and reduce
        /// them column-wise by function g.
        member this.MapReduceV (f, g) =
            SeqRope (QuadRope.mapVreduce (toFunc1 f) (toFunc2 g) rope)

        /// Reduce this rope row-wise by function f.
        member this.ReduceH f =
            SeqRope (QuadRope.hreduce (toFunc2 f) rope)

        /// Reduce this rope column-wise by function f.
        member this.ReduceV f =
            SeqRope (QuadRope.vreduce (toFunc2 f) rope)

        /// Fold this rope row-wise by function f starting from
        /// states. States may only be single-column.
        member this.FoldH (f, states : _ SeqRope) =
            SeqRope (QuadRope.hfold (toFunc2 f) states.rope rope)

        /// Fold this rope column-wise by function f starting from
        /// states. States may only be single-row.
        member this.FoldV (f, states : _ SeqRope) =
            SeqRope (QuadRope.vfold (toFunc2 f) states.rope rope)

        /// Compute the exclusive row-wise prefix-sum of this rope for
        /// function f and a state provider states. The function f
        /// <emph>must</emph> be associative. Otherwise, the result is
        /// undefined.
        member this.ScanH (f, states) =
            SeqRope (QuadRope.hscan f (toFunc1 states) rope)

        /// Compute the exclusive column-wise prefix-sum of this rope
        /// for function f and a state provider states. The function f
        /// <emph>must</emph> be associative. Otherwise, the result is
        /// undefined.
        member this.ScanV (f, states) =
            SeqRope (QuadRope.vscan f (toFunc1 states) rope)

        /// Combine this with other rope element-wise using f.
        member this.ZipWith (f, other : _ SeqRope) =
            SeqRope (QuadRope.zip f rope other.rope)

        /// Check whether the predicate p holds for all elements of
        /// this rope.
        member this.ForAll p =
            QuadRope.forall (toFunc1 p) rope

        /// Check whether there exists any element in rope that
        /// satisfies the predicate p.
        member this.Exists p =
            QuadRope.exists (toFunc1 p) rope

        /// Retain only elements of rope that satisfy p. Throws if rope
        /// has more than one row.
        member this.FilterH p =
            SeqRope (QuadRope.hfilter (toFunc1 p) rope)

        /// Retain only elements of rope that satisfy p. Throws if rope
        /// has more than one column.
        member this.FilterV p =
            SeqRope (QuadRope.vfilter (toFunc1 p) rope)
