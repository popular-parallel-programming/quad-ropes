module RadTrees.Types

[<CustomEquality;NoComparison>]
type 'a ArraySlice when 'a : equality = ArraySlice of int * int * int * int * 'a [,] with

    static member private equals (ArraySlice (i0, j0, h0, w0, arr0)) (ArraySlice (i1, j1, h1, w1, arr1)) =
        if h0 = h1 && w0 = w1 then
            let mutable eq = true
            for i in 0 .. h0 - 1 do
                for j in 0 .. w0 - 1 do
                    eq <- eq && arr0.[i + i0, j + j0] = arr1.[i + i1, j + j1]
            eq
        else
            false

    override this.Equals(o) =
        match o with
            | :? ('a ArraySlice) as other -> ArraySlice.equals this other
            | _ -> false

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a QuadRope when 'a : equality =
    | Empty
    | Leaf of 'a ArraySlice
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope
    | Slice of int * int * int * int * 'a QuadRope
