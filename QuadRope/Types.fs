module RadTrees.Types

type 'a ArraySlice = ArraySlice of int * int * int * int * 'a [,]

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a QuadRope =
    | Empty
    | Leaf of 'a ArraySlice
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope
    | Slice of int * int * int * int * 'a QuadRope
