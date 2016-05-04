module RadTrees.Types

type 'a Array2DView =
    | All of 'a [,]
    | View of int * int * int * int * 'a [,]

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a QuadRope =
    | Empty
    | Leaf of 'a Array2DView
    | Node of int * int * int * 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope
