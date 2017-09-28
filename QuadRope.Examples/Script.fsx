#load "../QuadRope/Types.fs"
#load "../QuadRope/Utils.fs"
#load "../QuadRope/Array2D.fs"
#load "../QuadRope/Array2D.Parallel.fs"
#load "../QuadRope/ArraySlice.fs"
#load "../QuadRope/Target.fs"
#load "../QuadRope/QuadRope.fs"
#load "../QuadRope/QuadRope.Parallel.fs"

#load "MatMult.fs"
#load "SmithWaterman.fs"
#load "VanDerCorput.fs"

open QuadRope
open QuadRope.Examples

let a = array2D [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]]
let b = array2D [[6.0; -1.0]; [3.0; 2.0]; [0.0; -3.0]]

let a' = QuadRope.fromArray2D a
let b' = QuadRope.fromArray2D b

let prefixSum r d c v =
    printfn "r = %A; d = %A; c = %A; v = %A" r d c v
    r + c + v - d

let prefixSumArr =
    Array2DExt.scan prefixSum 0

let prefixSumQr =
    QuadRope.fromArray2D >> QuadRope.scan prefixSum 0 >> QuadRope.toArray2D


/// Generalized reversal in both dimensions.
let rev = QuadRope.hrev >> QuadRope.vrev


/// Return 1 if n is greater than 0, otherwise 0.
let truncate n = if n > 0 then 1 else 0


/// Count the number of neighbors as prefixes
let count c d r _ =
    truncate c + truncate d + truncate r


/// Evolve a cell based on its surrounding cells.
let evolve state count =
    match count with
        | 2 -> state // Live on
        | 3 -> 1     // Live on or be born.
        | _ -> 0     // All other cases involve death.


/// Conway's game of life, using 2D-scan, reversal and zip.
let rec gameOfLife n world =
    match n with
        | 0 -> world
        | n ->
            QuadRope.zip (+)
                         (QuadRope.scan count 0 world)
                         (rev (QuadRope.scan count 0 (rev world)))
            |> QuadRope.zip evolve world
            |> gameOfLife (n - 1)
