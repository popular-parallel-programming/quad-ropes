#load "Utils.fs"
#load "RadixTree2D.fs"
#load "QuadRope.fs"
#load "QuadRope.Parallel.fs"

open RadTrees

let init = QuadRope.init
let rows = QuadRope.rows
let cols = QuadRope.cols
let depth = QuadRope.depth
let get = QuadRope.get
let set = QuadRope.set
let write = QuadRope.write
let split = QuadRope.split
let hcat = QuadRope.hcat
let vcat = QuadRope.vcat
let isBalanced = QuadRope.isBalanced

let start = QuadRope.Path.start

let next (a, b) =
    match QuadRope.Parallel.next a b with
        | QuadRope.Parallel.Done rp -> rp, QuadRope.Top
        | QuadRope.Parallel.More (rp, path) -> rp, path

let balance = QuadRope.balance
let flatten = QuadRope.flatten

let a = init 5 5 (*)
let b = init 5 10 (*)
let c = init 10 5 (*)

let aStart : int QuadRope * (int, int) QuadRope.Path = start a
let bStart : int QuadRope * (int, int) QuadRope.Path = start b
