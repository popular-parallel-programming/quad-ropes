#load "Utils.fs"
#load "RadixTree2D.fs"
#load "QuadRope.fs"

open RadTrees

let init = QuadRope.init
let rows = QuadRope.rows
let cols = QuadRope.cols
let get = QuadRope.get
let split = QuadRope.split
let hcat = QuadRope.hcat
let vcat = QuadRope.vcat

let a = init 5 5 (*)
let b = init 5 10 (*)
