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

module RadTrees.Test.QuadRope

open FsCheck
open RadTrees
open Types

module private Utils =

    let makeIndices h w =
        seq { for i in 0..h - 1 do
              for j in 0..w - 1 ->
              i, j }

    let makeIndicesFrom qr =
        makeIndices (QuadRope.rows qr) (QuadRope.cols qr)

    let rec maintainsTight = function
        | Node (_, _, _, _, ne, nw, Empty, Empty) ->
            maintainsTight ne && maintainsTight nw
        | Node (_, _, _, _, Empty, nw, sw, Empty) ->
            maintainsTight nw && maintainsTight sw
        | Node (_, _, _, _, _, Empty, _, _)
        | Node (_, _, _, _, _, _, Empty, _) -> false
        | Node (_, _, _, _, ne, nw, sw, se) ->
            maintainsTight ne && maintainsTight nw && maintainsTight sw && maintainsTight se
        | Slice _ as qr -> maintainsTight qr
        | _ -> true

    let access rope (i, j) =
        try
            ignore (QuadRope.get rope i j)
            true
        with
            | _ -> false

    let equalMap a b f g = fun (i, j) -> f (QuadRope.get a i j) = g (QuadRope.get b i j)
    let equal a b = equalMap a b id id

    let pointWiseEqualMap a b f g =
        Seq.forall (equalMap a b f g) (makeIndices (QuadRope.rows a) (QuadRope.cols a))

    let pointWiseEqual a b = pointWiseEqualMap a b id id

open Utils

type Handle = class end

let ``DEBUG leaf sizes enabled`` () =
    QuadRope.s_max = 4

(* Hight of generated rope is equal to height parameter. *)
let ``init produces correct height`` (NonNegativeInt h) (NonNegativeInt w) =
    (0 < h && 0 < w) ==>
    lazy (let qr = QuadRope.init h w (*)
          QuadRope.rows qr = h &&
          QuadRope.get (QuadRope.vmapreduce (fun _ -> 1) (+) 0 qr) 0 0 = h)

(* Width of generated rope is equal to width parameter. *)
let ``init produces correct width`` (NonNegativeInt h) (NonNegativeInt w) =
    (0 < h && 0 < w) ==>
    lazy (let qr = QuadRope.init h w (*)
          QuadRope.cols qr = w &&
          QuadRope.get (QuadRope.hmapreduce (fun _ -> 1) (+) 0 qr) 0 0 = w)

(* Wavefront initialization with multiplication produces correct values at positions. *)
let ``init produces correct values`` (NonNegativeInt h) (NonNegativeInt w) =
    (0 < h && 0 < w) ==>
    lazy (let qr = QuadRope.init h w (*)
          Seq.forall (fun (i, j) -> QuadRope.get qr i j = i * j) (makeIndices h w))

let ``get is always inside bounds`` (a : int QuadRope) =
    Seq.forall (access a) (makeIndicesFrom a)

(* The width of hcat of two ropes is equal to the sum of their widths. *)
let ``hcat width is equal to width sum`` (a : int QuadRope) (b : int QuadRope)  =
    (QuadRope.rows a = QuadRope.rows b) ==>
    lazy (QuadRope.cols a + QuadRope.cols b = QuadRope.cols (QuadRope.hcat a b))

(* The height of vcat of two ropes is equal to the sum of their heights. *)
let ``vcat height is equal to height sum`` (a : int QuadRope) (b : int QuadRope)  =
    (QuadRope.cols a = QuadRope.cols b) ==>
    lazy (QuadRope.rows a + QuadRope.rows b = QuadRope.rows (QuadRope.vcat a b))

let ``hbalance maintains order`` (NonNegativeInt x) =
    let mutable r = QuadRope.singleton 0
    for i in 1 .. x do
        r <- QuadRope.hcat r (QuadRope.singleton i)
    QuadRope.forallRows (<) r

let ``vbalance maintains order`` (NonNegativeInt x) =
    let mutable r = QuadRope.singleton 0
    for i in 1 .. x do
        r <- QuadRope.vcat r (QuadRope.singleton i)
    QuadRope.forallCols (<) r

let ``hrev maintains uper-left invariant`` (a: int QuadRope) =
    maintainsTight (QuadRope.hrev a)

let ``vrev maintains uper-left invariant`` (a: int QuadRope) =
    maintainsTight (QuadRope.vrev a)

let ``hrev of hrev is identity`` (a : int QuadRope) =
    pointWiseEqual a (QuadRope.hrev (QuadRope.hrev a))

let ``vrev of vrev is identity`` (a : int QuadRope) =
    pointWiseEqual a (QuadRope.vrev (QuadRope.vrev a))

(* hrev puts values in the correct position and get can access them. *)
let ``get accesses hrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
    let h = QuadRope.rows a
    let w = QuadRope.cols a
    (i < h && j < w) ==>
    lazy (QuadRope.get (QuadRope.hrev a) i j = QuadRope.get a i ((w - 1) - j))

(* vrec puts values in the correct position and get can access them. *)
let ``get accesses vrev correctly`` (a: int QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
    let h = QuadRope.rows a
    let w = QuadRope.cols a
    (i < h && j < w) ==>
    lazy (QuadRope.get (QuadRope.vrev a) i j = QuadRope.get a ((h - 1) - i) j)

let ``get accesses hcat correctly`` (a : int QuadRope) (b : int QuadRope) =
    (QuadRope.rows a = QuadRope.rows b) ==>
    lazy (let ab = QuadRope.hcat a b
          Seq.forall (access ab) (makeIndicesFrom ab))

let ``get accesses vcat correctly`` (a : int QuadRope) (b : int QuadRope) =
    (QuadRope.cols a = QuadRope.cols b) ==>
    lazy (let ab = QuadRope.vcat a b
          Seq.forall (access ab) (makeIndicesFrom ab))

let ``get accesses slice correctly`` (a : int QuadRope) (NonNegativeInt i)
                                                        (NonNegativeInt j)
                                                        (NonNegativeInt h)
                                                        (NonNegativeInt w) =
    (i < QuadRope.rows a && i < h && j < QuadRope.cols a && j < w) ==>
    lazy (let b = QuadRope.slice i j h w a
          Seq.forall
            (fun (i0, j0) -> QuadRope.get b i0 j0 = QuadRope.get a (i + i0) (j + j0))
            (makeIndicesFrom b))

let ``slice is equal to materialized slice`` (a : int QuadRope) (NonNegativeInt i)
                                                                (NonNegativeInt j)
                                                                (NonNegativeInt h)
                                                                (NonNegativeInt w) =
    (i < QuadRope.rows a && i < h && j < QuadRope.cols a && j < w) ==>
    lazy (let b = QuadRope.slice i j h w a
          let c = QuadRope.materialize b
          Seq.forall
            (fun (i0, j0) -> QuadRope.get b i0 j0 = QuadRope.get c i0 j0)
            (makeIndicesFrom b))

let ``recursive slice computes correct indices`` (a : int QuadRope) (d : NonNegativeInt) =
    let i = 1
    let j = 1
    let h = QuadRope.rows a - 1
    let w = QuadRope.cols a - 1
    let b = QuadRope.slice i j h w a
    QuadRope.rows (QuadRope.slice (i + d.Get) j h w b) <= QuadRope.rows b &&
    QuadRope.cols (QuadRope.slice i (j + d.Get) h w b) <= QuadRope.cols b &&
    QuadRope.rows (QuadRope.slice (i - d.Get) j h w b) <= QuadRope.rows b &&
    QuadRope.cols (QuadRope.slice i (j - d.Get) h w b) <= QuadRope.cols b &&
    QuadRope.rows (QuadRope.slice i j (h + d.Get) w b) <= QuadRope.rows b &&
    QuadRope.cols (QuadRope.slice i j h (w + d.Get) b) <= QuadRope.cols b &&
    QuadRope.rows (QuadRope.slice i j (h - d.Get) w b) <= QuadRope.rows b &&
    QuadRope.cols (QuadRope.slice i j h (w - d.Get) b) <= QuadRope.cols b

let ``hslice produces ropes of correct width`` (a : int QuadRope) (NonNegativeInt w) =
    w <= QuadRope.cols a ==>
    lazy (w = QuadRope.cols (QuadRope.slice 0 0 (QuadRope.rows a) w a))

let ``hsplit2 produces two ropes of correct width`` (a : int QuadRope) (NonNegativeInt w) =
    w <= QuadRope.cols a ==>
    lazy (let b, c = QuadRope.hsplit2 a w
          QuadRope.cols b = w && QuadRope.cols c = QuadRope.cols a - w)

let ``vslice produces ropes of correct height`` (a : int QuadRope) (NonNegativeInt h) =
    h <= QuadRope.rows a ==>
    lazy (h = QuadRope.rows (QuadRope.slice 0 0 h (QuadRope.cols a) a))

let ``vsplit2 produces two ropes of correct height`` (a : int QuadRope) (NonNegativeInt h) =
    h <= QuadRope.rows a ==>
    lazy (let b, c = QuadRope.vsplit2 a h
          (QuadRope.rows b = h) && (QuadRope.rows c = QuadRope.rows a - h))

let ``reallocate results in correctly shaped quad rope`` (a : int QuadRope) =
    let r = QuadRope.map id a
    QuadRope.rows r = QuadRope.rows a && QuadRope.cols r = QuadRope.cols a

let ``reallocate results in logically equal rope`` (a : int QuadRope) =
    pointWiseEqual a (QuadRope.map id a)

let ``balanceH maintains layout`` (a : int QuadRope) =
    not (QuadRope.isBalancedH a) ==> lazy (pointWiseEqual a (QuadRope.hbalance a))

let ``balanceH maintains or improves depth`` (a : int QuadRope) =
    not (QuadRope.isBalancedH a) ==>
    lazy (let b = QuadRope.hbalance a
          QuadRope.depth b <= QuadRope.depth a)

let ``balanceV maintains layout`` (a : int QuadRope) =
    not (QuadRope.isBalancedV a) ==> lazy (pointWiseEqual a (QuadRope.vbalance a))

let ``balanceV maintains or improves depth`` (a : int QuadRope) =
    not (QuadRope.isBalancedV a) ==>
    lazy (let b = QuadRope.vbalance a
          QuadRope.depth b <= QuadRope.depth a)

let ``map modifies all values`` (a : int QuadRope) (f : int -> int) =
    pointWiseEqualMap a (QuadRope.map f a) f id

let ``hreduce produces thin ropes`` (a : int QuadRope) =
    (not (QuadRope.isEmpty a)) ==> lazy (QuadRope.cols (QuadRope.hreduce (+) 0 a) = 1)

let ``vreduce produces flat ropes`` (a : int QuadRope) =
    (not (QuadRope.isEmpty a)) ==> lazy (QuadRope.rows (QuadRope.vreduce (+) 0 a) = 1)

let ``hreduce >> vreduce equals vreduce >> hreduce`` (a : int QuadRope) =
    (not (QuadRope.isEmpty a)) ==>
    lazy (pointWiseEqual (QuadRope.hreduce (+) 0 (QuadRope.vreduce (+) 0 a))
                         (QuadRope.vreduce (+) 0 (QuadRope.hreduce (+) 0 a)))

let ``reduce equals hreduce + vreduce`` (a : int QuadRope) =
    (not (QuadRope.isEmpty a)) ==>
      lazy (QuadRope.reduce (+) 0 a = QuadRope.get (QuadRope.hreduce (+) 0 (QuadRope.vreduce (+) 0 a)) 0 0)

let ``map + reduce equals mapreduce`` (a : int QuadRope) =
    (not (QuadRope.isEmpty a)) ==>
     lazy (QuadRope.mapreduce (fun x -> x * x) (+) 0 a = QuadRope.reduce (+) 0 (QuadRope.map (fun x -> x * x) a))

let ``hfilter removes elements correctly`` (a : int QuadRope) (Fun p) =
    QuadRope.rows a = 1 ==> lazy QuadRope.forall p (QuadRope.hfilter p a)

let ``vfilter removes elements correctly`` (a : int QuadRope) (Fun p) =
    QuadRope.cols a = 1 ==> lazy QuadRope.forall p (QuadRope.vfilter p a)

let ``last of hscan equals hreduce`` (a : int QuadRope) =
    let b = QuadRope.hscan (+) (fun _ -> 0) a
    let c = QuadRope.hreduce (+) 0 a
    QuadRope.equals (QuadRope.col b (QuadRope.cols b - 1)) c

let ``last of vscan equals vreduce`` (a : int QuadRope) =
    let b = QuadRope.vscan (+) (fun _ -> 0) a
    let c = QuadRope.vreduce (+) 0 a
    QuadRope.equals (QuadRope.row b (QuadRope.rows b - 1)) c

let ``hscan's elements are strictly ordered`` (a : int QuadRope) =
    let b = QuadRope.map List.singleton a
    let c = QuadRope.hscan (@) (fun _ -> []) b
    c |> QuadRope.map (List.length) |> QuadRope.forallRows (<)

let ``vscan's elements are strictly ordered`` (a : int QuadRope) =
    let b = QuadRope.map List.singleton a
    let c = QuadRope.vscan (@) (fun _ -> []) b
    c |> QuadRope.map (List.length) |> QuadRope.forallCols (<)

let ``transpose of transpose is identity`` (a : int QuadRope) =
    QuadRope.equals (QuadRope.transpose (QuadRope.transpose a)) a

let ``zip ignores internal structure`` (a : int QuadRope) (b : int QuadRope) =
    (QuadRope.rows a = QuadRope.rows b && QuadRope.cols a = QuadRope.cols b) ==>
    lazy (QuadRope.equals (QuadRope.zip (+) a b)
                          (QuadRope.init (rows a) (cols b) (fun r c -> QuadRope.get a r c + QuadRope.get b r c)))
let ``transpose equals switching indices`` (a : int QuadRope) =
    let b = QuadRope.init (QuadRope.cols a) (QuadRope.rows a) (fun i j -> QuadRope.get a j i)
    QuadRope.equals (QuadRope.transpose a) b


let ``toArray -> fromArray produces equal rope`` (a : int QuadRope) =
    pointWiseEqual a (QuadRope.fromArray (QuadRope.toArray a) (QuadRope.cols a))

let ``toArray2D -> fromArray2D produces equal rope`` (a : int QuadRope) =
    pointWiseEqual a (QuadRope.fromArray2D (QuadRope.toArray2D a))
