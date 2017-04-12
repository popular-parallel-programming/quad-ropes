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

module QuadRope.Test.QuadRope

open FsCheck
open QuadRope
open QuadRope.Types

module Utils =

    let makeIndices h w =
        seq { for i in 0..h - 1 do
              for j in 0..w - 1 ->
              i, j }


    let makeIndicesFrom qr =
        makeIndices (QuadRope.rows qr) (QuadRope.cols qr)


    let access rope (i, j) =
        try
            ignore (QuadRope.get rope i j)
            true
        with
            | _ -> false


    let pointWiseEqualMap a b f g=
        let mutable eq = true
        QuadRope.iteri (fun i j v -> eq <- eq && (f v = g (QuadRope.get b i j))) a
        eq


    let pointWiseEqual a b =
        pointWiseEqualMap a b id id


open Utils

type Handle = class end


let ``DEBUG leaf sizes enabled`` () =
    QuadRope.smax = 4


let ``let q = init h w _ ==> rows q = h`` (NonNegativeInt h) (NonNegativeInt w) =
    (0 < h && 0 < w) ==>
    lazy (let qr = QuadRope.init h w (*)
          QuadRope.rows qr = h &&
          QuadRope.get (QuadRope.vmapreduce (fun _ -> 1) (+) 0 qr) 0 0 = h)


let ``let q = init h w _ ==> cols q = w`` (NonNegativeInt h) (NonNegativeInt w) =
    (0 < h && 0 < w) ==>
    lazy (let qr = QuadRope.init h w (*)
          QuadRope.cols qr = w &&
          QuadRope.get (QuadRope.hmapreduce (fun _ -> 1) (+) 0 qr) 0 0 = w)


let ``let q = init _ _ f ==> get q i j = f i j`` (NonNegativeInt h) (NonNegativeInt w) =
    (0 < h && 0 < w) ==>
    lazy (let qr = QuadRope.init h w (*)
          Seq.forall (fun (i, j) -> QuadRope.get qr i j = i * j) (makeIndices h w))


let ``get is always inside bounds`` (a : int64 QuadRope) =
    Seq.forall (access a) (makeIndicesFrom a)


let ``let r = set q i j x ==> get r i j = x`` (a : int64 QuadRope) x =
    Seq.forall (fun (i, j) -> let a' = QuadRope.set a i j x in QuadRope.get a' i j = x)
               (makeIndicesFrom a)


let ``cols (hcat a b) = cols a + cols b`` (a : int64 QuadRope) (b : int64 QuadRope)  =
    (QuadRope.rows a = QuadRope.rows b) ==>
    lazy (QuadRope.cols a + QuadRope.cols b = QuadRope.cols (QuadRope.hcat a b))


let ``rows (vcat a b) = rows a + rows b`` (a : int64 QuadRope) (b : int64 QuadRope)  =
    (QuadRope.cols a = QuadRope.cols b) ==>
    lazy (QuadRope.rows a + QuadRope.rows b = QuadRope.rows (QuadRope.vcat a b))


let ``get (hbalance q) i j = get q i j`` (NonNegativeInt x) =
    let mutable r = QuadRope.singleton 0
    for i in 1 .. x * 10 do
        r <- QuadRope.hcat r (QuadRope.singleton i)
    QuadRope.forallRows (<) r


let ``get (vbalance q) i j = get q i j`` (NonNegativeInt x) =
    let mutable r = QuadRope.singleton 0
    for i in 1 .. x * 10 do
        r <- QuadRope.vcat r (QuadRope.singleton i)
    QuadRope.forallCols (<) r


let ``hrev (hrev q) = q`` (a : int64 QuadRope) =
    pointWiseEqual a (QuadRope.hrev (QuadRope.hrev a))


let ``vrev (vrev q) = q`` (a : int64 QuadRope) =
    pointWiseEqual a (QuadRope.vrev (QuadRope.vrev a))


let ``get i j (hrev q) = get i (cols q - 1 - j) q`` (a: int64 QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
    let h = QuadRope.rows a
    let w = QuadRope.cols a
    (i < h && j < w) ==>
    lazy (QuadRope.get (QuadRope.hrev a) i j = QuadRope.get a i ((w - 1) - j))


let ``get i j (vrev q) = get (rows q - 1 - i) j q`` (a: int64 QuadRope) (NonNegativeInt i) (NonNegativeInt j) =
    let h = QuadRope.rows a
    let w = QuadRope.cols a
    (i < h && j < w) ==>
    lazy (QuadRope.get (QuadRope.vrev a) i j = QuadRope.get a ((h - 1) - i) j)


let ``get accesses hcat correctly`` (a : int64 QuadRope) (b : int64 QuadRope) =
    (QuadRope.rows a = QuadRope.rows b) ==>
    lazy (let ab = QuadRope.hcat a b
          Seq.forall (access ab) (makeIndicesFrom ab))


let ``get accesses vcat correctly`` (a : int64 QuadRope) (b : int64 QuadRope) =
    (QuadRope.cols a = QuadRope.cols b) ==>
    lazy (let ab = QuadRope.vcat a b
          Seq.forall (access ab) (makeIndicesFrom ab))


let ``get accesses slice correctly`` (a : int64 QuadRope) (NonNegativeInt i)
                                                          (NonNegativeInt j)
                                                          (NonNegativeInt h)
                                                          (NonNegativeInt w) =
    (i < QuadRope.rows a && i < h && j < QuadRope.cols a && j < w) ==>
    lazy (let b = QuadRope.slice i j h w a
          Seq.forall
            (fun (i0, j0) -> QuadRope.get b i0 j0 = QuadRope.get a (i + i0) (j + j0))
            (makeIndicesFrom b))


let ``slice is equal to materialized slice`` (a : int64 QuadRope) (NonNegativeInt i)
                                                                  (NonNegativeInt j)
                                                                  (NonNegativeInt h)
                                                                  (NonNegativeInt w) =
    (i < h && j < w) ==> lazy (
        let b = QuadRope.slice i j h w a
        let c = QuadRope.materialize b
        QuadRope.rows b = QuadRope.rows c && QuadRope.cols b = QuadRope.cols c
        && pointWiseEqual c b)


let ``recursive slice computes correct indices`` (a : int64 QuadRope) (d : NonNegativeInt) =
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


let ``let r = hslice 0 w q => cols r = w`` (a : int64 QuadRope) (NonNegativeInt w) =
    w <= QuadRope.cols a ==>
    lazy (w = QuadRope.cols (QuadRope.slice 0 0 (QuadRope.rows a) w a))


let ``let r, s = hsplit2 q w ==> cols r = w /\ cols s = cols q = w`` (a : int64 QuadRope) (NonNegativeInt w) =
    w <= QuadRope.cols a ==>
    lazy (let b, c = QuadRope.hsplit2 a w
          QuadRope.cols b = w && QuadRope.cols c = QuadRope.cols a - w)


let ``let r = vslice 0 h q => rows r = h`` (a : int64 QuadRope) (NonNegativeInt h) =
    h <= QuadRope.rows a ==>
    lazy (h = QuadRope.rows (QuadRope.slice 0 0 h (QuadRope.cols a) a))


let ``let r, s = vsplit2 q h ==> rows r = h /\ rows s = rows q = h`` (a : int64 QuadRope) (NonNegativeInt h) =
    h <= QuadRope.rows a ==>
    lazy (let b, c = QuadRope.vsplit2 a h
          (QuadRope.rows b = h) && (QuadRope.rows c = QuadRope.rows a - h))


let ``let r = map id q ==> rows r = rows q /\ cols r = cols q`` (a : int64 QuadRope) =
    let r = QuadRope.map id a
    QuadRope.rows r = QuadRope.rows a && QuadRope.cols r = QuadRope.cols a


let ``map id q = q`` (a : int64 QuadRope) =
    pointWiseEqual a (QuadRope.map id a)


let ``let r = map f q ==> get i j r = f (get i j q)`` (a : int64 QuadRope) (f : int64 -> int64) =
    pointWiseEqualMap a (QuadRope.map f a) f id


let ``let r = hreduce f e q ==> cols r = 1`` (a : int64 QuadRope) =
    (not (QuadRope.isEmpty a)) ==> lazy (QuadRope.cols (QuadRope.hreduce (+) 0L a) = 1)


let ``let r = vreduce f e q ==> rows r = 1`` (a : int64 QuadRope) =
    (not (QuadRope.isEmpty a)) ==> lazy (QuadRope.rows (QuadRope.vreduce (+) 0L a) = 1)


let ``hreduce >> vreduce = vreduce >> hreduce`` (a : int64 QuadRope) =
    (not (QuadRope.isEmpty a)) ==>
    lazy (pointWiseEqual (a |> QuadRope.hreduce (+) 0L |> QuadRope.vreduce (+) 0L)
                         (a |> QuadRope.vreduce (+) 0L |> QuadRope.hreduce (+) 0L))


let ``reduce = hreduce >> vreduce`` (a : int64 QuadRope) =
    (not (QuadRope.isEmpty a)) ==>
      lazy (QuadRope.reduce (+) 0L a = QuadRope.get (a |> QuadRope.hreduce (+) 0L |> QuadRope.vreduce (+) 0L ) 0 0)

let ``reduce = vreduce >> hreduce`` (a : int64 QuadRope) =
    (not (QuadRope.isEmpty a)) ==>
      lazy (QuadRope.reduce (+) 0L a = QuadRope.get (a |> QuadRope.vreduce (+) 0L |> QuadRope.hreduce (+) 0L ) 0 0)


let ``map >> reduce = mapreduce`` (a : int64 QuadRope) =
    (not (QuadRope.isEmpty a)) ==>
     lazy (QuadRope.mapreduce (fun x -> x * x) (+) 0L a = QuadRope.reduce (+) 0L (QuadRope.map (fun x -> x * x) a))


let ``last of hscan = hreduce`` (a : int64 QuadRope) =
    let b = QuadRope.hscan (+) 0L a
    let c = QuadRope.hreduce (+) 0L a
    QuadRope.equals (QuadRope.col b (QuadRope.cols b - 1)) c


let ``last of vscan = vreduce`` (a : int64 QuadRope) =
    let b = QuadRope.vscan (+) 0L a
    let c = QuadRope.vreduce (+) 0L a
    QuadRope.equals (QuadRope.row b (QuadRope.rows b - 1)) c


let ``hscan's elements are strictly ordered`` (a : int64 QuadRope) =
    let b = QuadRope.map List.singleton a
    let c = QuadRope.hscan (@) [] b
    c |> QuadRope.map (List.length) |> QuadRope.forallRows (<)


let ``vscan's elements are strictly ordered`` (a : int64 QuadRope) =
    let b = QuadRope.map List.singleton a
    let c = QuadRope.vscan (@) [] b
    c |> QuadRope.map (List.length) |> QuadRope.forallCols (<)


let ``transpose (transpose q) = q`` (a : int64 QuadRope) =
    QuadRope.equals (QuadRope.transpose (QuadRope.transpose a)) a


let ``let s = zip f q r ==> get s i j = f (get q i j) (get r i j)`` (a : int64 QuadRope)
                                                                    (b : int64 QuadRope) =
    (QuadRope.rows a = QuadRope.rows b && QuadRope.cols a = QuadRope.cols b) ==>
    lazy (QuadRope.equals (QuadRope.zip (+) a b)
                          (QuadRope.init (QuadRope.rows a) (QuadRope.cols b) (fun r c -> QuadRope.get a r c + QuadRope.get b r c)))


let ``fromArray (toArray q) = q`` (a : int64 QuadRope) =
    pointWiseEqual a (QuadRope.fromArray (QuadRope.toArray a) (QuadRope.cols a))


let ``fromArray2D (toArray2D q) = q`` (a : int64 QuadRope) =
    pointWiseEqual a (QuadRope.fromArray2D (QuadRope.toArray2D a))
