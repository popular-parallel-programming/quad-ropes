namespace QuadTree

type 'a QuadRope =
    | Empty
    | Leaf of 'a [,]
    | Node of 'a QuadRope * 'a QuadRope * 'a QuadRope * 'a QuadRope
