module RadTrees.Test.Utils

open FsCheck
open RadTrees
open Types
open Utils

type Handle = class end

let ``nth fibonacci value is >= than argument`` (NonNegativeInt n) =
    n <= Fibonacci.fib (Fibonacci.nth n)
