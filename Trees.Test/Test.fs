namespace RadTrees.Test

module TestRunner =
    open FsCheck

    let test () =
        RadTrees.Test.Gen.register() |> ignore
        Check.QuickAll<QuadRopeTest.QuadRopeTest>()

    [<EntryPoint>]
    let main args =
        try
            test()
            0
        with
            | e -> printfn "%A" e; 1
