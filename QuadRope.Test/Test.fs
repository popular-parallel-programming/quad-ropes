namespace RadTrees.Test

module TestRunner =
    open FsCheck

    let test () =
        RadTrees.Test.Gen.register() |> ignore
        Check.QuickAll (typeof<QuadRopeTest.Handle>.DeclaringType)

    [<EntryPoint>]
    let main _ =
        try
            test()
            0
        with
            | e -> printfn "%A" e; 1
