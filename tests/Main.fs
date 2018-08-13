module Tests.Main

#if FABLE_COMPILER

open Fable.Core

let [<Global>] describe (name: string) (f: unit->unit) = jsNative
let [<Global>] it (msg: string) (f: unit->unit) = jsNative

let run () =
    let tests = [ Tests.Warp10.tests
                ] :> Util.Testing.Test seq

    for (moduleName, moduleTests) in tests do
        describe moduleName <| fun () ->
            for (name, tests) in moduleTests do
                describe name <| fun _ ->
                    for (msg, test) in tests do
                        it msg test

run()

#else

open Expecto
open Util.Testing

[<EntryPoint>]
let main args =
    testList "All" [ Tests.Warp10.tests
                   ]
    |> runTestsWithArgs defaultConfig args

#endif
