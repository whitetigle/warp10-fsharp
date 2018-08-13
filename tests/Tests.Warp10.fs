module Tests.Warp10

#if FABLE_COMPILER
open Warp10.Client
#else
open Warp10.Net.Client
#endif
open Util.Testing

let tests : Test =
    testList "Warp10.Client" [

        testList "Basic" [

            testCase "a string works" <| fun _ ->
                let expected = "francois"
                let actual = "francois"
                equal expected actual
        ]

    ]
