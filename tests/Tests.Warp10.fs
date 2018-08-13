module Tests.Warp10

open Util.Testing

open Fable.Core
open Fable.Import
open Fable.PowerPack

//#if FABLE_COMPILER
open Warp10.Client

open Fable.Core.Testing

let testList (name : string) (tests: seq<'b>) = name, tests
let testCase (msg : string) (test : obj -> unit) = msg, test

let equal expected actual: unit =
    Assert.AreEqual(expected, actual)

type Test = string * seq<string * seq<string * (obj -> unit)>>

// Fetch polyfill for node
JsInterop.importAll "isomorphic-fetch"

// assume warp10 is working locally and tokens have been set
// change this token to yours
let readToken = "4l_tJMgbsVp8Tydi5Msxb3GqEAudZTyDHXTCYZADwZ_RzPVhHB17aoHukmqHUYUdxFQoTZw5Y7POdldPrWpUXZsYNUU8Nt8l1Lky2KeubuY9Z3w5DCzST."

// change this token to yours
let writeToken = "UCuQmMpw7qZOsmzVX3jmboLOtcE20DFoCFR2ty88c9ulPtes3o4giPUsHZuLerFdHWsxOeRyHW6XhiEe1jUxzokuFg28fXyQzdLJzTAmAvc"

// change this host and port to yours
let endpoint = { Url="192.168.99.100:8081"; Protocol=HTTP}

let script = """NEWGTS
'testname'
RENAME
{ 'label0' '42' 'label1' 'foo' }
RELABEL
100  NaN NaN NaN 10 ADDVALUE
200  NaN NaN NaN  9 ADDVALUE
300  NaN NaN NaN  8 ADDVALUE
400  NaN NaN NaN  7 ADDVALUE
500  NaN NaN NaN  6 ADDVALUE
600  NaN NaN NaN  5 ADDVALUE
700  NaN NaN NaN  4 ADDVALUE
800  NaN NaN NaN 42 ADDVALUE
[ SWAP bucketizer.sum 0 0 2 ] BUCKETIZE"""


let tests : Test =
    testList "Warp10.Client" [

        testList "Basic" [

            testCase "a string works" <| fun _ ->
                let expected = "francois"
                let actual = "francois"
                equal expected actual

            testCase "fetch" <| fun _ ->
                let expected = """[{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,57],[449,34]]}]"""

                Warp10.Client.fetch (endpoint, readToken, script )
                |> Promise.map ( fun res ->
                    let result =
                        match res with
                        | Ok result ->
                           printfn "%s" result
                           result
                        | Error err ->
                            printfn "%i" err
                            "failed"
                    equal expected result
                ) |> ignore
        ]

    ]

(*
#else
open Warp10.Net.Client

let tests : Test =
    testList "Warp10.Client" [

        testList "Basic" [

            testCase "a string works" <| fun _ ->
                let expected = "francois"
                let actual = "francois"
                equal expected actual
        ]
    ]

#endif

*)
