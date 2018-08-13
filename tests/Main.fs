module Tests.Main

open Fable.Core
open Fable.Import
open Fable.PowerPack
open Fable.Core.Testing
open Warp10.Shared.Types
open System

let inline equal (expected: 'T) (actual: 'T): unit =
  Testing.Assert.AreEqual(expected, actual)

[<Global>]
let it (msg: string) (f: unit->JS.Promise<'T>): unit = jsNative

[<Global>]
let describe (msg: string) (f: unit->unit): unit = jsNative

// Fetch polyfill for node
JsInterop.importAll "isomorphic-fetch"

// assume warp10 is working locally and tokens have been set
// change this token to yours
let readToken : Token = Read "4l_tJMgbsVp8Tydi5Msxb3GqEAudZTyDHXTCYZADwZ_RzPVhHB17aoHukmqHUYUdxFQoTZw5Y7POdldPrWpUXZsYNUU8Nt8l1Lky2KeubuY9Z3w5DCzST."

// change this token to yours
let writeToken : Token = Write "UCuQmMpw7qZOsmzVX3jmboLOtcE20DFoCFR2ty88c9ulPtes3o4giPUsHZuLerFdHWsxOeRyHW6XhiEe1jUxzokuFg28fXyQzdLJzTAmAvc"

// change this host and port to yours
let endpoint = { Url="192.168.99.100:8080"; Protocol=HTTP}

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


describe "Warp10" <| fun _ ->

    describe "Client" <| fun _ ->

        it "should fetch" <| fun _ ->
            let expected = """[[{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,57],[449,34]]}]]"""

            Warp10.Client.fetch (endpoint, readToken, script )
            |> Promise.map ( fun res ->
                let result =
                    match res with
                    | Ok result ->
                       result
                    | Error _ ->
                        "failed"
                equal expected result
            )

        it "should update" <| fun _ ->
            let expected = 200

            let testUpdate request =
                Warp10.Client.update (endpoint, writeToken, request )
                |> Promise.map ( fun res ->
                    let result =
                        match res with
                        | Ok code -> code
                        | Error code -> code
                    equal expected result
                )

            [
                {
                    TimeStamp=None
                    Latitude=None
                    Longitude=None
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 123
                }
                {
                    TimeStamp=Some (DateTimeOffset(DateTime.Now).ToUnixTimeSeconds())
                    Latitude=None
                    Longitude=None
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 124
                }
                {
                    TimeStamp=None
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 129
                }
                {
                    TimeStamp=Some (DateTimeOffset(DateTime.Now).ToUnixTimeSeconds())
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 129
                }
                {
                    TimeStamp=Some (DateTimeOffset(DateTime.Now).ToUnixTimeSeconds())
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=BOOL true
                }
                {
                    TimeStamp=Some (DateTimeOffset(DateTime.Now).ToUnixTimeSeconds())
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=STRING "toto"
                }
                {
                    TimeStamp=Some (DateTimeOffset(DateTime.Now).ToUnixTimeSeconds())
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=DOUBLE 456.789
                }
            ]
            |> List.map testUpdate
            |> Promise.Parallel


