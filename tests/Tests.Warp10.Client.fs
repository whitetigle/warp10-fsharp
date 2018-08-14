module Tests.Warp10.Client

open Fable.Core
open Fable.Import
open Fable.PowerPack
open Fable.Core.Testing
open Warp10.Shared.Types
open System

let inline equal (expected: 'T) (actual: 'T): unit =
  Testing.Assert.AreEqual(expected, actual)

[<Global>]
let describe (msg: string) (f: unit->unit): unit = jsNative

[<Global>]
let it (msg: string) (f: unit->JS.Promise<'T>): unit = jsNative

// exec polyfill for node
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


describe "Warp10.Client" <| fun _ ->

    describe "exec" <| fun _ ->

        it "should exec" <| fun _ ->
            let expected = """[[{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,57],[449,34]]}]]"""

            Warp10.Client.exec (endpoint, readToken, script )
            |> Promise.map ( fun res ->
                let result =
                    match res with
                    | Ok result ->
                       result
                    | Error _ ->
                        "failed"
                equal expected result
            )

    describe "update" <| fun _ ->

        it "should update from string list" <| fun _ ->
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
                "1440000000000000// toto{a=42,b=42} 42"
                "1440000000000000// titi{a=42,b=42} 42"
                "1441000000000000// toto{a=4,b=4} 4"
                "1441000000000000// titi{a=4,b=4} 4"
                "1442000000000000// toto{a=42,b=42} 42"
                "1442000000000000// titi{a=42,b=42} 42"
                "1443000000000000// toto{a=4,b=4} 4"
                "1443000000000000// titi{a=4,b=4} 4"
                "1444000000000000// toto{a=42,b=42} 42"
                "1444000000000000// titi{a=42,b=42} 42"
            ]
            |> List.map UpdateRequest.fromString
            |> List.map testUpdate
            |> Promise.Parallel

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

            let now = (double (DateTimeOffset(DateTime.Now).ToUnixTimeSeconds()))
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
                    TimeStamp=Some now
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
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 129
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=BOOL true
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=STRING "toto"
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=DOUBLE 456.789
                }
            ]
            |> List.map testUpdate
            |> Promise.Parallel

    describe "delete" <| fun _ ->

        let prepareData() =
            let insert request =
                Warp10.Client.update (endpoint, writeToken, request )

            [
                "1440000000000000// toto{a=42,b=42} 42"
                "1440000000000000// titi{a=42,b=42} 42"
                "1441000000000000// toto{a=4,b=4} 4"
                "1441000000000000// titi{a=4,b=4} 4"
                "1442000000000000// toto{a=42,b=42} 42"
                "1442000000000000// titi{a=42,b=42} 42"
                "1443000000000000// toto{a=4,b=4} 4"
                "1443000000000000// titi{a=4,b=4} 4"
                "1444000000000000// toto{a=42,b=42} 42"
                "1444000000000000// titi{a=42,b=42} 42"
            ]
            |> List.map UpdateRequest.fromString
            |> List.map insert
            |> Promise.Parallel

        let checkDelete request =
            let expected = 200
            Warp10.Client.delete (endpoint, writeToken, request )
            |> Promise.map ( fun res ->
                let result =
                    match res with
                    | Ok code -> code
                    | Error code -> code
                equal expected result
            )

        it "should delete" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request = FullRange "toto{a=42,b=42}"
                checkDelete request
                )

        it "should delete" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request = FullRange "~t.t.{b=4}"
                checkDelete request
                )

        it "should delete" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request = Partial("~t.t.{b=4}", (Timestamp (1440000000000000.,1444000000000000. )))
                checkDelete request
                )

        it "should delete" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request = FullRange "~.*{a=42}"
                checkDelete request
                )

        it "should delete" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request = Partial("~.*{a=42}", (Timestamp (1440000000000000.,1444000000000000. )))
                checkDelete request
                )

        it "should delete" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request = Partial("toto{}", (Timestamp (1440000000000000.,1444000000000000. )))
                checkDelete request
                )

        it "should delete" <| fun _ ->

            let start = DateTime(2015,8,19,16,0,0,0)
            let stop= DateTime(2015,10,04,23,6,40,0)

            prepareData()
            |> Promise.map( fun _ ->
                let request = Partial("toto{}", (ISO8601 (start,stop)))
                checkDelete request
                )

    describe "fetch" <| fun _ ->

        let prepareData() =
            let insert request =
                Warp10.Client.update (endpoint, writeToken, request )

            [
                "1440000000000000// toto{a=42,b=42} 42"
                "1440000000000000// titi{a=42,b=42} 42"
                "1441000000000000// toto{a=4,b=4} 4"
                "1441000000000000// titi{a=4,b=4} 4"
                "1442000000000000// toto{a=42,b=42} 42"
                "1442000000000000// titi{a=42,b=42} 42"
                "1443000000000000// toto{a=4,b=4} 4"
                "1443000000000000// titi{a=4,b=4} 4"
                "1444000000000000// toto{a=42,b=42} 42"
                "1444000000000000// titi{a=42,b=42} 42"
            ]
            |> List.map UpdateRequest.fromString
            |> List.map insert
            |> Promise.Parallel

        let fetchData request =
            let expected = true
            Warp10.Client.fetch (endpoint, readToken, request )
            |> Promise.map ( fun res ->
                let result =
                    match res with
                    | Ok _ -> true
                    | Error _ -> false
                equal expected result
            )

        it "should fetch" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request =
                    {
                        Selector="toto{a=42,b=42}"
                        Format = FullText
                        Dedup=true
                        Interval = LastReadings (1440000000000000. , 5)
                    }
                fetchData request
                )

        it "should fetch" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request =
                    {
                        Selector="toto{a=42,b=42}"
                        Format = FullText
                        Dedup=true
                        Interval = Timespan (1440000000000000. , 1444000000000000.)
                    }
                fetchData request
                )

        it "should fetch" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let start = DateTime(2015,8,19,16,0,0,0) //1440000000000000
                let stop = DateTime(2015,10,04,23,6,40,0) //1444000000000000
                let request =
                    {
                        Selector="toto{a=42,b=42}"
                        Format = FullText
                        Dedup=true
                        Interval = StartAndStop (start,stop)
                    }
                fetchData request
                )
