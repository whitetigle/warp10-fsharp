module Tests.Warp10.Client

open Fable.PowerPack
open Warp10.Shared.Types
open System
open Util
open Util.TestsWithPromises


describe "Warp10.Client" <| fun _ ->

    describe "exec" <| fun _ ->

        it "should exec" <| fun _ ->
            let expected = """[[{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,57],[449,34]]}]]"""

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

            Warp10.Client.exec (server, readToken, script )
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
                Warp10.Client.update (server, writeToken, request )
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
                Warp10.Client.update (server, writeToken, request )
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
                    Elevation=None
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 123
                }
                {
                    TimeStamp=Some now
                    Latitude=None
                    Longitude=None
                    ClassName="foo"
                    Elevation=None
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 124
                }
                {
                    TimeStamp=None
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Elevation=None
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 129
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Elevation=None
                    Labels=[("label0","val0");("label1","val1")]
                    Value=LONG 129
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Elevation=None
                    Labels=[("label0","val0");("label1","val1")]
                    Value=BOOL true
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    ClassName="foo"
                    Elevation=None
                    Labels=[("label0","val0");("label1","val1")]
                    Value=STRING "toto"
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    Elevation=None
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=DOUBLE 456.789
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    Elevation=Some 158
                    ClassName="foo"
                    Labels=[("label0","val0");("label1","val1")]
                    Value=DOUBLE 456.789
                }
                {
                    TimeStamp=Some now
                    Latitude=Some (48,0)
                    Longitude=Some (-4,5)
                    Elevation=Some 158
                    ClassName="foo{"
                    Labels=[("label0}","val0,");("label1=","val1")]
                    Value=DOUBLE 456.789
                }
            ]
            |> List.map testUpdate
            |> Promise.Parallel

    describe "delete" <| fun _ ->

        let prepareData() =
            let insert request =
                Warp10.Client.update (server, writeToken, request )

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
                "1444000000000000//45.89 titi{a=42,b=42} 42"
            ]
            |> List.map UpdateRequest.fromString
            |> List.map insert
            |> Promise.Parallel

        let checkDelete request =
            let expected = 200
            Warp10.Client.delete (server, writeToken, request )
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

        (* // on latest versions it does not seem to work anymore
        it "should delete" <| fun _ ->

            prepareData()
            |> Promise.map( fun _ ->
                let request = From("~t.t.{b=4}", DateTime(2015,9,2,12,0,0,0))
                checkDelete request
                )
        *)

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
                Warp10.Client.update (server, writeToken, request )

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
            Warp10.Client.fetch (server, readToken, request )
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
