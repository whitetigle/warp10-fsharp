module Tests.Warp10.Script

open Fable.Core
open Fable.Import
open Fable.PowerPack
open Fable.Core.Testing
open Warp10.Shared.Types
open Warp10.Shared.Script
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
let readToken : Token = Read "7GJWBE3sW4PKDrgmRH1NP8y02DhMyQZtnn1chiuJGIxqF0Hi9USmcujWm.qj_4eN3YFIyZ551ofyRFygmOYEkVaSf_hlc.4ScQ1aQhCvd6PrbuQ8Q7.MC."

// change this token to yours
let writeToken : Token = Write "GgBpQQSFcQ82WSOhaG7toJDAJ1EQhoVQz8O11LO_KVJRGjqPBc2r3r5WtwMne2gXj1Wj.vWkp3fUSx5GF6Piy4UDuPh5oxaV0V.LyX96Gog"

// change this host and port to yours
let server = { Host="192.168.99.100"; Port=Some 8080; ApiVersion=V0; Protocol=HTTP}

describe "Warp10.Script" <| fun _ ->

    let test expected script =
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

    describe "Bucketize" <| fun _ ->

        let gts =
            GTS.create
                "testname"
                (Some ["label0","42";"label1","foo"])
                [
                    UserDefined 100,None,None,None,LONG 10
                    UserDefined 200,None,None,None,LONG 9
                    UserDefined 300,None,None,None,LONG 8
                    UserDefined 400,None,None,None,LONG 7
                    UserDefined 500,None,None,None,LONG 6
                    UserDefined 600,None,None,None,LONG 5
                    UserDefined 700,None,None,None,LONG 4
                    UserDefined 800,None,None,None,LONG 42
                ]

        it "should bucketize.sum" <| fun _ ->
            let expected = """[[{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,57],[449,34]]}]]"""
            gts
                |> GTS.bucketize Bucketizer.Sum 0 0 2
                |> test expected

        it "should bucketize.max" <| fun _ ->
            let expected = """[[{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,42],[449,10]]}]]"""
            gts
                |> GTS.bucketize Bucketizer.Max 0 0 2
                |> test expected

        it "should bucketize.min" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,4],[449,7]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketize Bucketizer.Min 0 0 2
                |> test expected

        it "should bucketize.mean" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,14.25],[449,8.5]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketize Bucketizer.Mean 0 0 2
                |> test expected

        it "should bucketize.mean.circular" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,14.003215405992911],[449,8.499999999999996]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketizeWithParams (Bucketizer.MeanCircularNoNull 360.) 0 0 2
                |> test expected

        it "should bucketize.mean.circular.exclude-nulls" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,14.003215405992911],[449,8.499999999999996]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketizeWithParams (Bucketizer.MeanCircularNoNull 360.) 0 0 2
                |> test expected

        it "should bucketize.first" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,6],[449,10]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketize Bucketizer.First 0 0 2
                |> test expected

        it "should bucketize.last" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,42],[449,7]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketize Bucketizer.Last 0 0 2
                |> test expected

        it "should bucketize.join" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,"6 & 5 & 4 & 42"],[449,"10 & 9 & 8 & 7"]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketizeWithParams (Bucketizer.Join "&") 0 0 2
                |> test expected

        it "should bucketize.median" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,5],[449,8]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketize Bucketizer.Median 0 0 2
                |> test expected

        it "should bucketize.count" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,4],[449,4]]}"""
            let expected = sprintf "[[%s]]" result
            gts
                |> GTS.bucketize Bucketizer.Count 0 0 2
                |> test expected

        describe "Bucketize And/Or" <| fun _ ->

            let gts =
                GTS.create
                    "testname"
                    (Some ["label0","42";"label1","foo"])
                    [
                        UserDefined 100,None,None,None,BOOL true
                        UserDefined 200,None,None,None,BOOL false
                        UserDefined 300,None,None,None,BOOL true
                        UserDefined 400,None,None,None,BOOL true
                        UserDefined 500,None,None,None,BOOL true
                        UserDefined 600,None,None,None,BOOL true
                        UserDefined 700,None,None,None,BOOL true
                        UserDefined 800,None,None,None,BOOL true
                    ]

            let gts2 =
                GTS.create
                    "testname"
                    (Some ["label0","42";"label1","foo"])
                    [
                        UserDefined 100,None,None,None,BOOL true
                        UserDefined 200,None,None,None,BOOL true
                        UserDefined 300,None,None,None,BOOL true
                        UserDefined 400,None,None,None,BOOL true
                        UserDefined 500,None,None,None,BOOL true
                        UserDefined 600,None,None,None,BOOL true
                        UserDefined 700,None,None,None,BOOL true
                        UserDefined 2000,None,None,None,BOOL false
                    ]

            it "should bucketize.and" <| fun _ ->
                let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[1049,true],[732,true],[415,false]]},{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[2000,false],[732,true],[415,true]]}"""
                let expected = sprintf "[[%s]]" result

                gts @ gts2
                    |> GTS.bucketize Bucketizer.And 2000 0 6
                    |> test expected

            it "should bucketize.or" <| fun _ ->
                let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[1049,true],[732,true],[415,true]]},{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[2000,false],[732,true],[415,true]]}"""
                let expected = sprintf "[[%s]]" result

                gts @ gts2
                    |> GTS.bucketize Bucketizer.Or 2000 0 6
                    |> test expected

