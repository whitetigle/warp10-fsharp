module Tests.Warp10.Samples

open Fable.Core
open Fable.PowerPack
open Warp10.Shared.Types
open Warp10.Shared.Script
open Util
open Util.TestsWithPromises
open Warp10.Shared.DateUtils

describe "Warp10.Samples" <| fun _ ->

    let test (expected:string) (script:string) =
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

    describe "Sensor" <| fun _ ->

        it "should get data for  sensor" <| fun _ ->
            let expected = """{"c":"myarduino.sensor4","l":{"id":"39",".app":"test","kind":"psi"},"a":{},"v":[[1534348800000000,99]]}"""
            let expected = sprintf "[[%s]]" expected
            let labels = None

            let script = ""
            (script,[])
                |> GTS.fetch readToken "~myarduino.*" ["id","39"] 1534348800000000. -1.
                |> GTS.filter labels (Filter.ByClass "~myarduino.sensor4")
                |> GTS.join
                |> test expected
