module Tests.Warp10.Samples

open Fable.Core
open Fable.PowerPack
open Warp10.Shared.Types
open Warp10.Shared.Script
open Util
open Util.TestsWithPromises
open Warp10.Shared.DateUtils

(*
[
    'sMewcQeIFrr2v2JTCHCGTqMRDr2Ubb7lxlUFZ6Ft6isAJQxZIQUwA0I038J7X9uBy6b6akOlRQYeW1N_35cMusYJMB7NuKP79sz.kK5PwxBkQDtHBT4Ls6oODTe19DerIH047n7weB_rDxGcdHIv3L.Tg8GJLMcC'
    'speed'
    {
    }
    NOW -1000000000000
]
FETCH
[ SWAP bucketizer.count 0 300000000 0 ] BUCKETIZE
<%
  DROP DUP TICKS <% DROP 'UTC' ISO8601 %> LMAP SWAP VALUES 2 ->LIST ZIP
%> LMAP
*)

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

        it "should get last value for card #39 and sensor #4" <| fun _ ->
            let expected = """{"c":"myarduino.sensor4","l":{"id":"39",".app":"test","kind":"psi"},"a":{},"v":[[1534348800000000,99]]}"""
            let expected = sprintf "[[%s]]" expected
            let labels = None

            let script = ""
            (script,[])
                |> GTS.fetch readToken "~myarduino.*" ["id","39"] 1534348800000000. -1.
                |> GTS.filter labels (Filter.ByClass "~myarduino.sensor4")
                |> GTS.join
                |> test expected

        it "should get max temperature for all temp sensors on card #1" <| fun _ ->
            let expected = """{"c":"myarduino.sensor2","l":{"id":"1",".app":"test","kind":"temp"},"a":{},"v":[[1534348800000000,99]]},{"c":"myarduino.sensor1","l":{"id":"1",".app":"test","kind":"temp"},"a":{},"v":[[1534348800000000,99]]}"""
            let expected = sprintf "[[%s]]" expected
            let labels = None

            let script = ""
            (script,[])
                |> GTS.fetch readToken "~myarduino.*" ["kind","temp";"id","1"] 1534348800000000. -10000000000.
                |> GTS.bucketize Bucketizer.Max 0 0 1
                |> GTS.join
                |> test expected

        it "should get max temperature for temp sensor #2 on card #1" <| fun _ ->
            let expected = """{"c":"myarduino.sensor2","l":{"id":"1",".app":"test","kind":"temp"},"a":{},"v":[[1534348800000000,99]]}"""
            let expected = sprintf "[[%s]]" expected
            let labels = None

            let script = ""
            (script,[])
                |> GTS.fetch readToken "myarduino.sensor2" ["kind","temp";"id","1"] 1534348800000000. -10000000000.
                |> GTS.bucketize Bucketizer.Max 0 0 1
                |> GTS.join
                |> test expected

        it "should get max temperature for temp sensor #2 on card #1 for the last hour" <| fun _ ->
            let expected = """{"c":"myarduino.sensor2","l":{"id":"1",".app":"test","kind":"temp"},"a":{},"v":[[1534348800000000,99]]}"""
            let expected = sprintf "[[%s]]" expected
            let labels = None

            let script = ""
            (script,[])
                |> GTS.fetch readToken "myarduino.sensor2" ["kind","temp";"id","1"] 1534348800000000. 36000000000.
                |> GTS.bucketize Bucketizer.Max 0 0 1
                |> GTS.join
                |> test expected
