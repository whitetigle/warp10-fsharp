module Tests.Warp10.Script

open Fable.PowerPack
open Warp10.Shared.Types
open Warp10.Shared.Script
open Util
open Util.TestsWithPromises

describe "Warp10.Script" <| fun _ ->

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

            let script = ""
            (script,gts)
                |> GTS.bucketize Bucketizer.Sum 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.max" <| fun _ ->
            let expected = """[[{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,42],[449,10]]}]]"""
            let script = ""
            (script,gts)
                |> GTS.bucketize Bucketizer.Max 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.min" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,4],[449,7]]}"""
            let expected = sprintf "[[%s]]" result
            let script = ""
            (script,gts)
                |> GTS.bucketize Bucketizer.Min 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.mean" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,14.25],[449,8.5]]}"""
            let expected = sprintf "[[%s]]" result
            let script = ""
            (script,gts)
                |> GTS.bucketize Bucketizer.Mean 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.mean.circular" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,14.003215405992911],[449,8.499999999999996]]}"""
            let expected = sprintf "[[%s]]" result
            let script = ""
            (script,gts)
                |> GTS.bucketize (Bucketizer.MeanCircularNoNull 360.) 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.mean.circular.exclude-nulls" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,14.003215405992911],[449,8.499999999999996]]}"""
            let expected = sprintf "[[%s]]" result
            let script = ""
            (script,gts)
                |> GTS.bucketize (Bucketizer.MeanCircularNoNull 360.) 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.first" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,6],[449,10]]}"""
            let expected = sprintf "[[%s]]" result
            let script = ""
            (script,gts)
                |> GTS.bucketize Bucketizer.First 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.last" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,42],[449,7]]}"""
            let expected = sprintf "[[%s]]" result
            let script = ""
            (script,gts)
                |> GTS.bucketize Bucketizer.Last 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.join" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,"6 & 5 & 4 & 42"],[449,"10 & 9 & 8 & 7"]]}"""
            let expected = sprintf "[[%s]]" result

            let script = ""
            (script,gts)
                |> GTS.bucketize (Bucketizer.Join "&") 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.median" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,5],[449,8]]}"""
            let expected = sprintf "[[%s]]" result

            let script = ""
            (script,gts)
                |> GTS.bucketize Bucketizer.Median 0 0 2
                |> GTS.join
                |> test expected

        it "should bucketize.count" <| fun _ ->
            let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[800,4],[449,4]]}"""
            let expected = sprintf "[[%s]]" result

            let script = ""
            (script,gts)
                |> GTS.bucketize Bucketizer.Count 0 0 2
                |> GTS.join
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

                ("", gts @ gts2)
                    |> GTS.bucketize Bucketizer.And 2000 0 6
                    |> GTS.join
                    |> test expected

            it "should bucketize.or" <| fun _ ->
                let result = """{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[1049,true],[732,true],[415,true]]},{"c":"testname","l":{"label0":"42","label1":"foo"},"a":{},"v":[[2000,false],[732,true],[415,true]]}"""
                let expected = sprintf "[[%s]]" result

                ("", gts @ gts2)
                    |> GTS.bucketize Bucketizer.Or 2000 0 6
                    |> GTS.join
                    |> test expected

        describe "Filter" <| fun _ ->

            let gts1 =
                GTS.create
                    "GTS1"
                    (Some ["label0","42"])
                    [
                        UserDefined 10,None,None,None,LONG 42
                        UserDefined 20,None,None,None,LONG 123
                    ]

            let gts2 =
                GTS.create
                    "GTS2"
                    (Some ["label0","43"])
                    [
                        UserDefined 10,None,None,None,LONG 211
                        UserDefined 20,None,None,None,LONG 42
                    ]

            it "should filter.byClass" <| fun _ ->
                let result = """{"c":"GTS1","l":{"label0":"42"},"a":{},"v":[[10,42],[20,123]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.ByClass "~GTS1")
                    |> GTS.join
                    |> test expected

            it "should filter.byLabels" <| fun _ ->
                let result = """{"c":"GTS1","l":{"label0":"42"},"a":{},"v":[[10,42],[20,123]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None
                let searchLabels = ["label0","=42"]

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.ByLabels searchLabels)
                    |> GTS.join
                    |> test expected

            it "should filter.last.eq" <| fun _ ->
                let result = """{"c":"GTS1","l":{"label0":"42"},"a":{},"v":[[10,42],[20,123]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.LastEq (LONG 123))
                    |> GTS.join
                    |> test expected

            it "should filter.last.ne" <| fun _ ->
                let result = """{"c":"GTS2","l":{"label0":"43"},"a":{},"v":[[10,211],[20,42]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.LastNe (LONG 123))
                    |> GTS.join
                    |> test expected

            it "should filter.last.gt" <| fun _ ->
                let result = """{"c":"GTS1","l":{"label0":"42"},"a":{},"v":[[10,42],[20,123]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.LastGt (LONG 100))
                    |> GTS.join
                    |> test expected

            it "should filter.last.ge" <| fun _ ->
                let result = """{"c":"GTS1","l":{"label0":"42"},"a":{},"v":[[10,42],[20,123]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.LastGe (LONG 100))
                    |> GTS.join
                    |> test expected

            it "should filter.last.lt" <| fun _ ->
                let result = """{"c":"GTS2","l":{"label0":"43"},"a":{},"v":[[10,211],[20,42]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.LastLt (LONG 100))
                    |> GTS.join
                    |> test expected

            it "should filter.last.le" <| fun _ ->
                let result = """{"c":"GTS2","l":{"label0":"43"},"a":{},"v":[[10,211],[20,42]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.LastLe (LONG 100))
                    |> GTS.join
                    |> test expected

            it "should filter.last.le && filter.last.gt" <| fun _ ->
                let result = """{"c":"GTS2","l":{"label0":"43"},"a":{},"v":[[10,211],[20,42]]}"""
                let expected = sprintf "[[%s]]" result
                let labels = None

                let script = ""
                (script,gts1 @ gts2)
                    |> GTS.filter labels (Filter.LastLe (LONG 43))
                    |> GTS.filter labels (Filter.LastGt (LONG 40))
                    |> GTS.join
                    |> test expected
