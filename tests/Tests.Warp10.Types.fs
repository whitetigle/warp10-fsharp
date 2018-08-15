module Tests.Warp10.Types

open Fable.Core
open Warp10.Shared.Types
open Warp10.Shared.Script
open Util

[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative

describe "Warp10.Types" <| fun _ ->

    describe "Encoding" <| fun _ ->

        it "should convert a string into an update request" <| fun _ ->
            let input = "1440000000000000// toto{a=42,b=42} 42"
            let output = input |> UpdateRequest.fromString
            let wanted =
                {
                    TimeStamp=Some 1440000000000000.
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=LONG 42
                }

            equal output wanted

    describe "UpdateRequest fromString" <| fun _ ->

        it "should convert a string into an update request" <| fun _ ->
            let input = "1440000000000000// toto{a=42,b=42} 42"
            let output = input |> UpdateRequest.fromString
            let wanted =
                {
                    TimeStamp=Some 1440000000000000.
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=LONG 42
                }

            equal output wanted

        it "should convert a string into an update request" <| fun _ ->
            let input = "1440000000000000//1589 toto{a=42,b=42} 42"
            let output = input |> UpdateRequest.fromString
            let wanted =
                {
                    TimeStamp=Some 1440000000000000.
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=Some 1589
                    Labels=[("a","42");("b","42")]
                    Value=LONG 42
                }

            equal output wanted

        it "should convert a string into an update request" <| fun _ ->
            let input = "/12.5:14.8/1589 toto{a=42,b=42} 42"
            let output = input |> UpdateRequest.fromString
            let wanted =
                {
                    TimeStamp=None
                    Latitude=Some(12,5)
                    Longitude=Some(14,8)
                    ClassName="toto"
                    Elevation=Some 1589
                    Labels=[("a","42");("b","42")]
                    Value=LONG 42
                }

            equal output wanted

        it "should convert a string into an update request" <| fun _ ->
            let input = "// toto{a=42,b=42} 42"
            let output = input |> UpdateRequest.fromString
            let wanted =
                {
                    TimeStamp=None
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=LONG 42
                }

            equal output wanted

        it "should convert a string into an update request" <| fun _ ->
            let input = "// toto{a=42,b=42} true"
            let output = input |> UpdateRequest.fromString
            let wanted =
                {
                    TimeStamp=None
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=BOOL true
                }

            equal output wanted

        it "should convert a string into an update request" <| fun _ ->
            let input = "// toto{a=42,b=42} 'illbeback'"
            let output = input |> UpdateRequest.fromString
            let wanted =
                {
                    TimeStamp=None
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=STRING "illbeback"
                }

            equal output wanted

        it "should convert a string into an update request" <| fun _ ->
            let input = "// toto{a=42,b=42} 25.896"
            let output = input |> UpdateRequest.fromString
            let wanted =
                {
                    TimeStamp=None
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=DOUBLE 25.896
                }

            equal output wanted

    describe "UpdateRequest toString" <| fun _ ->

        it "should convert an update request into a string request" <| fun _ ->
            let wanted =
                {
                    TimeStamp=None
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=BOOL true
                }
            let expected = "// toto{a=42,b=42} true"
            let actual = wanted |> UpdateRequest.toString
            equal expected actual

        it "should convert an update request into a string request" <| fun _ ->
            let wanted =
                {
                    TimeStamp=Some 1440000000000000.
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=BOOL true
                }
            let expected = "1440000000000000// toto{a=42,b=42} true"
            let actual = wanted |> UpdateRequest.toString
            equal expected actual

        it "should convert an update request into a string request" <| fun _ ->
            let wanted =
                {
                    TimeStamp=None
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=DOUBLE 25.896
                }
            let expected = "// toto{a=42,b=42} 25.896"
            let actual = wanted |> UpdateRequest.toString
            equal expected actual

        it "should convert an update request into a string request" <| fun _ ->
            let wanted =
                {
                    TimeStamp=None
                    Latitude=None
                    Longitude=None
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=STRING "illbeback"
                }
            let expected = "// toto{a=42,b=42} 'illbeback'"
            let actual = wanted |> UpdateRequest.toString
            equal expected actual

        it "should convert an update request into a string request" <| fun _ ->
            let wanted =
                {
                    TimeStamp=Some 1440000000000000.
                    Latitude=Some (42,8)
                    Longitude=Some (58,5)
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=BOOL true
                }
            let expected = "1440000000000000/42.8:58.5/ toto{a=42,b=42} true"
            let actual = wanted |> UpdateRequest.toString
            equal expected actual

        it "should convert an update request into a string request" <| fun _ ->
            let wanted =
                {
                    TimeStamp=Some 1440000000000000.
                    Latitude=Some (42,8)
                    Longitude=Some (58,5)
                    ClassName="toto"
                    Elevation=None
                    Labels=[("a","42");("b","42")]
                    Value=DOUBLE 25.898
                }
            let expected = "1440000000000000/42.8:58.5/ toto{a=42,b=42} 25.898"
            let actual = wanted |> UpdateRequest.toString
            equal expected actual

        it "should convert an update request into a string request" <| fun _ ->
            let wanted =
                {
                    TimeStamp=Some 1440000000000000.
                    Latitude=Some (42,8)
                    Longitude=Some (58,5)
                    ClassName="toto"
                    Elevation=Some 1589
                    Labels=[("a","42");("b","42")]
                    Value=DOUBLE 25.898
                }
            let expected = "1440000000000000/42.8:58.5/1589 toto{a=42,b=42} 25.898"
            let actual = wanted |> UpdateRequest.toString
            equal expected actual

    describe "FastReading -> Reading" <| fun _ ->
        it "should convert a FastReading into a Reading" <| fun _ ->
            let expected =
                {
                    TimeStamp=Now
                    Latitude=Some (42,8)
                    Longitude=Some (58,5)
                    Elevation=Some 1589
                    Value=DOUBLE 25.898
                }
            let wanted = Now, Some (42,8), Some (58,5), Some 1589,DOUBLE 25.898
            let actual = wanted |> FastReading.toReading
            equal expected actual
