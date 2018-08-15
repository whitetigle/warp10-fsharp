module Tests.Warp10.PrepareData

open Fable.Core
open Fable.PowerPack
open Warp10.Shared.Types
open Warp10.Shared.Script
open Util
open Util.TestsWithPromises
open Warp10.Shared.DateUtils

describe "Warp10.PrepareData" <| fun _ ->

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

        (*
            The sensor sample takes 40 arduino cards with four sensors which send their data every 15 minutes
            The tests take place at this date: 2018-08-15 18:00:00
            sensor1 (temperature/°C)
            sensor2 (temperature/°C)
            sensor3 (light/lux)
            sensor4 (pressure/psi)
            batteryStatus (remaining)
        *)

        let myGTS =
            let now = System.DateTime(2018,08,15,18,0,0)
            let max = 99
            let populateData i : FastReading list =
                [
                    for i in 0..max do
                        let minutesAgo = max - i
                        let now = now.AddMinutes (float minutesAgo * 15.)
                        let unix = System.DateTimeOffset(now).ToUnixTimeSeconds()
                        let value = i
                        yield (UserDefined (int unix),None,None,None, LONG value)
                ]

            [ 0..39 ]
                |> Seq.map( fun i ->
                    let id = sprintf "%i" i

                    let gts1 =
                        (populateData i)
                        |> List.map (FastReading.toUpdateRequest "myarduino.sensor1" ["kind","temp";"id",id])

                    let gts2 =
                        (populateData i)
                        |> List.map (FastReading.toUpdateRequest "myarduino.sensor2" ["kind","temp";"id",id])


                    let gts3 =
                        (populateData i)
                        |> List.map (FastReading.toUpdateRequest "myarduino.sensor3" ["kind","lux";"id",id])

                    let gts4 =
                        (populateData i)
                        |> List.map (FastReading.toUpdateRequest "myarduino.sensor4" ["kind","psi";"id",id])

                    gts1 @ gts2 @ gts3 @ gts4
                )
                |> Seq.concat
                |> Seq.toList

        it "should insert the data" <| fun _ ->
            let expected = 200

            Warp10.Client.batchUpdate (server, writeToken, myGTS )
                |> Promise.map ( fun res ->
                    let result =
                        match res with
                        | Ok code -> code
                        | Error code -> code
                    equal expected result
                )
