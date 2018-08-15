namespace Warp10.Shared

open System
open Fable.Core
open Fable.Core.JsInterop
open Types
open Thoth.Json

module Script =

    type TimeStamp =
        | Now
        | UserDefined of int

    type Reading =
        {
            TimeStamp:TimeStamp
            Latitude: WGS84 option
            Longitude: WGS84 option
            Elevation: int option
            Value:ValueType
        }

    type FastReading = TimeStamp * WGS84 option * WGS84 option * int option * ValueType

    module FastReading =
        let toReading fastreading =
            let ts, lat, long, elev, value = fastreading
            { TimeStamp = ts;Latitude = lat;Longitude = long;Elevation=elev;Value=value}


    type ReadingValue = float list

    type Output<'T> =
        {
            ClassName:string
            Labels:'T
            Readings:ReadingValue list
        }
        static member Decoder decoder =
            Decode.decode
                (fun c l r->
                    {   ClassName=c
                        Labels=l
                        Readings=r
                    }:Output<'T>)
                        |> Decode.required "c" Decode.string
                        |> Decode.required "l" decoder
                        |> Decode.required "v" (Decode.list (Decode.list Decode.float) )

    [<StringEnum>]
    type MiscToken =
        | [<CompiledName("NaN")>] Nan

    [<RequireQualifiedAccess>]
    module Date =
        [<StringEnum>]
        type CommandNames =
            | [<CompiledName("NOW")>] Now

    [<RequireQualifiedAccess>]
    module Bucketizer =
        [<StringEnum>]
        type CommandNames =
            | [<CompiledName("bucketizer.sum")>] Sum
            | [<CompiledName("bucketizer.max")>] Max
            | [<CompiledName("bucketizer.min")>] Min
            | [<CompiledName("bucketizer.mean")>] Mean
            | [<CompiledName("bucketizer.first")>] First
            | [<CompiledName("bucketizer.last")>] Last
            | [<CompiledName("bucketizer.median")>] Median
            | [<CompiledName("bucketizer.count")>] Count
            | [<CompiledName("bucketizer.and")>] And
            | [<CompiledName("bucketizer.or")>] Or

        type WithParams =
            | MeanCircular of float
            | MeanCircularNoNull of float
            | Join of string


    [<RequireQualifiedAccess>]
    module Stack =
        [<StringEnum>]
        type CommandNames =
            | [<CompiledName("SWAP")>] Swap

    [<RequireQualifiedAccess>]
    module Frameworks =
        [<StringEnum>]
        type CommandNames =
            | [<CompiledName("BUCKETIZE")>] Bucketize
            | [<CompiledName("MAP")>] Map
            | [<CompiledName("FILTER")>] Filter
            | [<CompiledName("REDUCE")>] Reduce
            | [<CompiledName("APPLY")>] Apply

        [<RequireQualifiedAccess>]
        type Command =
            | Bucketize of Bucketizer.CommandNames * int * int * int

    [<RequireQualifiedAccess>]
    module GTS =

        [<StringEnum>]
        type CommandNames =
            | [<CompiledName("NEWGTS")>] New
            | [<CompiledName("RENAME")>] Rename
            | [<CompiledName("RELABEL")>] Relabel
            | [<CompiledName("ADDVALUE")>] AddValue
            | [<CompiledName("GET")>] Get
            | [<CompiledName("0 GET")>] Extract

        [<RequireQualifiedAccess>]
        type Command =
            | New
            | Rename of string
            | Relabel of (string * string) list
            | AddValue of Reading
            | Framework of Frameworks.Command

        let toScript (commands:Command list) =
            commands
                |> List.map( fun cmd ->
                    match cmd with
                    | Command.New ->
                        (string CommandNames.New)

                    | Command.Rename name ->
                        sprintf "'%s' %s" name (string CommandNames.Rename)

                    | Command.Relabel list ->
                        let labels =
                            list
                                |> List.map( fun (k,v) ->
                                    sprintf "'%s' '%s'" k v
                                )
                                |> String.concat " "
                        sprintf "{ %s } %s" labels (string CommandNames.Relabel)

                    | Command.AddValue reading ->
                        let timestamp =
                            match reading.TimeStamp with
                            | TimeStamp.Now -> (string Date.Now)
                            | TimeStamp.UserDefined ts -> sprintf "%i" ts

                        let lat =
                            match reading.Latitude with
                            | Some lat ->
                                let m,n = lat
                                sprintf "%i.%i" m n
                            | None -> (string Nan)

                        let long =
                            match reading.Longitude with
                            | Some long ->
                                let m,n = long
                                sprintf "%i.%i" m n
                            | None -> (string Nan)

                        let elev =
                            match reading.Elevation with
                            | Some elev ->  sprintf "%i" elev
                            | None -> (string Nan)

                        let value =
                            match reading.Value with
                            | LONG v -> sprintf "%i" v
                            | DOUBLE v -> double(v).ToString() // remove trailing zeros
                            | BOOL v -> sprintf "%b" v
                            | STRING v -> sprintf "'%s'" v

                        sprintf "%s %s %s %s %s %s" timestamp lat long elev value (string CommandNames.AddValue)
                )
                |> String.concat " "

        let create name labels values =
            match labels with
            | Some labels ->
                [
                    Command.New
                    Command.Rename name
                    Command.Relabel labels
                ] @ (values |> List.map (FastReading.toReading >> Command.AddValue))
            | None ->
                [
                    Command.New
                    Command.Rename name
                ] @ (values |> List.map (FastReading.toReading >> Command.AddValue))


        let toList script =
            sprintf "[ %s ]" script

        let bucketize bucketizer lastbucket bucketspan bucketcount gts =
            let currentGTS = gts |> toScript |> toList
            sprintf "[ %s %s %i %i %i ] %s" currentGTS (string bucketizer) lastbucket bucketspan bucketcount (string Frameworks.Bucketize)

        let bucketizeWithParams bucketizer lastbucket bucketspan bucketcount gts =
            let currentGTS = gts |> toScript |> toList
            match bucketizer with
            | Bucketizer.Join param ->
                sprintf "[ %s ' %s ' bucketizer.join %i %i %i ] %s" currentGTS param lastbucket bucketspan bucketcount (string Frameworks.Bucketize)
            | Bucketizer.MeanCircular param ->
                sprintf "[ %s %s bucketizer.mean.circular %i %i %i ] %s" currentGTS (double(param).ToString()) lastbucket bucketspan bucketcount (string Frameworks.Bucketize)
            | Bucketizer.MeanCircularNoNull param ->
                sprintf "[ %s %s bucketizer.mean.circular.exclude-nulls %i %i %i ] %s" currentGTS (double(param).ToString()) lastbucket bucketspan bucketcount (string Frameworks.Bucketize)
            | _ ->
                failwith (sprintf "this operation does not allow parameters: %s" (string bucketizer))

        (*
        NEWGTS "GTS1" RENAME
{ 'label0' '42' } RELABEL
10 NaN NaN NaN  42  ADDVALUE
20 NaN NaN NaN 123  ADDVALUE
*)
