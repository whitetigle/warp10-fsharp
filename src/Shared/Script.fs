namespace Warp10.Shared

open System
open Fable.Core
open Fable.Core.JsInterop
open Types
open Thoth.Json

module Script =

    let parseValueType v =
        match v with
        | LONG v -> sprintf "%i" v
        | DOUBLE v -> double(v).ToString() // remove trailing zeros
        | BOOL v -> sprintf "%b" v
        | STRING v -> sprintf "'%s'" v

    type ReadingTimeStamp =
        | Now
        | UserDefined of int

    type Reading =
        {
            TimeStamp:ReadingTimeStamp
            Latitude: WGS84 option
            Longitude: WGS84 option
            Elevation: int option
            Value:ValueType
        }

    type FastReading = ReadingTimeStamp * WGS84 option * WGS84 option * int option * ValueType

    [<RequireQualifiedAccess>]
    module FastReading =

        let toReading fastreading =
            let ts, lat, long, elev, value = fastreading
            { TimeStamp = ts;Latitude = lat;Longitude = long;Elevation=elev;Value=value}

        let toUpdateRequest classname labels (fastreading:FastReading) =
            let ts, lat, long, elev, value = fastreading
            let ts =
                match ts with
                | Now -> None
                | UserDefined value -> (float value) * 1000000. |> Some
            {
                TimeStamp=ts
                Latitude= lat
                Longitude= long
                Elevation= elev
                ClassName=classname
                Labels=labels
                Value=value
            }


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
        type CommandName =
            | [<CompiledName("NOW")>] Now

    [<RequireQualifiedAccess>]
    module Bucketizer =
        type CommandName =
            | Sum
            | Max
            | Min
            | Mean
            | First
            | Last
            | Median
            | Count
            | And
            | Or
            | MeanCircular of float
            | MeanCircularNoNull of float
            | Join of string

    [<RequireQualifiedAccess>]
    module Filter =
        type CommandName =
            | ByClass of string
            | ByLabels of (string * string) list
            | LastEq of ValueType
            | LastNe of ValueType
            | LastGt of ValueType
            | LastGe of ValueType
            | LastLt of ValueType
            | LastLe of ValueType

    [<RequireQualifiedAccess>]
    module Mapper =
        type CommandName =
            | Abs

    [<RequireQualifiedAccess>]
    module Stack =
        [<StringEnum>]
        type CommandName =
            | [<CompiledName("SWAP")>] Swap

    [<RequireQualifiedAccess>]
    module Frameworks =
        type CommandName =
            | Bucketize
            | Map
            | Filter
            | Reduce
            | Apply

    [<RequireQualifiedAccess>]
    module GTS =

        type CommandName =
            | NewGTS
            | Rename
            | Relabel
            | AddValue
            | Get
            | Fetch

        [<RequireQualifiedAccess>]
        type Command =
            | New
            | Rename of string
            | Relabel of (string * string) list
            | AddValue of Reading

        let toScript (commands:Command list) =
            commands
                |> List.map( fun cmd ->
                    match cmd with
                    | Command.New ->
                        (string CommandName.NewGTS).ToUpper()

                    | Command.Rename name ->
                        sprintf "'%s' %s" name ((string CommandName.Rename).ToUpper())

                    | Command.Relabel list ->
                        let labels =
                            list
                                |> List.map( fun (k,v) ->
                                    sprintf "'%s' '%s'" k v
                                )
                                |> String.concat " "
                        sprintf "{ %s } %s" labels ((string CommandName.Relabel).ToUpper())

                    | Command.AddValue reading ->
                        let timestamp =
                            match reading.TimeStamp with
                            | ReadingTimeStamp.Now -> (string Date.Now)
                            | ReadingTimeStamp.UserDefined ts -> sprintf "%i" ts

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

                        let value = parseValueType reading.Value

                        sprintf "%s %s %s %s %s %s" timestamp lat long elev value ((string CommandName.AddValue).ToUpper())
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

        let join (currentScript,(gts:Command list)) =
            let hasGTS = not gts.IsEmpty // if we get results from a FETCH, then it means we do not have any newly created GTS to manage
            if hasGTS then
                let output = sprintf "[ %s ] %s" (gts |> toScript) currentScript
                printfn "%s" output
                output
            else
                let output = sprintf "%s" currentScript
                printfn "%s" output
                output

        let bucketize bucketizer lastbucket bucketspan bucketcount (previousScript,gts) =
            let command = (string Frameworks.Bucketize).ToUpper()
            let script =
                match bucketizer with
                | Bucketizer.Join param ->
                    sprintf "[ SWAP ' %s ' bucketizer.join %i %i %i ] %s" param lastbucket bucketspan bucketcount command
                | Bucketizer.MeanCircular param ->
                    sprintf "[ SWAP %s bucketizer.mean.circular %i %i %i ] %s" (double(param).ToString()) lastbucket bucketspan bucketcount command
                | Bucketizer.MeanCircularNoNull param ->
                    sprintf "[ SWAP %s bucketizer.mean.circular.exclude-nulls %i %i %i ] %s" (double(param).ToString()) lastbucket bucketspan bucketcount command
                | _ ->
                    let operation = (string bucketizer).ToLower()
                    sprintf "[ SWAP bucketizer.%s %i %i %i ] %s" operation lastbucket bucketspan bucketcount command

            sprintf "%s %s" previousScript script, gts

        let map mapper preWindow postWindow occurences (previousScript,gts) =
            let command = (string Frameworks.Map).ToUpper()
            let script =
                match mapper with
                | _ ->
                    let operation = (string mapper).ToLower()
                    sprintf "[ SWAP mapper.%s %i %i %i ] %s" operation preWindow postWindow occurences command

            sprintf "%s %s" previousScript script, gts

        let fetch token classQuery labelsQuery (stop:double) (elapsedTime:double) (previousScript,gts) =
            let token =
                match token with
                | Write tok -> failwith "you need a Read token"
                | Read tok -> tok

            let command = (string Fetch).ToUpper()
            let args =
                labelsQuery |> List.map( fun (k,v) -> sprintf "'%s' '%s'" k v ) |> String.concat " "

            let stop = stop.ToString().Split('.').[0]
            let elapsedTime = elapsedTime.ToString().Split('.').[0]

            let script =
                sprintf "[ '%s' '%s' { %s } %s %s ] %s" token classQuery args stop elapsedTime command

            sprintf "%s %s" previousScript script, gts

        let reduce labels reducer (previousScript,gts) =
            let command = (string Frameworks.Reduce).ToUpper()
            let labels =
                match labels with
                | Some labels -> failwith "labels are not yet handled" // labels |> toScript
                | None -> "[]"

            let script =
                match reducer with
                | _ ->
                    let operation = (string reducer).ToLower()
                    sprintf "[ SWAP %s reducer.%s  ] %s" labels operation command

            sprintf "%s %s" previousScript script, gts

        let apply labels op (previousScript,gts) =
            let command = (string Frameworks.Apply).ToUpper()
            let labels =
                match labels with
                | Some labels -> failwith "labels are not yet handled" // labels |> toScript
                | None -> "[]"

            let script =
                match op with
                | _ ->
                    let operation = (string op).ToLower()
                    sprintf "[ SWAP %s %s ] %s" labels operation command

            sprintf "%s %s" previousScript script, gts


        let filter labels filter (previousScript,gts) =
            let operation = (string filter).ToLower()
            let command = (string Frameworks.Filter).ToUpper()
            let labels =
                match labels with
                | Some labels -> failwith "labels are not yet handled" // labels |> toScript
                | None -> "[]"

            let query =
                match filter with
                | Filter.ByClass param ->
                    sprintf "[ SWAP %s '%s' filter.byclass ] %s" labels param command

                | Filter.ByLabels list ->
                    let args =
                        list |> List.map( fun (k,v) -> sprintf "'%s' '%s'" k v ) |> String.concat " "

                    sprintf "[ SWAP %s { %s } filter.bylabels ] %s" labels args command

                | _ ->
                    let operation,param =
                        match filter with
                        | Filter.LastEq param ->
                            "filter.last.eq", parseValueType param
                        | Filter.LastNe param ->
                            "filter.last.ne", parseValueType param
                        | Filter.LastGe param ->
                            "filter.last.ge", parseValueType param
                        | Filter.LastGt param ->
                            "filter.last.gt", parseValueType param
                        | Filter.LastLt param ->
                            "filter.last.lt", parseValueType param
                        | Filter.LastLe param ->
                            "filter.last.le", parseValueType param
                        | _ ->  failwith (sprintf "unhandled command %A" filter)

                    sprintf "[ SWAP %s %s %s ] %s" labels param operation command

            sprintf "%s %s" previousScript query, gts

