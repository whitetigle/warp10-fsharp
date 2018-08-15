namespace Warp10.Shared

open System
open Fable.Core
open Fable.Core.JsInterop

module WebUtils =

    [<Emit("encodeURI($0)")>]
    let urlEncode v : string = jsNative

    let warpEncode (str:string) =
        str
            .Replace("{","%7B")
            .Replace(",","%2C")
            .Replace("}","%7D")
            .Replace("=","%3D")

type TimeStamp = float

module DateUtils =

    let [<Literal>] microSecondsLength = 16

    let timestampToMicroSeconds ts =
        let ts = sprintf "%f" ts
        let ts = (ts.Split '.') |> String.concat ""
        let ts = ts.Substring(0, microSecondsLength-1)
        let missing = microSecondsLength - 1 - ts.Length
        let trailingZeros = [0..missing] |> List.map ( fun v -> "0") |>  List.toSeq |> String.concat ""
        sprintf "%s%s" ts trailingZeros

    let toISO8601 (dt:DateTime) =
        sprintf "%i-%02d-%02dT%02d:%02d:%02d.%02d0000Z" dt.Year dt.Month dt.Day dt.Hour dt.Minute dt.Second dt.Millisecond

module Types =

    let isBool (v:string) =
        if v.Contains "true" then Some true
        elif v.Contains "false" then Some false
        else None

    [<StringEnum>]
    type Endpoint =
        | [<CompiledName("fetch")>] Fetch
        | [<CompiledName("update")>] Update
        | [<CompiledName("exec")>] Exec
        | [<CompiledName("delete")>] Delete

    type Token =
        | Write of string
        | Read of string

    [<StringEnum>]
    type Protocol =
        | [<CompiledName("http")>] HTTP
        | [<CompiledName("https")>] HTTPS

    [<StringEnum>]
    type WarpApiVersion =
        | [<CompiledName("0")>] V0

    type EndPoint =
        {
            Host:string
            Port:int option
            Protocol:Protocol
            ApiVersion:WarpApiVersion
        }

    [<StringEnum>]
    type FetchFormat =
        | [<CompiledName("text")>] Text
        | [<CompiledName("fulltext")>] FullText

    type Interval =
        | StartAndStop of DateTime * DateTime
        | LastReadings of TimeStamp * int
        | Timespan of TimeStamp * TimeStamp

    type FetchRequest =
        {
            Selector:string
            Format:FetchFormat
            Dedup:bool
            Interval:Interval
        }
        static member toString (request:FetchRequest) =
            let requestParams = ["selector",request.Selector]
            let requestParams = requestParams @ ["dedup",sprintf "%b" request.Dedup]
            let requestParams = requestParams @ ["format",sprintf "%s" (string request.Format)]

            let requestParams =
                match request.Interval with
                | StartAndStop (start,stop) ->
                    requestParams
                        @ ["start",start |> DateUtils.toISO8601]
                        @ ["stop",stop |> DateUtils.toISO8601]

                | LastReadings (start,count) ->
                    let count =
                        if count < 0 then sprintf "%i" count else sprintf "-%i" count

                    requestParams
                        @ ["now",start |> DateUtils.timestampToMicroSeconds]
                        @ ["timespan", count]

                | Timespan (start,ts) ->
                    requestParams
                        @ ["now",start |> DateUtils.timestampToMicroSeconds]
                        @ ["timespan",ts |> DateUtils.timestampToMicroSeconds]

            requestParams
                |> List.map( fun kv ->
                    let key,value=kv
                    sprintf "%s=%s" key value
                )
                |> String.concat "&"


    type WGS84 = int * int
    type LabelKeyValue = string * string

    type ValueType =
        | LONG of int
        | DOUBLE of double
        | BOOL of bool
        | STRING of string

    type UpdateRequest =
        {
            TimeStamp:TimeStamp option
            Latitude: WGS84 option
            Longitude: WGS84 option
            Elevation: int option
            ClassName:string
            Labels:LabelKeyValue list
            Value:ValueType
        }
        static member fromString (input:string) =
            //1440000000000000// toto{a=42,b=42} 42
//            printfn "input=%s" input

            (*
                /!\ WARNING: this is an over simplified parser which does not take special characters into account
                like {,=}
                TODO: rework parser.
            *)
            let parts = input.Split '/'
            match parts.Length with
            | x when x = 3 ->
                let ts =
                    let currentPart = parts.[0].Trim()
                    match currentPart.Length with
                    | x when x = 0 -> None
                    | _ ->
                        let currentPart = currentPart.Substring(0,16)
                        match Single.TryParse(currentPart) with
                        | true, v -> Some  (float v)
                        | false, _ -> None

                let lat,long =
                    let parts = parts.[1].Split ':'
                    match parts.Length with
                    | x when x = 2 ->
                        let parseParts (parts:string) =
                            let sub = parts.Split '.'
                            match sub.Length with
                            | x when x = 2 ->
                                match Int32.TryParse(sub.[0]), Int32.TryParse(sub.[1]) with
                                | (true, m), (true,n) -> Some (m,n)
                                | _ -> failwith "invalid lat:long format. Invalid number"
                            | x when x = 1 ->
                                match Int32.TryParse(sub.[0]) with
                                | true, v -> Some (v,0)
                                | false, _ -> failwith "invalid lat:long format. Invalid number"
                            | _ -> failwith "invalid lat:long format. Invalid number"

                        let lat = parseParts parts.[0]
                        let long = parseParts parts.[1]
                        lat,long
                    | _ -> None,None


                let data = parts.[2].Trim().Split '{'
                let className, labels, value, elev =
                    match data.Length with
                    | x when x = 2 ->
                        let className, elev =
                            let tokens = data.[0].Split ' '
                            if tokens.Length > 1 then
                                let cls = tokens.[1]
                                let elev =
                                    match Int32.TryParse(tokens.[0]) with
                                    | true, v -> Some  (int v)
                                    | false, _ -> None
                                cls, elev
                            else
                                data.[0], None
                        let remaining = data.[1].Split '}'
                        let labels, value =
                            match remaining.Length with
                            | x when x = 2 ->
                                let kvs = remaining.[0].Trim().Split ','
                                let labels =
                                    kvs
                                        |> Seq.map( fun v -> v.Split '=')
                                        |> Seq.filter( fun data -> data.Length = 2)
                                        |> Seq.map( fun data ->
                                            let key = data.[0]
                                            let value = data.[1]
                                            key,value
                                        )
                                        |> Seq.toList
                                let value =
                                    let v = remaining.[1]
                                    match isBool(v) with
                                    | Some b -> BOOL b
                                    | _ ->
                                        match Int32.TryParse(v) with
                                        | true, v -> LONG v
                                        | _ ->
                                            match Double.TryParse(v) with
                                            | true, v -> DOUBLE v
                                            | _ ->
                                                let output = v.Replace("'", "")
                                                STRING (output.Trim())

                                labels, value
                            | _ -> failwith "invalid format for labels and value"


                        className, labels, value, elev
                    | _ -> failwith "invalid format"

                {
                    TimeStamp=ts
                    Latitude=lat
                    Longitude=long
                    Elevation = elev
                    ClassName = className
                    Labels = labels
                    Value = value
                }

            | _ -> failwith "invalid structure"

        static member toString (request:UpdateRequest) =
            let ts =
                match request.TimeStamp with
                | Some ts ->
                    DateUtils.timestampToMicroSeconds ts
                | None -> ""

            let latlong =
                match request.Latitude, request.Longitude  with
                | Some lat, Some long ->
                    let latmaj,latmin = lat
                    let longmaj,longmin = long
                    sprintf "%i.%i:%i.%i" latmaj latmin longmaj longmin
                | Some lat, None -> failwith "Missing longitude"
                | None, Some long -> failwith "Missing latitude"
                | _ -> ""

            let cls = request.ClassName |> WebUtils.urlEncode |> WebUtils.warpEncode
            let elev =
                match request.Elevation with
                | Some elev -> sprintf "%i" elev
                | None -> ""

            let labels =
                request.Labels
                |> Seq.map( fun lkv ->
                    let key,value = lkv
                    sprintf "%s=%s" (key|> WebUtils.urlEncode |> WebUtils.warpEncode) (value|> WebUtils.urlEncode |> WebUtils.warpEncode)
                )
                |> String.concat ","

            let value =
                match request.Value with
                | LONG v -> sprintf "%i" v
                | DOUBLE v -> double(v).ToString() // remove trailing zeros
                | BOOL v -> sprintf "%b" v
                | STRING v -> sprintf "'%s'" v


            let request = sprintf "%s/%s/%s %s{%s} %s" ts latlong elev cls labels value
//            printfn "%s" request
            request


    type DateFormat =
        | Timestamp of TimeStamp * TimeStamp
        | ISO8601 of DateTime * DateTime

    type DeleteRequest =
        | FullRange of string
        | Partial of string * DateFormat
        | From of string * DateTime

    module DeleteRequest =
        let toString (request:DeleteRequest) =
            let requestParams,deleteAll =
                match request with
                | FullRange s ->
                    ["selector",s], true

                | Partial (s, kind) ->
                    let parms =
                        match kind with
                        | Timestamp (start,stop) ->
                            ["selector",s]
                                @ ["start",start |> DateUtils.timestampToMicroSeconds]
                                @ ["end",stop |> DateUtils.timestampToMicroSeconds]
                        | ISO8601 (start,stop) ->
                            ["selector",s]
                                @ ["start",start |> DateUtils.toISO8601]
                                @ ["end",stop |> DateUtils.toISO8601]
                    parms, false

                | From (s, start) ->
                    let parms =
                        ["selector",s]
                            @ ["start",start |> DateUtils.toISO8601]
                    parms, false

            let requestString =
                requestParams
                    |> List.map( fun kv ->
                        let key,value=kv
                        sprintf "%s=%s" key value
                    )
                    |> String.concat "&"

            match deleteAll with
            | true ->
                sprintf "deleteall&%s" requestString
            | false ->
                sprintf "%s" requestString
