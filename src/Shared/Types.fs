namespace Warp10.Shared

open System
open Fable.Core
open Fable.Core.JsInterop

module Types =

    [<Emit("typeof($0) === \"boolean\"")>]
    let isBool v : bool = jsNative

    [<Emit("Boolean($0)")>]
    let toBoolean v : bool = jsNative

    type Token =
        | Write of string
        | Read of string

    [<StringEnum>]
    type Protocol =
        | [<CompiledName("http")>] HTTP
        | [<CompiledName("https")>] HTTPS

    type EndPoint =
        {
            Url:string
            Protocol:Protocol
        }

    type FetchRequest =
        {
            Token:string
            ClassnameSelector:string
            LabelsSelector:string
            StartTimeStamp:string
            EndTimeStamp:string
        }

    type WGS84 = int * int
    type LabelKeyValue = string * string

    type ValueType =
        | LONG of int
        | DOUBLE of double
        | BOOL of bool
        | STRING of string

    let [<Literal>] microSecondsLength = 16

    type UpdateRequest =
        {
            TimeStamp:float option
            Latitude: WGS84 option
            Longitude: WGS84 option
            ClassName:string
            Labels:LabelKeyValue list
            Value:ValueType
        }
        static member fromString (input:string) =
            //1440000000000000// toto{a=42,b=42} 42
            printfn "input=%s" input

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
                let className, labels, value =
                    match data.Length with
                    | x when x = 2 ->
                        let className = data.[0]
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
                                    match Int32.TryParse(v) with
                                    | true, v -> LONG v
                                    | _ ->
                                        match isBool(v) with
                                        | true -> BOOL (toBoolean v)
                                        | _ ->
                                            match Double.TryParse(v) with
                                            | true, v -> DOUBLE v
                                            | _ -> STRING v

                                labels, value
                            | _ -> failwith "invalid format for labels and value"


                        className, labels, value
                    | _ -> failwith "invalid format"

                {
                    TimeStamp=ts
                    Latitude=lat
                    Longitude=long
                    ClassName = className
                    Labels = labels
                    Value = value
                }

            | _ -> failwith "invalid structure"

        static member toString (request:UpdateRequest) =
            let ts =
                match request.TimeStamp with
                | Some ts ->
                    let ts = sprintf "%f" ts
                    let ts = (ts.Split '.') |> String.Concat
                    let missing = microSecondsLength - ts.Length
                    let trailing = [0..missing] |> List.map ( fun v -> "0") |>  List.toSeq |> String.concat ""
                    sprintf "%s%s" ts trailing
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

            let cls = request.ClassName
            let labels =
                request.Labels
                |> Seq.map( fun lkv ->
                    let key,value = lkv
                    sprintf "%s=%s" key value
                )
                |> String.concat ","

            let value =
                match request.Value with
                | LONG v -> sprintf "%i" v
                | DOUBLE v -> sprintf "%f" v
                | BOOL v -> sprintf "%b" v
                | STRING v -> sprintf "'%s'" v


            let request = sprintf "%s/%s/ %s{%s} %s" ts latlong cls labels value
//            printfn "%s" request
            request

