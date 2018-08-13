namespace Warp10.Shared

open System
open Fable.Core
open Fable.Core.JsInterop

module Types =

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
            TimeStamp:int64 option
            Latitude: WGS84 option
            Longitude: WGS84 option
            ClassName:string
            Labels:LabelKeyValue list
            Value:ValueType
        }
        static member prepare (request:UpdateRequest) =
            let ts =
                match request.TimeStamp with
                | Some ts ->
                    let ts = sprintf "%i" ts
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

