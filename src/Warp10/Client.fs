namespace Warp10

open Fable.Core
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Shared.Types

[<RequireQualifiedAccess>]
module Client =

    let update (endpoint:EndPoint, token:Token, value:UpdateRequest) =
        let token =
            match token with
            | Write tok -> tok
            | Read _ -> failwith "you need a Write token"

        let defaultProps =
            [
                RequestProperties.Method HttpMethod.POST
                requestHeaders [
                    ContentType "text/plain"
                    HttpRequestHeaders.Custom ("X-Warp10-Token", token)
                ]
                RequestProperties.Body <| unbox (UpdateRequest.toString value)
            ]

        promise {
            let url = sprintf "%s://%s/api/v0/update" (string endpoint.Protocol) endpoint.Url
            let! res = fetch url defaultProps
            let status =
                match res.Status with
                | x when x = 200 -> Ok 200
                | x -> Error x
            return status
        }

    let fetch (endpoint:EndPoint, token:Token, script:string) =
        let token =
            match token with
            | Read tok -> tok
            | Write _ -> failwith "you need a Read token"

        let defaultProps =
            [
                RequestProperties.Method HttpMethod.POST
                requestHeaders [
                    ContentType "text/plain"
                    HttpRequestHeaders.Custom ("X-Warp10-Token", token)
                ]
                RequestProperties.Body <| unbox script
            ]

        promise {
            let url = sprintf "%s://%s/api/v0/exec" (string endpoint.Protocol) endpoint.Url
            let! res = fetch url defaultProps
            match res.Ok with
            | true ->
                let! txt = res.text()
                return Ok txt
            | false ->
                return Error res.Status
            }

    let exec = fetch
