namespace Warp10

open Fable.Core
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Shared.Types

[<RequireQualifiedAccess>]
module Client =

    let prepareUrl endpoint operation=
        match endpoint.Port with
        | Some p -> sprintf "%s://%s:%i/api/v%s/%s" (string endpoint.Protocol) endpoint.Host p (string endpoint.ApiVersion) (string operation)
        | None -> sprintf "%s://%s/api/v%s/%s" (string endpoint.Protocol) endpoint.Host (string endpoint.ApiVersion) (string operation)

    let prepareHeaders token =
        requestHeaders [
            ContentType "text/plain"
            HttpRequestHeaders.Custom ("X-Warp10-Token", token)
        ]

    let update (endpoint:EndPoint, token:Token, value:UpdateRequest) =
        let token =
            match token with
            | Write tok -> tok
            | Read _ -> failwith "you need a Write token"

        let defaultProps =
            [
                RequestProperties.Method HttpMethod.POST
                prepareHeaders token
                RequestProperties.Body <| unbox (UpdateRequest.toString value)
            ]

        promise {
            let url =  prepareUrl endpoint Endpoint.Update
            let! res = fetch url defaultProps
            let status =
                match res.Status with
                | x when x = 200 -> Ok 200
                | x -> Error x
            return status
        }

    let delete (endpoint:EndPoint, token:Token, value:DeleteRequest) =
        let token =
            match token with
            | Write tok -> tok
            | Read _ -> failwith "you need a Write token"

        let defaultProps =
            [
                RequestProperties.Method HttpMethod.GET
                prepareHeaders token
            ]

        promise {
            let url =  prepareUrl endpoint Endpoint.Delete
            let url = sprintf "%s?%s" url (DeleteRequest.toString value)
            let! res = fetch url defaultProps
            let status =
                match res.Status with
                | x when x = 200 -> Ok 200
                | x -> Error x
            return status
        }

    let exec (endpoint:EndPoint, token:Token, script:string) =
        let token =
            match token with
            | Read tok -> tok
            | Write _ -> failwith "you need a Read token"

        let defaultProps =
            [
                RequestProperties.Method HttpMethod.POST
                prepareHeaders token
                RequestProperties.Body <| unbox script
            ]

        promise {
            let url =  prepareUrl endpoint Endpoint.Exec
            let! res = fetch url defaultProps
            match res.Ok with
            | true ->
                let! txt = res.text()
                return Ok txt
            | false ->
                return Error res.Status
            }

    let fetch (endpoint:EndPoint, token:Token, value:FetchRequest) =
        let token =
            match token with
            | Write tok -> failwith "you need a Read token"
            | Read tok -> tok

        let defaultProps =
            [
                RequestProperties.Method HttpMethod.GET
                prepareHeaders token
            ]

        promise {
            let url =  prepareUrl endpoint Endpoint.Fetch
            let url = sprintf "%s?%s" url (FetchRequest.toString value)
            let! res = fetch url defaultProps
            let! text = res.text()
            let status =
                match res.Status with
                | x when x = 200 -> Ok text
                | x -> Error x
            return status
        }
