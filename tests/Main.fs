module Tests.Main

open Fable.Core.JsInterop

// This is necessary to make webpack collect all test files
importAll "./Tests.Warp10.Client.fs"
importAll "./Tests.Warp10.Types.fs"
