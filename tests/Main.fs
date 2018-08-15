module Tests.Main

open Fable.Core.JsInterop

importAll "./Tests.Warp10.Client.fs"
importAll "./Tests.Warp10.Types.fs"
importAll "./Tests.Warp10.Script.fs"
importAll "./Tests.Warp10.Samples.fs"

// uncomment this test to put some data inside your warp local database
//importAll "./Tests.Warp10.PrepareData.fs"
