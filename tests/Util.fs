module Util

open Fable.Core
open Fable.Import
open Fable.PowerPack
open Fable.Core.Testing
open Warp10.Shared.Types
open Warp10.Shared.Script
open System

let inline equal (expected: 'T) (actual: 'T): unit =
  Testing.Assert.AreEqual(expected, actual)

[<Global>]
let describe (msg: string) (f: unit->unit): unit = jsNative

[<Emit("this.timeout($0)")>]
let timeout (duration:float):unit= jsNative

// exec polyfill for node
JsInterop.importAll "isomorphic-fetch"

// assume warp10 is working locally and tokens have been set
// change this token to yours
let readToken : Token = Read "7GJWBE3sW4PKDrgmRH1NP8y02DhMyQZtnn1chiuJGIxqF0Hi9USmcujWm.qj_4eN3YFIyZ551ofyRFygmOYEkVaSf_hlc.4ScQ1aQhCvd6PrbuQ8Q7.MC."

// change this token to yours
let writeToken : Token = Write "GgBpQQSFcQ82WSOhaG7toJDAJ1EQhoVQz8O11LO_KVJRGjqPBc2r3r5WtwMne2gXj1Wj.vWkp3fUSx5GF6Piy4UDuPh5oxaV0V.LyX96Gog"

// change this host and port to yours
let server = { Host="192.168.99.100"; Port=Some 8080; ApiVersion=V0; Protocol=HTTP}

module TestsWithPromises =

    [<Global>]
    let it (msg: string) (f: unit->JS.Promise<'T>): unit = jsNative

