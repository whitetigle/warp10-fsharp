# Disclaimer
:warning: This is a highly experimental project. While many things work, it should be noted that the Api may change. So for now use at your own risks :warning:

# warp10-fsharp
Fable/F# Client for [Warp10 Geo Time series](http://www.warp10.io)

Package | Stable | Prerelease
--- | --- | ---
Warp10.Client | [![NuGet Badge](https://buildstats.info/nuget/Warp10.Client)](https://www.nuget.org/packages/Warp10.Client/) | [![NuGet Badge](https://buildstats.info/nuget/Warp10.Client?includePreReleases=true)](https://www.nuget.org/packages/Warp10.Client/)

## Installation
- Install this library from nuget
```
paket add Warp10.Client --project /path/to/Project.fsproj
```

## Fable: How to use

### Install Warp10 steps
1 - Install warp10

2 - generate a read token and a write token
> Everything is explained in the official [docs](http://www.warp10.io/getting-started/)

### Run the tests

1 - Register server and token data in `tests/Util.fs`

```fsharp
let readToken : Token = Read "YOUR_READ_TOKEN"

// change this token to yours
let writeToken : Token = Write "YOUR_WRITE_TOKEN"

// change this host and port to yours
let server = { Host="192.168.99.100"; Port=Some 8080; ApiVersion=V0; Protocol=HTTP}
```

2 - uncomment the prepareData test in `tests/Main.fs`

```fsharp
importAll "./Tests.Warp10.PrepareData.fs"
```

3 - Run the test target

```shell
> ./fake.sh build [-t MochaTest]
```

4 - Make sure all the tests pass

*If tests do not pass, check the errors in the console.*

Common errors:
- warp server is not started
- server information is not set (check `tests/Util.fs`)
- tokens are not set (check `tests/Util.fs`)
- you are using docker on windows/mac and localhost is not available. Just put your local IP and it should work.


## .NET: work in progress
The main target being Fable/NodeJS, the .NET target has not been coded yet.
However, the project could easily be ported to .NET.
*Please feel free to submit a PR.*

### Step to build the repo

```shell
> ./fake.sh build
```
