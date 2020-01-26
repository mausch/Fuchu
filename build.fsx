#load ".fake/build.fsx/intellisense.fsx"
#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif
open System
open Fake.SystemHelper
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api
open Fake.BuildServer

let version = "1.1.0.0"
let assemblyVersion = "1.0.0.0"

type Package = {
    Name: string
    Author: string
    Description: string
    Sign: bool
}
let vsProjProps = [
     ("VersionSuffix", "")
     ("VersionPrefix", version)
]

module Pkg=
  let build (pkg:Package) = 
    MSBuild.runReleaseExt id null vsProjProps "pack" ["./"+pkg.Name @@ pkg.Name+".fsproj"]
    |> ignore
let packages = 
    [
        { Package.Name = "Fuchu"
          Author = "Mauricio Scheffer"
          Description = "Functional test library"
          Sign = true}

        { Package.Name = "Fuchu.MbUnit"
          Author = "Mauricio Scheffer"
          Description = "Converts Fuchu tests to MbUnit tests"
          Sign = false}

        { Package.Name = "Fuchu.FsCheck"
          Author = "Mauricio Scheffer"
          Description = "Integrates Fuchu with FsCheck"
          Sign = false}

        { Package.Name = "Fuchu.PerfUtil"
          Author = "Henrik Feldt"
          Description = "Integrates Fuchu with PerfUtil"
          Sign = false}
    ]

Target.create "BuildSolution" (fun _ ->
    MSBuild.runRelease id null "Restore" ["./Fuchu.sln"] |> ignore

    MSBuild.runRelease id null "Rebuild" ["./Fuchu.sln"] |> ignore
)

Target.create "NuGet" <| fun _ ->
    List.iter Pkg.build packages


Target.create "AssemblyInfo" <| fun _ ->
    let asmInfo (p: Package) =
        let attributes = [
            AssemblyInfo.Version assemblyVersion
            AssemblyInfo.FileVersion version
            AssemblyInfo.Title p.Name
            AssemblyInfo.Product p.Name
            AssemblyInfo.Description p.Description
            AssemblyInfo.Copyright (sprintf "Copyright %s %d" p.Author DateTime.Now.Year)
        ]
        let attributes = 
            if p.Sign 
                then (AssemblyInfo.KeyFile "../Fuchu.snk")::attributes
                else attributes
        AssemblyInfoFile.createFSharp (p.Name @@ "AssemblyInfo.fs") attributes
    List.iter asmInfo packages

Target.create "Test" <| fun _ ->
    let errorCode = 
        [
            "Fuchu.Tests"
            "Fuchu.CSharpTests"
        ]
        |> List.sumBy (fun t -> 
            let r=
                RawCommand( t @@ "bin" @@ "Release" @@ "net452" @@ (t + ".exe"), Arguments.Empty)
                |> CreateProcess.fromCommand 
                |> CreateProcess.withToolType ( ToolType.Create() )
                |> CreateProcess.withTimeout (TimeSpan.FromSeconds 10.)
                |> Proc.run
            r.ExitCode
        )
    if errorCode <> 0 then failwith "Error in tests"

"BuildSolution" <== ["AssemblyInfo"]
"Test" <== ["BuildSolution"]
"NuGet" <== ["Test"]

Target.runOrDefaultWithArguments "NuGet"
