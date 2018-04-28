#I @"packages/Build/FAKE/tools/"
#r @"FakeLib.dll"
#r "System.Xml.Linq"

open System
open Fake
open Fake.ProcessHelper
open Fake.AssemblyInfoFile

let version = "1.0.3.0"
let assemblyVersion = "1.0.0.0"

type Package = {
    Name: string
    Author: string
    Description: string
    Sign: bool
}
module Pkg=
  let build (pkg:Package) = 
    MSBuildRelease null "pack" ["./"+pkg.Name]
    |> Log "Package-Output: "

let packages = 
    [
        { Package.Name = "Fuchu"
          Author = "Mauricio Scheffer"
          Description = "Functional test library"
          Sign = true}

        { Package.Name = "Fuchu.MbUnit"
          Author = "Mauricio Scheffer"
          Description = "Converts Fuchu tests to MbUnit tests"
          Sign = true}

        { Package.Name = "Fuchu.FsCheck"
          Author = "Mauricio Scheffer"
          Description = "Integrates Fuchu with FsCheck"
          Sign = false}

        { Package.Name = "Fuchu.PerfUtil"
          Author = "Henrik Feldt"
          Description = "Integrates Fuchu with PerfUtil"
          Sign = false}
    ]

Target "BuildSolution" (fun _ ->
    MSBuildRelease null "Rebuild" ["./Fuchu.sln"]
    |> Log "AppBuild-Output: "
)

Target "NuGet" <| fun _ ->
    List.iter Pkg.build packages


Target "AssemblyInfo" <| fun _ ->
    let asmInfo (p: Package) =
        let attributes = [
            Attribute.Version assemblyVersion
            Attribute.FileVersion version
            Attribute.Title p.Name
            Attribute.Product p.Name
            Attribute.Description p.Description
            Attribute.Copyright (sprintf "Copyright %s %d" p.Author DateTime.Now.Year)
        ]
        let attributes = 
            if p.Sign 
                then (Attribute.KeyFile "../Fuchu.snk")::attributes
                else attributes
        CreateFSharpAssemblyInfo (p.Name @@ "AssemblyInfo.fs") attributes
    List.iter asmInfo packages

Target "Test" <| fun _ ->
    let errorCode = 
        [
            "Fuchu.Tests"
            "Fuchu.CSharpTests"
        ]
        |> Seq.map (fun t -> t @@ "bin" @@ "Release" @@ (t + ".exe"))
        |> Seq.map (fun p -> if not isMono then p,null else "mono",p)
        |> Seq.map (fun (p,a) -> asyncShellExec { defaultParams with Program = p; CommandLine = a })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.sum
    if errorCode <> 0 then failwith "Error in tests"

"BuildSolution" <== ["AssemblyInfo"]
"Test" <== ["BuildSolution"]
"NuGet" <== ["Test"]

RunTargetOrDefault "NuGet"
