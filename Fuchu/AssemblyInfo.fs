﻿namespace System
open System.Reflection

[<assembly: AssemblyKeyFileAttribute("../Fuchu.snk")>]
[<assembly: AssemblyVersionAttribute("0.4.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.4.0.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu")>]
[<assembly: AssemblyProductAttribute("Fuchu")>]
[<assembly: AssemblyDescriptionAttribute("Functional test library")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Mauricio Scheffer 2014")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.4.0.0"
