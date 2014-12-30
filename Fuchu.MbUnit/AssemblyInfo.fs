﻿namespace System
open System.Reflection

[<assembly: AssemblyKeyFileAttribute("../Fuchu.snk")>]
[<assembly: AssemblyVersionAttribute("0.4.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.4.0.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu.MbUnit")>]
[<assembly: AssemblyProductAttribute("Fuchu.MbUnit")>]
[<assembly: AssemblyDescriptionAttribute("Converts Fuchu tests to MbUnit tests")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Mauricio Scheffer 2014")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.4.0.0"
