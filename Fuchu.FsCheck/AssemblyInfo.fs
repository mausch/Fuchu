﻿namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.5.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.5.0.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu.FsCheck")>]
[<assembly: AssemblyProductAttribute("Fuchu.FsCheck")>]
[<assembly: AssemblyDescriptionAttribute("Integrates Fuchu with FsCheck")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Mauricio Scheffer 2014")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.0.0"
