module Main

open Fuchu
#if FABLE_COMPILER
let exitIfNonZero v =
    if v <> 0 then
        failwithf "expected a nonzero exitcode, but got %i" v
    v
#endif

[<EntryPoint>]
let main args =
    defaultMain Tests.tests args
    #if FABLE_COMPILER
    |> exitIfNonZero
    #endif
