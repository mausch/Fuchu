namespace Fuchu

open System
                
module Seq = 
    let (|Empty|Cons|) l = 
        if Seq.isEmpty l
            then Empty
            else Cons(Seq.head l, Seq.skip 1 l)

    let (|One|_|) l = 
        match Seq.toList l with
        | [x] -> Some x
        | _ -> None

    let (|Two|_|) l = 
        match Seq.toList l with
        | [x;y] -> Some(x,y)
        | _ -> None

module String =
    let internal nullBool2 f a b =
        if a = null && a = null then
            true
        elif a = null || b = null then
            false
        else
            f b a

    let internal nullOption2 f a b =
        nullBool2 f a b |> function true -> Some() | false -> None

    let (|StartsWith|_|) =
        nullOption2 (fun (s: string) -> s.StartsWith)

    let (|Contains|_|) =
        nullOption2 (fun (s: string) -> s.Contains)

[<AutoOpen>]
module TestHelpers = 
    open Fuchu
    open Fuchu.Impl

    let evalSilent t = eval TestPrinters.Default Seq.map t  

    let inline assertTestFails test = 
        let test = TestCase test
        match evalSilent test with
        | [{ TestRunResult.Result = TestResult.Failed _ }] -> ()
        | x -> failtestf "Should have failed, but was %A" x
    
