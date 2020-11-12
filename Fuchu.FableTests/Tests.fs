namespace Fuchu

open System
module Strings=
  let contains (substr:string) (t: string) = t.Contains substr

module Tests =
    open Fuchu
    open Fuchu.Impl
    open System.Threading
    open System.IO
    open System.Globalization

    let (==?) actual expected = Assert.Equal("", expected, actual)

    let tests = 
        TestList [
            testCase "basic" <| fun _ -> Assert.Equal("2+2", 4, 2+2)

            test "using computation expression" {
                Assert.Equal("2+2", 4, 2+2)
            }

            testList "Test filter" [
                let tests = 
                    TestList [
                        testCase "a" ignore
                        testCase "b" ignore
                        testList "c" [
                            testCase "d" ignore
                            testCase "e" ignore
                        ]
                    ]
                yield testCase "with one testcase" <| fun _ -> 
                    let t = Test.filter ((=) "a") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 1 // same as assertEqual "" 1 t.Length
                yield testCase "with nested testcase" <| fun _ ->
                    let t = Test.filter (Strings.contains "d") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 1
                yield testCase "with one testlist" <| fun _ ->
                    let t = Test.filter (Strings.contains "c") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 2
                yield testCase "with no results" <| fun _ ->
                    let t = Test.filter ((=) "z") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 0
            ]
            testList "Exception handling" [
                testCase "Fuchu ignore" <| fun _ ->
                    let test () = skiptest "b"
                    let test = TestCase test
                    match evalSilent test with
                    | [{ Result = Ignored "b" }] -> ()
                    | x -> failtestf "Expected result = Ignored, got\n %A" x
            ]

            (*testList "Timeout" [
                testCaseAsync "fail" <| fun _ -> async{
                    let test = TestCaseAsync(Test.timeoutAsync 10 (fun _ -> Async.Sleep 100))
                    let! evald = evalSilent test
                    let result = sumTestResults evald
                    return result.Failed ==? 1 }
                testCaseAsync "pass" <| fun _ -> async{
                    let test = TestCaseAsync(Test.timeoutAsync 1000 (fun _-> async.Return() ))
                    let! evald = evalSilent test
                    let result = sumTestResults evald
                    return result.Passed ==? 1 }
            ]*)

            testList "parse args" [
                testCase "default" <|
                    fun _ ->
                        let opts = parseArgs [||]
                        opts.Parallel ==? false

                testCase "parallel" <|
                    fun _ ->
                        let opts = parseArgs [|"/m"|]
                        opts.Parallel ==? true
            ]

            testList "assertions" [
                testList "NotEqual" [
                    testCase "pass" <| fun _ ->
                        Assert.NotEqual("should be different", "", "monkey")
                    testCase "fail" <| fun _ ->
                        let test () = Assert.NotEqual("should fail", "", "")
                        assertTestFails test
                ]

                testList "string contain" [
                    testCase "pass" <| fun _ ->
                        Assert.StringContains("", "hello", "hello world")
                        
                    testCase "fail" <| fun _ ->
                        let test () = Assert.StringContains("", "a", "hello world")
                        assertTestFails test
                ]
            ]

            testList "computation expression" [
                let testNormal a =
                    testCase "" <| fun _ ->
                        if a < 0
                            then failtest "negative"
                        if a > 5
                            then failwith "over 5"
                let testCompExp a = 
                    test "" {
                        if a < 0
                            then failtest "negative"
                        if a > 5
                            then failwith "over 5"
                    }
                for c in [-5; 1; 6] ->
                    testCase (sprintf "compare comp.exp. and normal with value %d" c) <| fun _ -> 
                        let normal = evalSilent <| testNormal c
                        let compexp = evalSilent <| testCompExp c
                        let normalTag = TestResult.tag normal.[0].Result
                        let compexpTag = TestResult.tag compexp.[0].Result
                        Assert.Equal("result", normalTag, compexpTag) 
            ]
        ]
