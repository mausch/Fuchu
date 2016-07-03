﻿namespace Fuchu

module MbUnit =
    open Fuchu
    open System
    open System.Linq
    open System.Reflection
    open Fuchu.Helpers
    open Fuchu.XunitHelpers

    [<RequireQualifiedAccess>]
    type private MbUnitTest =
        | TestCase of string * TestCode
        | TestSuite of string * MbUnitTest list

    let rec private MbUnitTestToFuchuTest =
        function
        | MbUnitTest.TestCase(name, code) -> Test.TestLabel(name, Test.TestCase code)
        | MbUnitTest.TestSuite(name, tests) -> Test.TestLabel(name, Test.TestList (List.map MbUnitTestToFuchuTest tests))

    let private testCaseTypeName = "MbUnit.Framework.TestCase, MbUnit"
    let private testCaseType = lazy Type.GetType testCaseTypeName
    let private testCaseExecuteProperty = lazy testCaseType.Value.GetProperty "Execute"
    let private testCaseNameProperty = lazy testCaseType.Value.GetProperty "Name"
    let private testSuiteTypeName = "MbUnit.Framework.TestSuite, MbUnit"
    let private testSuiteType = lazy Type.GetType testSuiteTypeName
    let private testSuiteNameProperty = lazy testSuiteType.Value.GetProperty "Name"
    let private testSuiteChildrenProperty = lazy testSuiteType.Value.GetProperty "Children"
    let private gallioActionTypeName = "Gallio.Common.Action, Gallio"
    let private gallioActionType = lazy Type.GetType gallioActionTypeName
    let private gallioActionInvokeMethod = lazy gallioActionType.Value.GetMethod "Invoke"

    let private categoryAttributeType = lazy Type.GetType "MbUnit.Framework.CategoryAttribute, MbUnit"
    let private categoryAttributeNameProperty = lazy categoryAttributeType.Value.GetProperty "Category"

    let private rowAttributeType = lazy Type.GetType "MbUnit.Framework.RowAttribute, MbUnit"
    let private rowAttributeValuesProperty = lazy rowAttributeType.Value.GetProperty "Values"
    let private rowAttributeGetValues (a: Attribute) = rowAttributeValuesProperty.Value.GetValue(a, null) :?> obj[]

    let rec private buildMbUnitTest (o: obj) =
        let typeName = o.GetType().AssemblyQualifiedName
        if typeName.StartsWith testCaseTypeName then
            let name = testCaseNameProperty.Value.GetValue(o, null) :?> string
            let testCode = testCaseExecuteProperty.Value.GetValue(o, null)
            let invoke() = gallioActionInvokeMethod.Value.Invoke(testCode, null) |> ignore
            MbUnitTest.TestCase(name, invoke)
        elif typeName.StartsWith testSuiteTypeName then
            let name = testSuiteNameProperty.Value.GetValue(o, null) :?> string
            let children = testSuiteChildrenProperty.Value.GetValue(o, null) :?> System.Collections.IEnumerable
            let children = Seq.cast<obj> children |> Seq.map buildMbUnitTest |> Seq.toList
            MbUnitTest.TestSuite(name, children)
        else
            failwith (sprintf "%A is not a test type" o)

    let private MbUnitAttr = sprintf "MbUnit.Framework.%sAttribute"
    let private ignoreAttr = MbUnitAttr "Ignore"

    let MbUnitTestToFuchu (t: Type) =

        let testCategory (m: MemberInfo) =
            m.GetCustomAttributes(categoryAttributeType.Value, true)
#if DNXCORE50
            |> Array.ofSeq
#endif
            |> Array.map (fun a -> categoryAttributeNameProperty.Value.GetValue(a, null) :?> string)
            |> Enumerable.FirstOrDefault

        let mbUnitAttrs = 
            { Ignore = MbUnitAttr "Ignore"
              Test = MbUnitAttr "Test"
              Setup = MbUnitAttr "SetUp"
              TearDown = MbUnitAttr "TearDown"
              FixtureSetup = MbUnitAttr "FixtureSetUp"
              ExpectedException = MbUnitAttr "ExpectedException", "ExceptionType" }
        
        let nonIgnoredMethods = nonIgnoredMethods ignoreAttr t |> methodsWithAttrs

        let rowTests = 
            let rowAttr = MbUnitAttr "Row"
            nonIgnoredMethods [rowAttr]
            |> Seq.collect (fun (m: MethodInfo) -> 
                                m.GetAttributes rowAttr 
                                |> Seq.map rowAttributeGetValues
                                |> Seq.map (fun args -> 
                                                let argsString = args |> Seq.map (sprintf "%A") |> String.concat ","
                                                let testName = sprintf "%s(%s)" m.Name argsString
                                                { TestMethod.Name = testName
                                                  Invoke = fun o -> m.Invoke(o, args) |> ignore
                                                  ExpectedException = None }))

        let testMethods = Seq.append (getTestMethods testCategory mbUnitAttrs t) rowTests

#if DNXCORE50
        let test = TestToFuchu mbUnitAttrs (fun t -> t.GetTypeInfo() |> testCategory) t testMethods
#else
        let test = TestToFuchu mbUnitAttrs testCategory t testMethods
#endif

        let staticTests = 
            nonIgnoredMethods [MbUnitAttr "StaticTestFactory"]
            |> Seq.collect (fun m -> m.Invoke(null, null) :?> System.Collections.IEnumerable |> Seq.cast<obj>)
            |> Seq.map (buildMbUnitTest >> MbUnitTestToFuchuTest)
            |> Seq.toList

        TestList (seq {
            yield test
            if staticTests.Length > 0 then
#if DNXCORE50
                yield testList (t.FullName + testCategory (t.GetTypeInfo())) staticTests
#else
                yield testList (t.FullName + testCategory t) staticTests
#endif
        })
