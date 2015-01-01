﻿namespace Fuchu

open System
open global.PerfUtil

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FuchuPerfUtil =
    open Fuchu
    open Fuchu.Helpers
    open Fuchu.Impl

    open System.IO

    /// Create a new performance test. The values given from this method are usable both
    /// by PerfUtil as well as Fuchu's testPerfImplsWithConfig, testPerfImpls,
    /// testPerfHistoryWithConfig and testPerfHistory. You can give the values from this
    /// function to both Fuchu and PerfUtil.
    let perfTest name testImpl repeat =
        { PerfTest.Id = name
          Test        = testImpl
          Repeat      = repeat }

    type PerfImplsConf =
          /// <summary>
          /// Whether to throw <see cref="PerfUtil.PerformanceException" />
          /// if the subject is slower than the alternative that it is compared to.
          /// Useful for making sure you don't accidentally write code that degrades
          /// performance. Defaults to false.
          /// </summary>
        { ThrowOnError  : bool
          /// <summary>
          /// The comparer for how much 'better' you need the subject to be. Defaults to
          /// <see cref="PerfUtil.MeanComparer" />.
          /// </summary>
          Comparer      : IPerformanceComparer
          /// Whether to print results to stdout. Defaults to true.
          Verbose       : bool
          /// An optional function that is called when the perf tests have been completed
          /// allowing you to extrace the results and save them or display them.
          HandleResults : TestSession list -> unit }
        static member Defaults =
            { ThrowOnError  = false
              Comparer      = WeightedComparer()
              Verbose       = true
              HandleResults = fun _ -> () }

    /// <summary>
    /// Compares given implementation performance against a collection of other implementations.
    /// Use the 'perfTest' function to easily construct test cases.
    /// </summary>
    /// <param name="conf">The <see cref="" /> configuration</param>
    /// <param name="name">Name for the group of performance tests</param>
    /// <param name="subject">Implementation under test.</param>
    /// <param name="alternatives">Secondary implementations to be compared against.</param>
    /// <param name="tests">The performance tests to run against the subject and the alternatives.</param>
    let testPerfImplsWithConfig (conf : PerfImplsConf) name subject alternatives tests =
        let tester () =
            new ImplementationComparer<_>(subject, alternatives, conf.Comparer, conf.Verbose, conf.ThrowOnError)
                :> PerformanceTester<_>

        testCase name <| fun _ ->
            let results = PerfTest.run tester tests
            conf.HandleResults results

    /// <summary>
    /// Compares given implementation performance against a collection of other implementations.
    /// Use the 'perfTest' function to easily construct test cases. With this function, the configuration
    /// will be the sane defaults; if you want to override them, please see <see cref="testPerfImplsWithConfig" />.
    /// </summary>
    /// <param name="name">Name for the group of performance tests</param>
    /// <param name="subject">Implementation under test.</param>
    /// <param name="alternatives">Secondary implementations to be compared against.</param>
    /// <param name="tests">The performance tests to run against the subject and the alternatives.</param>
    let testPerfImpls name subj alts tests =
        testPerfImplsWithConfig PerfImplsConf.Defaults name subj alts tests

    /// A configuration for the historical performance development for a given implementation.
    type PerfHistoryConf =
          /// path to history file
        { HistoryFile   : string
          Comparer      : IPerformanceComparer
          /// Whether to print results to stdout. Defaults to true.
          Verbose       : bool
          /// Whether to throw if the subject has gotten worse in comparison to previous runs, 
          /// as decided by the 'comparer'.
          ThrowOnError  : bool
          /// Whether to overwrite previous tests. Defaults to true.
          Overwrite     : bool
          /// Perform a warmup run before attempting benchmark. Defaults to false.
          Warmup        : bool
          /// An optional function that is called when the perf tests have been completed
          /// allowing you to extrace the results and save them or display them. 
          /// It will be passed the path of the xml file with test results and
          /// the list of TestSessions that comes from PerfUtil.
          HandleResults : string * TestSession list -> unit }
        /// Defaults to a xml file in the currently executing DLL's directory
        /// named the same as the collection of perf tests.
        static member Defaults testName =
            { HistoryFile   = Path.Combine(Path.GetDirectoryName(PerfUtil.DefaultPersistenceFile), testName + ".xml")
              Comparer      = WeightedComparer(0.05, 1.0)
              Verbose       = true
              ThrowOnError  = false
              Overwrite     = true
              Warmup        = false
              HandleResults = fun _ -> () }

    // (currentImpl : 'Testable, testRunId : string, ?historyFile : string, ?warmup,
    //  ?comparer : IPerformanceComparer, ?verbose : bool, ?throwOnError : bool, ?overwrite : bool)

    /// <summary>
    /// Compares current implementation against a collection of past tests.
    /// </summary>
    /// <param name="conf">Configuration for the historical performance test</param>
    /// <param name="name">Name for the group of performance tests</param>
    /// <param name="subject">(Current) implementation under test.</param>
    /// <param name="testRunId">The id of the test run; subsequent runs must grow this value, so
    /// a recommended value for this parameter is the current assembly version.</param>
    /// <param name="tests">The performance tests to run against the subject and the alternatives.</param>
    let testPerfHistoryWithConfig (conf : PerfHistoryConf) name subject (testRunId: string) tests =
        let tester =
            new PastImplementationComparer<_>(
                subject, testRunId, conf.HistoryFile, conf.Warmup, conf.Comparer,
                conf.Verbose, conf.ThrowOnError, conf.Overwrite)

        testCase name <| fun _ ->
            let results = PerfTest.run (fun () -> tester :> PerformanceTester<_>) tests
            tester.PersistCurrentResults()
            conf.HandleResults(conf.HistoryFile, results)

    /// <summary>
    /// Compares current implementation against a collection of past tests.
    /// </summary>
    /// <param name="name">Name for the group of performance tests</param>
    /// <param name="subject">(Current) implementation under test.</param>
    /// <param name="testRunId">The id of the test run; subsequent runs must grow this value, so
    /// a recommended value for this parameter is the current assembly version.</param>
    /// <param name="tests">The performance tests to run against the subject and the alternatives.</param>
    let testPerfHistory name subject (testRunId : string) =
        testPerfHistoryWithConfig (PerfHistoryConf.Defaults name) name subject testRunId
