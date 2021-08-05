open System
open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes

let dim1Count = 10_000
let dim2Count = 10_000
let numberIterations = 1
let rng = System.Random 123

let ``data2D 100% Dense`` =
    [for i in 0 .. dim1Count do
        for j in 0 .. dim2Count ->
            i, j, float (i + j)
    ]

let ``data2D 10% Dense`` =
    [for i in 0 .. dim1Count do
        for j in 0 .. dim2Count ->
            if rng.NextDouble () < 0.1 then
                Some (i, j, float (i + j))
            else
                None
    ] |> List.choose id

let ``data2D 1% Dense`` =
    [for i in 0 .. dim1Count do
        for j in 0 .. dim2Count ->
            if rng.NextDouble () < 0.01 then
                Some (i, j, float (i + j))
            else
                None
    ] |> List.choose id

let data1D = [for i in 0 .. dim1Count -> i, float i]


module OldTests =
    open Flips.Types
    open Flips.SliceMap
    
    let currSm1 = 
        data1D
        |> SMap

    let currSm2_100Dense = 
        ``data2D 100% Dense``
        |> List.map (fun (k1, k2, v) -> (k1, k2), v)
        |> SMap2

    let currSm2_10Dense = 
        ``data2D 10% Dense``
        |> List.map (fun (k1, k2, v) -> (k1, k2), v)
        |> SMap2

    let currSm2_1Dense = 
        ``data2D 1% Dense``
        |> List.map (fun (k1, k2, v) -> (k1, k2), v)
        |> SMap2

    let ``100% Density SliceAndSum`` () =

        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>

        for _ in 1 .. numberIterations do
            for i in 0 .. dim1Count do
                result <- sum (currSm1 .* currSm2_100Dense.[i, All])

        result

    let ``10% Density SliceAndSum`` () =
    
        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
    
        for _ in 1 .. numberIterations do
            for i in 0 .. dim1Count do
                result <- sum (currSm1 .* currSm2_10Dense.[i, All])
    
        result

    let ``1% Density SliceAndSum`` () =
        
        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
        
        for _ in 1 .. numberIterations do
            for i in 0 .. dim1Count do
                result <- sum (currSm1 .* currSm2_1Dense.[i, All])
        
        result


module NewTests =
    open Flips.Types
    open TestSliceMap

    // New format
    let testSm2_100Dense = TestSliceMap.SliceMap2D ``data2D 100% Dense``
    let testSm2_10Dense = TestSliceMap.SliceMap2D ``data2D 10% Dense``
    let testSm2_1Dense = TestSliceMap.SliceMap2D ``data2D 1% Dense``
    let testSm1 = TestSliceMap.SliceMap data1D

    let ``100% Density SliceAndSum`` () =

        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
        
        for _ in 1 .. numberIterations do
            for i in 0 .. dim1Count do
                result <- sum (testSm1 .* testSm2_100Dense.[i, TestSliceMap.Filter.All])
        
        result

    let ``10% Density SliceAndSum`` () =
    
        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
            
        for _ in 1 .. numberIterations do
            for i in 0 .. dim1Count do
                result <- sum (testSm1 .* testSm2_10Dense.[i, TestSliceMap.Filter.All])
            
        result

    let ``1% Density SliceAndSum`` () =
        
        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
                
        for _ in 1 .. numberIterations do
            for i in 0 .. dim1Count do
                result <- sum (testSm1 .* testSm2_1Dense.[i, TestSliceMap.Filter.All])
                
        result


[<MemoryDiagnoser>]
type Benchmarks () =

    [<Benchmark>]
    member _.``Current SM: 100% Density SliceAndSum`` () =
        OldTests.``100% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.``Current SM: 10% Density SliceAndSum`` () =
        OldTests.``10% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.``Current SM: 1% Density SliceAndSum`` () =
        OldTests.``1% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.``New SM: 100% Density SliceAndSum`` () =
        NewTests.``100% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.``New SM: 10% Density SliceAndSum`` () =
        NewTests.``10% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.``New SM: 1% Density SliceAndSum`` () =
        NewTests.``1% Density SliceAndSum`` ()

[<EntryPoint>]
let main argv =

    let summary = BenchmarkRunner.Run<Benchmarks>()

    0 // return an integer exit code