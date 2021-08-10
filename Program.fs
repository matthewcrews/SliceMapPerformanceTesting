open System
open TestSliceMap
open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes

let dim1Count = 1_000
let dim2Count = 1_000
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

        for i in 0 .. dim1Count do
            result <- sum (currSm1 .* currSm2_100Dense.[i, All])

        for i in 0 .. dim1Count do
            result <- sum (currSm1 .* currSm2_100Dense.[All, i])

        result

    let ``10% Density SliceAndSum`` () =
    
        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
    
        for i in 0 .. dim1Count do
            result <- sum (currSm1 .* currSm2_10Dense.[i, All])

        for i in 0 .. dim1Count do
            result <- sum (currSm1 .* currSm2_10Dense.[All, i])
    
        result

    let ``1% Density SliceAndSum`` () =
        
        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
        
        for i in 0 .. dim1Count do
            result <- sum (currSm1 .* currSm2_1Dense.[i, All])

        for i in 0 .. dim1Count do
            result <- sum (currSm1 .* currSm2_1Dense.[All, i])
        
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
        
        for i in 0 .. dim1Count do
            result <- sum (testSm1 .* testSm2_100Dense.[i, TestSliceMap.Filter.All])

        for i in 0 .. dim1Count do
            result <- sum (testSm1 .* testSm2_100Dense.[TestSliceMap.Filter.All, i])
        
        result

    let ``10% Density SliceAndSum`` () =
    
        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
            
        for i in 0 .. dim1Count do
            result <- sum (testSm1 .* testSm2_10Dense.[i, TestSliceMap.Filter.All])
            
        for i in 0 .. dim1Count do
            result <- sum (testSm1 .* testSm2_10Dense.[TestSliceMap.Filter.All, i])

        result

    let ``1% Density SliceAndSum`` () =
        
        let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>
                
        for i in 0 .. dim1Count do
            result <- sum (testSm1 .* testSm2_1Dense.[i, TestSliceMap.Filter.All])

        for i in 0 .. dim1Count do
            result <- sum (testSm1 .* testSm2_1Dense.[TestSliceMap.Filter.All, i])
                
        result


[<MemoryDiagnoser>]
type SliceAndSumBenchmarks () =

    [<Benchmark>]
    member _.Old_100Percent_Density () =
        OldTests.``100% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.New_100Percent_Density () =
        NewTests.``100% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.Old_10Percent_Density () =
        OldTests.``10% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.New_10Percent_Density () =
        NewTests.``10% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.Old_1Percent_Density () =
        OldTests.``1% Density SliceAndSum`` ()

    [<Benchmark>]
    member _.New_1Percent_Density () =
        NewTests.``1% Density SliceAndSum`` ()


open Flips.Types

let profile () =
    
    let mutable result = FSharp.Core.LanguagePrimitives.GenericZero<LinearExpression>

    for _ in 0 .. 100_000 do
        result <- NewTests.``100% Density SliceAndSum`` ()

    result

[<EntryPoint>]
let main argv =

    //let summary = BenchmarkRunner.Run<SliceAndSumBenchmarks>()
    let x = profile ()

    //let x1 =
    //    [for i in 0..3 -> i, float i]
    //    |> SliceMap
    
    //let x2 =
    //    [for i in 0..4 do
    //        for j in 0..4 ->
    //            string i, j, i + j
    //    ]
    //    |> SliceMap2D
        

    //let s1 = x2.["1", All]
    //let s2 = x2.[All, 1]
    //let s3 = x2.["1", All]
    //let sum1 = sum s1
    //let sum2 = sum s2
    //let sum3 = sum s3
    //let r = sum s1

    ////let x2 = x1 .* x1

    ////let r = sum x2
    //let x =
    //    [1..10]
    //    |> List.map (fun x -> x, float x)
    //    |> SliceMap

    //let x2 =
    //    [for i in 1..10 do
    //        for j in 1..10 ->
    //            i, j, float (i + j)
    //    ] |> SliceMap2D
    
    //let x2Slice = x2.[1, All]
    
    //let r = x .* x2Slice
    //let r2 = sum r

    //printfn "%A" r2
    0 // return an integer exit code