module rec TestSliceMap

open System
open System.Collections.Generic

type Filter =
    | All

[<Struct>]
type IndexRange = {
    Start : int
    Length : int
}

let inline hadamardProduct (l: SliceMap<_,_>, r: SliceMap<_,_>) =
    let lKeys = l.Keys.Span
    let lValues = l.Values.Span
    let rKeys = r.Keys.Span
    let rValues = r.Values.Span
    let outKeys = Array.zeroCreate l.Keys.Length
    let outValues = Array.zeroCreate r.Keys.Length

    let mutable outIdx = 0
    let mutable lIdx = 0
    let mutable rIdx = 0

    while lIdx < lKeys.Length && rIdx < rKeys.Length do
        let c = l.Comparer.Compare (lKeys.[lIdx], rKeys.[rIdx])

        if c = 0 then
            outKeys.[outIdx] <- lKeys.[lIdx]
            outValues.[outIdx] <- lValues.[lIdx] * rValues.[rIdx]
            outIdx <- outIdx + 1
            lIdx <- lIdx + 1
            rIdx <- rIdx + 1
        elif c < 0 then
            lIdx <- lIdx + 1
        else
            rIdx <- rIdx + 1

    // Only want the data we actually computed
    SliceMap (l.Comparer, ReadOnlyMemory (outKeys, 0, outIdx), ReadOnlyMemory (outValues, 0, outIdx))


let inline sum (x : SliceMap<_,_>) =
    let values = x.Values.Span
    let mutable acc = LanguagePrimitives.GenericZero
    for idx = 0 to x.Values.Length - 1 do
        acc <- acc + values.[idx]
    acc


type SliceMap<'k, 'v when 'k : comparison> (comparer: IComparer<'k>, keys: ReadOnlyMemory<'k>, values: ReadOnlyMemory<'v>) =

    let comparer = comparer
    let keys = keys
    let values = values

    new (keyValuePairs: seq<'k * 'v>) =
        let data =
            let x = Array.ofSeq keyValuePairs
            Array.sortInPlaceBy fst x
            x

        let keys = data |> Array.map fst
        let values = data |> Array.map snd
        let comparer = LanguagePrimitives.FastGenericComparer<'k>
        SliceMap (comparer, ReadOnlyMemory keys, ReadOnlyMemory values)

    member _.Keys : ReadOnlyMemory<'k> = keys
    member _.Values : ReadOnlyMemory<'v> = values
    member _.Comparer : IComparer<'k> = comparer

    static member inline ( .* ) (l: SliceMap<_,_>, r: SliceMap<_,_>) =
        if l.Keys.Length > r.Keys.Length then
            hadamardProduct (l, r)
        else
            hadamardProduct (r, l)



let private toIntervals (x: _[]) =

    let groups = Array.groupBy id x

    let keyLengths =
        groups
        |> Array.map (fun (key, group) -> key, group.Length)

    let startIdxs =
        keyLengths
        |> Array.scan (fun acc (k, length) -> acc + length) 0

    Array.zip keyLengths startIdxs.[.. startIdxs.Length - 2]
    |> Array.map (fun ((key, length), startIdx) -> key, { Start = startIdx; Length = length })

[<Struct>]
type SliceMap2DInternals<'k1, 'k2, 'v when 'k1 : comparison and 'k2 : comparison> = {
    OuterComparer : IComparer<'k1>
    InnerComparer : IComparer<'k2>
    OuterKeyValues : 'k1[]
    OuterKeyRanges : IndexRange[]
    InnerKeyValues : ReadOnlyMemory<'k2>
    Values : ReadOnlyMemory<'v>
}

module private SliceMap2DInternals =

    let create (keyValuePairs: seq<'k1 * 'k2 * 'v>) =
        let keySelector (k1, k2, _) = k1, k2
        let valueSelector (_, _, v) = v
        let data =
            let x = Array.ofSeq keyValuePairs
            Array.sortInPlaceBy keySelector x
            x

        let keys = data |> Array.map keySelector
        let keys1 = keys |> Array.map fst
        let keysAndSpans = toIntervals keys1
        let key1Values = keysAndSpans |> Array.map fst
        let key1Ranges = keysAndSpans |> Array.map snd
        let keys2 = keys |> Array.map snd
        let values = data |> Array.map valueSelector
        let compare1 = LanguagePrimitives.FastGenericComparer<'k1>
        let compare2 = LanguagePrimitives.FastGenericComparer<'k2>
        {
            OuterComparer = compare1
            InnerComparer = compare2
            OuterKeyValues = key1Values
            OuterKeyRanges = key1Ranges
            InnerKeyValues = ReadOnlyMemory keys2
            Values = ReadOnlyMemory values
        }


    let swapKeys (s: SliceMap2DInternals<'k1, 'k2, 'v>) =
        let innerKeyValues = s.InnerKeyValues.Span
        let values = s.Values.Span
        let keyTuples : (struct ('k2 * 'k1 * 'v))[] = Array.zeroCreate innerKeyValues.Length

        let mutable outerKeyIdx = 0
        let mutable outerKeyCount = 0

        for innerKeyIdx = 0 to s.InnerKeyValues.Length - 1 do
            keyTuples.[innerKeyIdx] <- struct (innerKeyValues.[innerKeyIdx], s.OuterKeyValues.[outerKeyIdx], values.[innerKeyIdx])
            outerKeyCount <- outerKeyCount + 1

            if outerKeyCount = s.OuterKeyRanges.[outerKeyIdx].Length then
                outerKeyIdx <- outerKeyIdx + 1
                outerKeyCount <- 0

        let keySelector struct (k1, k2, _) = struct (k1, k2)
        let valueSelector struct (_, _, v) = v
        let keys = keyTuples |> Array.map keySelector
        let keys1 = keys |> Array.map (fun struct (k1, _) -> k1)
        let keysAndSpans = toIntervals keys1
        let key1Values = keysAndSpans |> Array.map fst
        let key1Ranges = keysAndSpans |> Array.map snd
        let keys2 = keys |> Array.map (fun struct (_, k2) -> k2)
        let values = keyTuples |> Array.map valueSelector

        {
            OuterComparer = s.InnerComparer
            InnerComparer = s.OuterComparer
            OuterKeyValues = key1Values
            OuterKeyRanges = key1Ranges
            InnerKeyValues = ReadOnlyMemory keys2
            Values = ReadOnlyMemory values
        }


type SliceMap2DState<'k1, 'k2, 'v when 'k1 : comparison and 'k2 : comparison> =
    | Key1Key2 of SliceMap2DInternals<'k1, 'k2, 'v>
    | Key2Key1 of SliceMap2DInternals<'k2, 'k1, 'v>


module private SliceMap2DState =

    let swap (s: SliceMap2DState<'k1, 'k2, 'v>) =
        match s with
        | Key1Key2 internals -> SliceMap2DState.Key2Key1 (SliceMap2DInternals.swapKeys internals)
        | Key2Key1 internals -> SliceMap2DState.Key1Key2 (SliceMap2DInternals.swapKeys internals)


type SliceMap2D<'k1, 'k2, 'v 
                 when 'k1 : comparison
                 and 'k2 : comparison>
    (internalState: SliceMap2DState<_, _, _>) =

    let mutable internalState = internalState


    new (keyValuePairs: seq<'k1 * 'k2 * 'v>) =
        let internals = SliceMap2DInternals.create keyValuePairs
        let state = SliceMap2DState.Key1Key2 internals
        SliceMap2D state


    member _.Item
        // Ignoring `f` at this time
        with get (x: 'k1, f: Filter) =

            let internals =
                match internalState with
                | SliceMap2DState.Key1Key2 i -> i
                | SliceMap2DState.Key2Key1 i -> 
                    let reOrdered = SliceMap2DInternals.swapKeys i
                    internalState <- SliceMap2DState.Key1Key2 reOrdered
                    reOrdered

            let mutable intervalIdx = 0
            let mutable keepSearching = true

            while keepSearching && intervalIdx < internals.OuterKeyValues.Length - 1 do
                if internals.OuterComparer.Compare (internals.OuterKeyValues.[intervalIdx], x) = 0 then
                    keepSearching <- false
                else
                    intervalIdx <- intervalIdx + 1

            if not keepSearching then
                let interval = internals.OuterKeyRanges.[intervalIdx]
                SliceMap (internals.InnerComparer, internals.InnerKeyValues.Slice (interval.Start, interval.Length), internals.Values.Slice (interval.Start, interval.Length))
            else
                SliceMap (internals.InnerComparer, ReadOnlyMemory Array.empty, ReadOnlyMemory Array.empty)


    member _.Item
        // Ignoring `f` at this time
        with get (f: Filter, x: 'k2) =
            let internals =
                match internalState with
                | SliceMap2DState.Key2Key1 i -> i
                | SliceMap2DState.Key1Key2 i -> 
                    let reOrdered = SliceMap2DInternals.swapKeys i
                    internalState <- SliceMap2DState.Key2Key1 reOrdered
                    reOrdered

            let mutable intervalIdx = 0
            let mutable keepSearching = true

            while keepSearching && intervalIdx < internals.OuterKeyValues.Length - 1 do
                if internals.OuterComparer.Compare (internals.OuterKeyValues.[intervalIdx], x) = 0 then
                    keepSearching <- false
                else
                    intervalIdx <- intervalIdx + 1

            if not keepSearching then
                let interval = internals.OuterKeyRanges.[intervalIdx]
                SliceMap (internals.InnerComparer, internals.InnerKeyValues.Slice (interval.Start, interval.Length), internals.Values.Slice (interval.Start, interval.Length))
            else
                SliceMap (internals.InnerComparer, ReadOnlyMemory Array.empty, ReadOnlyMemory Array.empty)