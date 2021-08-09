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


type SliceMap2D<'k1, 'k2, 'v 
                 when 'k1 : comparison
                 and 'k2 : comparison>
    (comparer1: IComparer<'k1>, 
     comparer2: IComparer<'k2>, 
     k1Values: 'k1[], 
     k1Ranges: IndexRange[], 
     keys2: ReadOnlyMemory<'k2>, 
     values: ReadOnlyMemory<'v>) =

    let k1Values = k1Values
    let k1Ranges = k1Ranges
    let keys2 = keys2
    let values = values
    let comparer1 = comparer1
    let comparer2 = comparer2


    new (keyValuePairs: seq<'k1 * 'k2 * 'v>) =
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


        SliceMap2D (compare1, compare2, key1Values, key1Ranges, ReadOnlyMemory keys2, ReadOnlyMemory values)


    member _.Item
        // Ignoring `f` at this time
        with get (x: 'k1, f: Filter) =

            let mutable intervalIdx = 0
            let mutable keepSearching = true

            while keepSearching && intervalIdx < k1Values.Length - 1 do
                if comparer1.Compare (k1Values.[intervalIdx], x) = 0 then
                    keepSearching <- false
                else
                    intervalIdx <- intervalIdx + 1

            if not keepSearching then
                let interval = k1Ranges.[intervalIdx]
                SliceMap (comparer2, keys2.Slice (interval.Start, interval.Length), values.Slice (interval.Start, interval.Length))
            else
                SliceMap (comparer2, ReadOnlyMemory Array.empty, ReadOnlyMemory Array.empty)

