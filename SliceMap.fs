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
    SliceMap (ReadOnlyMemory (outKeys, 0, outIdx), ReadOnlyMemory (outValues, 0, outIdx))

let inline sum (x : SliceMap<_,_>) =
    let values = x.Values.Span
    let mutable acc = LanguagePrimitives.GenericZero
    for idx = 0 to x.Values.Length - 1 do
        acc <- acc + values.[idx]
    acc


type SliceMap<'k, 'v when 'k : comparison> (keys: ReadOnlyMemory<'k>, values: ReadOnlyMemory<'v>) =

    let comparer = LanguagePrimitives.FastGenericComparer<'k>
    let keys = keys
    let values = values

    new (keyValuePairs: seq<'k * 'v>) =
        let data =
            let x = Array.ofSeq keyValuePairs
            Array.sortInPlaceBy fst x
            x

        let keys = data |> Array.map fst
        let values = data |> Array.map snd
        SliceMap (ReadOnlyMemory keys, ReadOnlyMemory values)

    member _.Keys : ReadOnlyMemory<'k> = keys
    member _.Values : ReadOnlyMemory<'v> = values
    member _.Comparer : IComparer<'k> = comparer

    static member inline ( .* ) (l: SliceMap<_,_>, r: SliceMap<_,_>) =
        if l.Keys.Length > r.Keys.Length then
            hadamardProduct (l, r)
        else
            hadamardProduct (r, l)