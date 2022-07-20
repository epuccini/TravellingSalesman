namespace GA.Salesman.Array

open System.Collections.Generic
open GA.Salesman.Global

module Array =
    let insertAt (a: '_a array) (idx : int) e =
        let newArray : '_a array = Array.create ((Array.length a) + 1) e
        for i = 0 to idx - 1 do
            newArray.[i] <- a.[i]
        newArray.[idx] <- e
        for i = idx + 1 to (Array.length a) do
            newArray.[i] <- a.[i - 1]
        newArray

    let swap (a: '_a array) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

