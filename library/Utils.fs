namespace Bigsharp.Core

module Utils =
    let numberOfSmallerElements refVal set =
        Set.fold (fun count value -> if refVal > value then count+1 else count) 0 set