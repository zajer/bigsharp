namespace Bigsharp.Core

module Utils =
    let numberOfSmallerElements refVal set =
        Set.fold (fun count value -> if refVal > value then count+1 else count) 0 set
    let rec nthAvailableNumber n start unavialable =
        match n with
        | 0 -> raise (invalidArg "n" "n has to be greater than 0")
        | 1 -> 
            match Set.contains start unavialable with
            | false -> start
            | true -> nthAvailableNumber n (start+1) unavialable
        | _ -> 
            match Set.contains start unavialable with
            | false -> nthAvailableNumber (n-1) (start+1) unavialable
            | true -> nthAvailableNumber n (start+1) (Set.remove start unavialable)
    let nthNaturalNumberOutsideProvidedSet n set =
        let nose = numberOfSmallerElements n set
        nthAvailableNumber (nose+1) n set

