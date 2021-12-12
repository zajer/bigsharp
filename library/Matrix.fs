namespace Bigsharp.Core

module Matrix =
    let ShiftValues shift structure = 
        Array.Parallel.map (fun set -> Set.map (fun value -> value+shift) set) structure
    [<CustomEquality; NoComparison>]
    type matrix = {structure: int Set array;width:int;height:int} with
        member this.isEqualTo another =
            match another.height=this.height,another.width=this.width with
            | true, true ->
                let partialResults = 
                        Array.Parallel.mapi 
                            (
                                fun idx rowInThis -> 
                                    let rowInAnother = another.structure.[idx]
                                    rowInThis = rowInAnother
                            )
                            this.structure
                Array.fold (fun currentRes partialRes -> currentRes &&  partialRes) true partialResults
            | _ , _ -> false
        member this.fill data =
            if Array.length data > this.height then
                raise (invalidArg "data" "There are more rows in provided data than the heigh of the matrix")
            else (
                let newStructure =
                    Array.Parallel.mapi 
                        (
                            fun row currentDataSet -> 
                                let providedDataSet = data.[row]
                                if Set.minElement providedDataSet < 0 then
                                    raise (invalidArg "data" $"The provided data for row {row} sets positive value in a column with negative index")
                                else if Set.maxElement providedDataSet > this.width then
                                    raise (invalidArg "data" $"The provided data for row {row} sets positive value in a column with index exceeding the width of the matrix")
                                else
                                    Set.union currentDataSet providedDataSet
                        )
                        this.structure
                {structure=newStructure;width=this.width;height=this.height}
            )
        member this.extend another =
            let resultStructure = Array.append this.structure (ShiftValues this.width another.structure)
            {structure=resultStructure;width=this.width+another.width;height=this.height+another.height}
        member this.shrink rows cols =
            let newStructure = 
                Array.init 
                    (this.height-(Set.count rows) ) 
                    (fun i -> 
                        let origIdx = Utils.nthNaturalNumberOutsideProvidedSet i rows
                        this.structure.[origIdx]
                    )
                |> Array.Parallel.map
                    (fun rowSet -> 
                        Set.difference rowSet cols |>
                        Set.map 
                            (fun v -> 
                                v - Utils.numberOfSmallerElements v cols
                            )
                    )
            {structure=newStructure;width=this.width-(Set.count cols);height=this.height-(Set.count rows)}
        override this.Equals another =
            match another with
            | :? matrix as mx -> this.isEqualTo mx
            | _ -> false
        override this.GetHashCode () =
            let structHash = this.structure.GetHashCode()
            (structHash+17*this.width+7*this.height)
        member this.getElem row col =
            if Set.contains col this.structure.[row] then
                1
            else
                0
        override this.ToString () = 
            let sb = System.Text.StringBuilder ()
            for i in 0 .. (this.height-1) do 
                (
                for j in 0 .. (this.width-1) do
                    (
                    let elem = this.getElem i j
                    sb.Append $"{elem}" |> ignore
                    sb.Append " " |> ignore
                    )
                if i < (this.height-1) then
                    sb.Append System.Environment.NewLine |> ignore
                )
            sb.ToString ()
    let create numOfRows numOfCols dataAsAoA =
            if Array.length dataAsAoA > numOfRows then
                raise (invalidArg "data" "There are more rows in provided data than the heigh of the matrix")
            else (
                let structure =
                    Array.Parallel.mapi 
                        (
                            fun row dataForRow -> 
                                if Array.length dataForRow > numOfCols then
                                    raise (invalidArg "data" $"The provided data for row {row} sets positive value in a column with index exceeding the width of the matrix")
                                else
                                    let resultSet, _ = Array.fold (fun (res,idx) value -> match value with | 0 -> (res,idx+1) | _ -> (Set.add idx res,idx+1) ) (Set.empty,0) dataForRow 
                                    resultSet
                        )
                        dataAsAoA
                {structure=structure;width=numOfCols;height=numOfRows}
            )
    let createUnsafe numOfRows numOfCols data = 
            {structure=data;width=numOfCols;height=numOfRows}
    let empty numOfRows numOfCols =
            {structure=Array.create numOfRows Set.empty;width=numOfCols;height=numOfRows}