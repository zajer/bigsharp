namespace Bigsharp.Core

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UtilsTests () =

    [<TestMethod>]
    [<DataRow([|1;3;4|],5,3)>]
    [<DataRow([|1;3;4|],2,1)>]
    [<DataRow([|1;3;4|],0,0)>]
    member this.NumberOfSmallerElementsTest1 (arrayOfRemovedElems:int array,referenceValue:int, expectedShift: int ) =
        let setOfRemovedElems = Set.ofArray arrayOfRemovedElems
        let actualShift = Utils.numberOfSmallerElements referenceValue setOfRemovedElems
        Assert.AreEqual(expectedShift,actualShift)
    [<TestMethod>]
    [<DataRow([|1;4;5;7|],1,0,0)>]
    [<DataRow([|1;4;5;7|],1,1,2)>]
    [<DataRow([|1;4;5;7|],1,2,2)>]
    [<DataRow([|1;4;5;7|],1,3,3)>]
    [<DataRow([|1;4;5;7|],1,4,6)>]
    [<DataRow([|1;4;5;7|],1,5,6)>]
    [<DataRow([|1;4;5;7|],1,6,6)>]
    [<DataRow([|1;4;5;7|],1,7,8)>]
    member this.NthAvailableNumberTest (arrayOfRemovedElems:int array,n:int,start:int,expectedNumber:int) =
        let setOfRemovedElems = Set.ofArray arrayOfRemovedElems
        let actualNumber = Utils.nthAvailableNumber n start setOfRemovedElems
        Assert.AreEqual(expectedNumber,actualNumber)
    [<TestMethod>]
    [<DataRow([|1;4;5;7|],0,0)>]
    [<DataRow([|1;4;5;7|],1,2)>]
    [<DataRow([|1;4;5;7|],2,3)>]
    [<DataRow([|1;4;5;7|],4,8)>]
    [<DataRow([|1;4;5;7|],5,9)>]
    [<DataRow([|1;4;5;7|],6,10)>]
    [<DataRow([|1;4;5;7|],7,11)>]
    [<DataRow([|1;4;5;7|],8,12)>]
    [<DataRow([|1;4;5;6;7;8|],0,0)>]
    [<DataRow([|1;4;5;6;7;8|],1,2)>]
    [<DataRow([|1;4;5;6;7;8|],2,3)>]
    [<DataRow([|1;4;5;6;7;8|],3,9)>]
    [<DataRow([|1;4;5;6;7;8|],4,10)>]
    [<DataRow([|1;4;5;6;7;8|],5,11)>]
    [<DataRow([|1;4;5;6;7;8|],6,12)>]
    [<DataRow([|1;4;5;6;7;8|],7,13)>]
    [<DataRow([|1;4;5;6;7;8|],8,14)>]
    member this.NthNaturalNumberOutsideProvidedSetTest (arrayOfRemovedElems:int array, n:int,expectedNumber:int) =
        let setOfRemovedElems = Set.ofArray arrayOfRemovedElems
        let actualNumber = Utils.nthNaturalNumberOutsideProvidedSet n setOfRemovedElems
        Assert.AreEqual(expectedNumber,actualNumber)
    