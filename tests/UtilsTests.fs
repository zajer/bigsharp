namespace Bigsharp.Core

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UtilsTests () =

    [<TestMethod>]
    member this.NumberOfSmallerElementsTest1 () =
        let referenceValue = 5
        let setOfRemovedElems = Set.ofList [1;3;4]
        let expectedShift = 3
        let actualShift = Utils.numberOfSmallerElements referenceValue setOfRemovedElems
        Assert.AreEqual(expectedShift,actualShift)
    [<TestMethod>]
    member this.NumberOfSmallerElementsTest2 () =
        let referenceValue = 2
        let setOfRemovedElems = Set.ofList [1;3;4]
        let expectedShift = 1
        let actualShift = Utils.numberOfSmallerElements referenceValue setOfRemovedElems
        Assert.AreEqual(expectedShift,actualShift)
    [<TestMethod>]
    member this.NumberOfSmallerElementsTest3 () =
            let referenceValue = 0
            let setOfRemovedElems = Set.ofList [1;3;4]
            let expectedShift = 0
            let actualShift = Utils.numberOfSmallerElements referenceValue setOfRemovedElems
            Assert.AreEqual(expectedShift,actualShift)