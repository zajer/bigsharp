namespace Bigsharp.Core

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type MatrixTests () =  
    [<TestMethod>]
    member this.EqualityTest1 () = 
        let width1 = 4
        let height1 = 4
        let data1 = [|
            Set.ofList[0;3];
            Set.ofList[1];
            Set.ofList[0;1;2;3]
        |]
        let mx1 = Matrix.createUnsafe width1 height1 data1
        Assert.IsTrue(mx1.isEqualTo mx1)
    [<TestMethod>]
    member this.EqualityTest2 () = 
        let width1 = 4
        let height1 = 4
        let data1 = [|
            Set.ofList[0;3];
            Set.ofList[1];
            Set.ofList[0;1;2;3]
        |]
        let data2 = [|
            Set.ofList[0;2];
            Set.ofList[1];
            Set.ofList[0;1;2;3]
        |]
        let width2 = 4
        let height2 = 4
        let mx1 = Matrix.createUnsafe width1 height1 data1
        let mx2 = Matrix.createUnsafe width2 height2 data2
        Assert.IsFalse(mx1.isEqualTo mx2)
    [<TestMethod>]
    member this.EqualityTest3 () = 
        let width1 = 4
        let height1 = 4
        let data1 = [|
            Set.ofList[0;3];
            Set.ofList[1];
            Set.ofList[0;1;2;3]
        |]
        let data2 = [|
            Set.ofList[0;3];
            Set.ofList[1;2];
            Set.ofList[0;1;2;3]
        |]
        let width2 = 4
        let height2 = 4
        let mx1 = Matrix.createUnsafe width1 height1 data1
        let mx2 = Matrix.createUnsafe width2 height2 data2
        Assert.IsFalse(mx1.isEqualTo mx2)
    [<TestMethod>]
    member this.EqualityTest4 () = 
        let width1 = 4
        let height1 = 4
        let data1 = [|
            Set.ofList[0;3];
            Set.ofList[1];
            Set.ofList[0;1;2;3]
        |]
        let data2 = [|
            Set.ofList[0;3];
            Set.ofList[1];
            Set.ofList[0;2;3]
        |]
        let width2 = 4
        let height2 = 4
        let mx1 = Matrix.createUnsafe width1 height1 data1
        let mx2 = Matrix.createUnsafe width2 height2 data2
        Assert.IsFalse(mx1.isEqualTo mx2)
    [<TestMethod>]
    member this.EqualityTest5 () = 
        let width1 = 4
        let height1 = 4
        let data1 = [|
            Set.ofList[0;3];
            Set.ofList[1];
            Set.ofList[0;1;2;3]
        |]
        let data2 = [|
            Set.ofList[0;3];
            Set.ofList[1];
            Set.ofList[0;1;2;3]
        |]
        let width2 = 3
        let height2 = 3
        let mx1 = Matrix.createUnsafe width1 height1 data1
        let mx2 = Matrix.createUnsafe width2 height2 data2
        Assert.IsFalse(mx1.isEqualTo mx2)
        
    member this.CreateTest1 () =
        let rawData = [|
            [|1;1;1;1|];
            [|0;0;1;1|];
            [|1;1;0;0|];
            [||]
        |]
        let width = 4
        let height = 4
        let referenceValue = 5
        let setOfRemovedElems = Set.ofList [1;3;4]
        let expectedShift = 3
        let actualShift = Utils.numberOfSmallerElements referenceValue setOfRemovedElems
        Assert.AreEqual(expectedShift,actualShift)