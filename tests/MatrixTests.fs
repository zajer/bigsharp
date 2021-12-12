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
    [<TestMethod>]
    member this.EqualityTest6 () = 
        let width = 3
        let height = 5
        let data = [|
            Set.ofList[0;2];
            Set.ofList[1];
            Set.ofList[0;1;2];
            Set.ofList[2];
            Set.ofList[0]
        |]
        let mx1 = Matrix.createUnsafe width height data
        Assert.IsTrue(mx1.isEqualTo mx1)
    [<TestMethod>]
    member this.CreateTest1 () =
        let rawData = [|
            [|1;1;1;1|];
            [|0;0;1;1|];
            [|1;1;0;0|];
            [||]
        |]
        let width = 4
        let height = 4
        let refData = [|
            Set.ofList [0;1;2;3];
            Set.ofList [2;3];
            Set.ofList [0;1]
            Set.empty
        |]
        let mx = Matrix.create height width rawData
        let refMx = Matrix.createUnsafe height width refData
        Assert.AreEqual(refMx,mx)
    [<TestMethod>]
    member this.CreateTest2 () =
        let rawData = [|
            [|1;1;1;1|];
            [|0;0;1;1|];
            [|1;1|];
            [||]
        |]
        let width = 4
        let height = 4
        let refData = [|
            Set.ofList [0;1;2;3];
            Set.ofList [2;3];
            Set.ofList [0;1]
            Set.empty
        |]
        let mx = Matrix.create height width rawData
        let refMx = Matrix.createUnsafe height width refData
        Assert.AreEqual(refMx,mx)
    [<TestMethod>]
    member this.MatrixExtendingTest1 () =
        let mx1Data = [|
            [|0;0;1|];
            [|0;1|];
            [|1|];
        |]
        let width1 = 3
        let height1 = 3
        let mx2Data = [|
            [|0;1|];
            [|1|]
        |]
        let width2 = 2
        let height2 = 2
        let mx1 = Matrix.create height1 width1 mx1Data
        let mx2 = Matrix.create height2 width2 mx2Data
        let mxResData = [|
            [|0;0;1|];
            [|0;1|];
            [|1|];
            [|0;0;0;0;1|];
            [|0;0;0;1;|]
        |]
        let resWidth = 5
        let resHeight = 5
        let expectedMx = Matrix.create resHeight resWidth mxResData
        let resultMx = mx1.extend mx2
        Assert.AreEqual(expectedMx,resultMx)
    [<TestMethod>]
    member this.MatrixExtendingTest2 () =
        let mx1Data = [|
            [|0;0;1|];
            [|0;1|]
        |]
        let width1 = 3
        let height1 = 2
        let mx2Data = [|
            [|0;0;0;1|];
            [|0;0;1|];
            [|0;1|];
            [|1|];
            [|1|]
        |]
        let width2 = 4
        let height2 = 5
        let mx1 = Matrix.create height1 width1 mx1Data
        let mx2 = Matrix.create height2 width2 mx2Data
        let mxResData = [|
            [|0;0;1|];
            [|0;1|];
            [|0;0;0;0;0;0;1|];
            [|0;0;0;0;0;1|];
            [|0;0;0;0;1|];
            [|0;0;0;1|];
            [|0;0;0;1|]
        |]
        let resWidth = 7
        let resHeight = 7
        let expectedMx = Matrix.create resHeight resWidth mxResData
        let resultMx = mx1.extend mx2
        Assert.AreEqual(expectedMx,resultMx)
    [<TestMethod>]
    member this.MatrixAsStringTest () =
        let mxData = 
            [|
                [|0;0;1|];
                [|0;1|];
                [|1|];
                [|0;0;0;1|];
                [|0;0;0;0;1|]
            |]
        let mxWidth = 5
        let mxHeight = 5
        let mx = Matrix.create mxHeight mxWidth mxData
        printfn "actual:\n%s" (mx.ToString ())
        let expectedString ="0 0 1 0 0 \n0 1 0 0 0 \n1 0 0 0 0 \n0 0 0 1 0 \n0 0 0 0 1 "
        printfn "expected:\n%s" expectedString
        Assert.AreEqual(expectedString,mx.ToString())
    [<TestMethod>]
    member this.MatrixShrinkingTest1 () =
        let mxData = 
            [|
                [|0;0;1|];
                [|0;1|];
                [|1|];
                [|0;0;0;1|];
                [|0;0;0;0;1|]
            |]
        let mxWidth = 5
        let mxHeight = 5
        let rowsToDelete = Set.ofArray [|1;3|]
        let colsToDelete = Set.ofArray [|0|]
        let expectResultMxData =
            [|
                [|0;1|];
                [||];
                [|0;0;0;1|]
            |]
        let expectedResultMxWidth = 4
        let expectedResultMxHeight = 3
        let inputMx = Matrix.create mxHeight mxWidth mxData
        let expectedResultMx = Matrix.create expectedResultMxHeight expectedResultMxWidth expectResultMxData
        let actualResultMx = inputMx.shrink rowsToDelete colsToDelete
        Assert.AreEqual(expectedResultMx,actualResultMx)
    [<TestMethod>]
    member this.MatrixShrinkingTest2 () =
        let mxData = 
            [|
                [|0;0;1|];
                [|0;1|];
                [|1|];
                [|0;0;0;1|];
                [|0;0;0;0;1|]
            |]
        let mxWidth = 5
        let mxHeight = 5
        let rowsToDelete = Set.ofArray [|1;2|]
        let colsToDelete = Set.ofArray [|0;1|]
        let expectResultMxData =
            [|
                [|1|];
                [|0;1|];
                [|0;0;1|]
            |]
        let expectedResultMxWidth = 3
        let expectedResultMxHeight = 3
        let inputMx = Matrix.create mxHeight mxWidth mxData
        let expectedResultMx = Matrix.create expectedResultMxHeight expectedResultMxWidth expectResultMxData
        let actualResultMx = inputMx.shrink rowsToDelete colsToDelete
        Assert.AreEqual(expectedResultMx,actualResultMx)
    [<TestMethod>]
    member this.MatrixElementAccessOperatorTest () =
        let mxData =
            [|
                [|1|];
                [|0;1|];
                [|0;0;1|]
            |]
        let mxWidth = 3
        let mxHeight = 3
        let mx = Matrix.create mxHeight mxWidth mxData
        let expectedResult1 = 1
        Assert.AreEqual(expectedResult1,mx.[0,0])
        Assert.AreEqual(expectedResult1,mx.[1,1])
        Assert.AreEqual(expectedResult1,mx.[2,2])
        let expectedResult2 = 0
        Assert.AreEqual(expectedResult2,mx.[0,2])
        Assert.AreEqual(expectedResult2,mx.[1,0])
        Assert.AreEqual(expectedResult2,mx.[2,1])