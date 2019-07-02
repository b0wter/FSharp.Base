namespace BaseTests.Collections.Array

module Remove =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp.Collections
    
    let items = [| 1; 2; 3; 4; 5; 6 |]
    
    [<Fact>]
    let ``Given an array containing the item, returns the array without the item.`` () =
        let result = items |> Array.remove 3
        result |> should equal [| 1; 2; 4; 5; 6 |]
        
    [<Fact>]
    let ``Given an array not containing the item, returns the same array.`` () =
        let result = items |> Array.remove 8
        result |> should equal items

    [<Fact>]
    let ``Given an empty array, returns an empty array.`` () =
        let result = [||] |> Array.remove 3
        result |> should be Empty