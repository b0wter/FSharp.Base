namespace BaseTests.Result

module mapBoth =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp
    
    [<Fact>]
    let ``Given an Ok-Result, maps the Ok result.`` () =
        let input = Ok "input"
        let ifOk = fun _ -> "output"
        let ifErr = fun _ -> "error"
        match input |> Result.mapBoth ifOk ifErr with
        | Ok "output" -> true
        | _ -> false
        |> should be True
        
    [<Fact>]
    let ``Given an Error-Result, maps the Error result.`` () =
        let input = Error "input"
        let ifOk = fun _ -> "output"
        let ifErr = fun _ -> "error"
        match input |> Result.mapBoth ifOk ifErr with
        | Error "error" -> true
        | _ -> false
        |> should be True
        
    
module bindBoth =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp
    
    [<Fact>]
    let ``Given an Ok-Result, binds the Ok result.`` () =
        let input = Ok "input"
        let ifOk = fun _ -> Ok "output"
        let ifErr = fun _ -> Error "error"
        match input |> Result.bindBoth ifOk ifErr with
        | Ok "output" -> true
        | _ -> false
        |> should be True
        
    [<Fact>]
    let ``Given an Error-Result, binds the Error result.`` () =
        let input = Error "input"
        let ifOk = fun _ -> Ok "output"
        let ifErr = fun _ -> Error "error"
        match input |> Result.bindBoth ifOk ifErr with
        | Error "error" -> true
        | _ -> false
        |> should be True
            