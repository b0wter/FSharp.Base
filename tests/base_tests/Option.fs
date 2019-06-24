namespace BaseTests.Option

module getOrElse =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    
    [<Fact>]
    let ``Given an option containing None, returns the alternative value.`` () =
        let item = (None: int option)
        let result = item |> b0wter.FSharp.Option.getOrElse 1
        result |> should equal 1
        
    [<Fact>]
    let ``Given an option containing Some, returns the value of the option not the alternative.`` () =
        let item = Some 1
        let result = item |> b0wter.FSharp.Option.getOrElse 99
        result |> should equal 1
