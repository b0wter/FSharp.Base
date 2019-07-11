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

module compare =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp
    
    [<Fact>]
    let ``Given two options which are Some and identical, returns true.`` () =
        let a = Some 1
        let b = Some 1
        (a |> Option.compare b) |> should be True
    
    [<Fact>]
    let ``Given two options which are Some and not identical, returns false.`` () =
        let a = Some 1
        let b = Some 2
        (a |> Option.compare b) |> should be False
    
    [<Fact>]
    let ``Given a Some and a None, returns false.`` () =
        let a = Some 1
        let b = None
        (a |> Option.compare b) |> should be False
    
    [<Fact>]
    let ``Given a None and a Some, returns false.`` () =
        let a = None
        let b = Some 1
        (a |> Option.compare b) |> should be False
    
    [<Fact>]
    let ``Given two None values, returns false.`` () =
        let a = None
        let b = None
        (a |> Option.compare b) |> should be False
    
module compareIfSome =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp
    
    [<Fact>]
    let ``Given a Some and a matching value, returns true.`` () =
        (Some "a") |> Option.compareIfSome "a" |> should be True
        
    [<Fact>]
    let ``Given a Some and a non-matching value, returns false.`` () =
        (Some "a") |> Option.compareIfSome "b" |> should be False
        
    [<Fact>]
    let ``Given a None and a value, returns false.`` () =
        None |> Option.compareIfSome "a" |> should be False
        
