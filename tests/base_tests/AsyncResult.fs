namespace BaseTests.AsyncResult

module bindA =
    
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp
    open FsUnit.CustomMatchers
    
    [<Fact>]
    let ``Given two async operations returns the chained result`` () =
        async {
        let async1 = fun () -> async { return Ok 1 }
        let async2 = fun (i: int) -> async { return (Ok <| i * 2) }
        
        let! result = async1 () |> AsyncResult.bindA async2
        do result |> should be (ofCase <@ Result<int,_>.Ok @>)
        
        match result with
        | Ok i -> i |> (should equal 2)
        | _ -> failwith "This needs to be caught earlier!"
        }
        
module bind =
    
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp
    open FsUnit.CustomMatchers
    
    [<Fact>]
    let ``Given two async operations returns the chained result`` () =
        async {
        let async = fun () -> async { return Ok 1 }
        let sync = fun (i: int) -> (Ok <| i * 2)
        
        let! result = async () |> AsyncResult.bind sync
        do result |> should be (ofCase <@ Result<int,_>.Ok @>)
        
        match result with
        | Ok i -> i |> (should equal 2)
        | _ -> failwith "This needs to be caught earlier!"
        }
    
module mapA =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp
    open FsUnit.CustomMatchers
    
    [<Fact>]
    let ``Given an async operation and a sync operation returns the result`` () =
        async {
        let asyncResult = (fun () -> async { return Ok 1 })
        let asyncOp = (fun (i: int) -> async { return i * 2})
        
        let! result = asyncResult () |> AsyncResult.mapA asyncOp
        
        do result |> should be (ofCase <@ Result<int, _>.Ok @>)
        
        }

module map =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp
    open FsUnit.CustomMatchers
    
    [<Fact>]
    let ``Given an async operation and a sync operation returns the result`` () =
        async {
        let asyncResult = (fun () -> async { return Ok 1 })
        let syncOp = (fun (i: int) -> i * 2)
        
        let! result = asyncResult () |> AsyncResult.map syncOp
        
        do result |> should be (ofCase <@ Result<int, _>.Ok @>)
        
        }

