namespace b0wter.FSharp

module AsyncResult =

    /// Works like `Result.bind` except that the result and the function are async.
    let bindA asyncOp asyncResult =
        async {
            let! b = asyncResult
            match b with
            | Ok o -> return! asyncOp o
            | Error e -> return Error e
        }
        
    /// Works like `Result.bind` except that the initial result is async (`Async<Result<_,_>>`)
    /// while the function is not.
    let bind syncOp asyncResult =
        async {
            let! b = asyncResult
            match b with
            | Ok o -> return syncOp o
            | Error e -> return Error e
        }

    /// Works like `Result.map` except that the result and the function are async.
    let mapA (asyncOp: 'a -> Async<'b>) (asyncResult: Async<Result<'a, 'c>>) =
        async {
            let! b = asyncResult
            match b with
            | Ok o -> let! a = asyncOp o
                      return Ok a
            | Error e -> return Error e
        }
        
    /// Works like `Result.map` except that the initial result is async (`Async<Result<_,_>>`)
    /// while the function is not.
    let map syncOp asyncResult =
        async {
            let! b = asyncResult
            match b with
            | Ok o -> return (Ok <| syncOp o)
            | Error e -> return Error e
        }
