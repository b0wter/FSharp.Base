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
        
    /// Works like `Result.bindError` except that the result and the function are async.
    let bindErrorA (asyncOp: 'b -> Async<Result<'a, 'c>>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'a, 'c>> =
        async {
            let! b = asyncResult
            match b with
            | Ok o -> return Ok o
            | Error e -> return! e |> asyncOp
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
        
    /// Works like `Result.bindError` except that the initial result is async (`Async<Result<_,_>>`)
    /// while the function is not.
    let bindError (syncOp: 'b -> Result<'a, 'c>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'a, 'c>> =
        async {
            let! b = asyncResult
            match b with
            | Ok o -> return Ok o
            | Error e -> return e |> syncOp
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
        
    /// Works like `Result.mapError` except that the result and the function are async.
    let mapErrorA (asyncOp: 'b -> Async<'c>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'a, 'c>> =
        async {
            let! b = asyncResult
            match b with
            | Ok o -> return Ok o
            | Error e -> let! c = (e |> asyncOp)
                         return Error c
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
        
    /// Works like `Result.mapError` except that the initial result is async (`Async<Result<_,_>>`)
    /// while the function is not.
    let mapError (syncOp: 'b -> 'c) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'a, 'c>> =
        async {
            let! b = asyncResult
            match b with
            | Ok o -> return Ok o
            | Error e -> return (Error <| syncOp e)
        }
